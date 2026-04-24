{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LinearLocks.Internal where

import Control.Concurrent (MVar, ThreadId, myThreadId)
import Control.Concurrent qualified as MVar
import Control.Exception (Exception (..), bracket_, throw)
import Control.Functor.Linear qualified as L
import Data.Kind (Type)
import Focus qualified
import GHC.Conc (atomically)
import GHC.IO (unsafePerformIO)
import GHC.TypeLits (Nat, type (+), type (<=))
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import StmContainers.Set qualified as StmSet
import System.IO.Linear qualified as L
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
import System.IO.Resource.Linear.Internal qualified as Internal

-- Notes:
--  * Do not export the constructor
--  * Do not implement `Consumable` / `Movable`
data MutexKey (lvl :: Nat) (scope :: Type) = MutexKey

newtype Mutex (lvl :: Nat) a = Mutex {getVar :: MVar a}

data MutexGuard a = MutexGuard
  { resource :: RIO.Resource (MutexResource a),
    -- The latest value set by the user.
    -- This will be comitted to the MVar when the guard is released.
    newValue :: Ur a
  }

data MutexResource a = MutexResource
  { -- The value to put back into the MVar when the mutex guard is released.
    --
    -- This starts out as the same value that was in the MVar when the mutex was acquired.
    -- This ensures that, if an exception is thrown, the same value will be put back in and the MVar won't be modified.
    --
    -- If no exceptions occur, `releaseGuard` will set `commitValue` to `MutexGuard.newValue` before releasing the guard.
    commitValue :: a,
    var :: MVar a
  }

-- | Consume the key and return a new key (with an increased level) in linear IO
lock ::
  forall a keyLvl mutexLvl scope.
  (keyLvl <= mutexLvl) =>
  MutexKey keyLvl scope %1 ->
  Mutex mutexLvl a ->
  RIO (MutexGuard a, MutexKey (mutexLvl + 1) scope)
lock MutexKey m = L.do
  resource <- RIO.unsafeAcquire acq rel
  -- NOTE: unsafeAcquire does not let us return additional data (e.g. `commitValue`), so
  -- we have to retrieve the `commitValue` from the resource after acquiring it.
  (Ur commitValue, resource) <- RIO.unsafeFromSystemIOResource (\mr -> pure mr.commitValue) resource

  L.pure (MutexGuard {resource, newValue = Ur commitValue}, MutexKey)
  where
    acq :: L.IO (Ur (MutexResource a))
    acq = L.do
      Ur a <- L.fromSystemIOU L.$ MVar.takeMVar m.getVar
      L.pure (Ur (MutexResource {commitValue = a, var = m.getVar}))

    rel :: MutexResource a -> L.IO ()
    rel (MutexResource (commitValue) var) =
      L.void L.$ L.fromSystemIO L.$ MVar.putMVar var commitValue

readGuard :: MutexGuard a %1 -> RIO (Ur a, MutexGuard a)
readGuard (MutexGuard resource (Ur newValue)) =
  L.pure (Ur newValue, MutexGuard {resource, newValue = Ur newValue})

writeGuard :: MutexGuard a %1 -> a -> RIO (MutexGuard a)
writeGuard (MutexGuard resource (Ur _)) newValue =
  L.pure (MutexGuard {resource, newValue = Ur newValue})

releaseGuard :: MutexGuard a %1 -> RIO ()
releaseGuard (MutexGuard (Internal.UnsafeResource key mr) (Ur newValue)) =
  RIO.release (Internal.UnsafeResource key (mr {commitValue = newValue}))

mkMutex :: forall lvl a. a -> IO (Mutex lvl a)
mkMutex a = do
  var <- MVar.newMVar a
  pure (Mutex var)

-- | Creates a new lock scope with a key of level 0, and runs the given function with it.
--  The key can be used to lock mutexes with `lock`.
-- The final key must be returned.
--
-- The key is indexed by a rank-2 type variable `scope` to prevent it from
-- being used outside of the scope of `lockScope`.
--
-- WARNING: Will throw a `NestedLocksScopeException` if a nested `lockScope` is created at runtime.
lockScope ::
  forall a lvl.
  ( forall (scope :: Type).
    MutexKey 0 scope %1 ->
    RIO (Ur a, MutexKey lvl scope)
  ) ->
  IO a
lockScope run = do
  ensureNotNested do
    RIO.run L.do
      let key = MutexKey @0
      (a, MutexKey) <- run key
      L.pure a
  where
    -- Ensures nested lock scopes are not created.
    -- We can't really detect this at compile-time, so we'll make do with a runtime check.
    ensureNotNested :: IO a -> IO a
    ensureNotNested action = do
      tid <- myThreadId
      bracket_
        -- Acquire: register the thread ID in the set of active lock scopes.
        ( do
            success <- atomically do
              StmSet.focus
                ( do
                    -- Check if the thread ID is already in the set.
                    Focus.lookup >>= \case
                      Just () ->
                        -- The thread ID was found in the set, which means we're trying to create a nested lock scope.
                        -- We return `False` to signal an error.
                        pure False
                      Nothing -> do
                        Focus.insert ()
                        pure True
                )
                tid
                lockScopes
            if success
              then pure ()
              else throw NestedLocksScopeException
        )
        -- Release: remove the thread ID from the set of active lock scopes.
        ( atomically do
            StmSet.delete tid lockScopes
        )
        action

data NestedLocksScopeException = NestedLocksScopeException
  deriving stock (Show)

instance Exception NestedLocksScopeException where
  displayException NestedLocksScopeException = "Nested lock scopes are not allowed"

-- | A set of the ThreadIds currently holding a lock scope.
-- We use this to prevent nested lock scopes at runtime.
{-# NOINLINE lockScopes #-}
lockScopes :: StmSet.Set ThreadId
lockScopes =
  -- See: https://wiki.haskell.org/index.php?oldid=64612
  unsafePerformIO StmSet.newIO
