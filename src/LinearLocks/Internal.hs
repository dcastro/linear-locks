{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal where

import Control.Concurrent (MVar, ThreadId, myThreadId)
import Control.Concurrent qualified as MVar
import Control.Exception (Exception (..), bracket_, throw)
import Control.Functor.Linear qualified as L
import Data.Atomics.Counter (AtomicCounter)
import Data.Atomics.Counter qualified as Atomic
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
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
--  * Do not implement `Consumable` / `Dupable` / `Movable`
data MutexKey (lvl :: Nat) = UnsafeMutexKey

-- | A unique identifier for a mutex.
newtype MutexId = MutexId Int
  deriving newtype (Eq, Ord)

newtype instance VU.MVector s MutexId = MV_MutexId (VP.MVector s Int)

newtype instance VU.Vector MutexId = V_MutexId (VP.Vector Int)

deriving via (VU.UnboxViaPrim Int) instance VGM.MVector VU.MVector MutexId

deriving via (VU.UnboxViaPrim Int) instance VG.Vector VU.Vector MutexId

instance VU.Unbox MutexId

data Mutex (lvl :: Nat) a = Mutex
  { var :: MVar a,
    -- | The unique ID for this mutex. It's used to ensure `MutexSet`s don't contain duplicate mutexes, see 'mkMutexSet'.
    id :: MutexId
  }

data MutexGuard a = MutexGuard
  { resource :: RIO.Resource (MutexResource a),
    -- | The latest value set by the user.
    -- This will be comitted to the MVar when the guard is released.
    newValue :: Ur a
  }

data MutexResource a = MutexResource
  { -- | The value to put back into the MVar when the mutex guard is released.
    --
    -- This starts out as the same value that was in the MVar when the mutex was acquired.
    -- This ensures that, if an exception is thrown, the same value will be put back in and the MVar won't be modified.
    --
    -- If no exceptions occur, `releaseGuard` will set `commitValue` to `MutexGuard.newValue` before releasing the guard.
    commitValue :: a,
    var :: MVar a
  }

-- | Acquire a mutex.
-- Consumes the key and return a new key (with an increased level).
lock ::
  forall a keyLvl mutexLvl.
  (keyLvl <= mutexLvl) =>
  MutexKey keyLvl %1 ->
  Mutex mutexLvl a ->
  RIO (MutexGuard a, MutexKey (mutexLvl + 1))
lock UnsafeMutexKey m = L.do
  guard <- unsafeLock m
  L.pure (guard, UnsafeMutexKey)

-- | This is marked as unsafe because it does not consume a `MutexKey`.
unsafeLock :: forall lvl a. Mutex lvl a -> RIO (MutexGuard a)
unsafeLock m = L.do
  Internal.UnsafeResource key guard <- RIO.unsafeAcquire acq rel
  L.pure
    MutexGuard
      { resource = Internal.UnsafeResource key guard,
        newValue = Ur guard.commitValue
      }
  where
    acq :: L.IO (Ur (MutexResource a))
    acq = L.do
      Ur a <- L.fromSystemIOU L.$ MVar.takeMVar m.var
      L.pure (Ur (MutexResource {commitValue = a, var = m.var}))

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
releaseGuard (MutexGuard ((Internal.UnsafeResource key mr)) (Ur newValue)) = L.do
  -- Note: the resource was initially registered with a release action that puts the original value back into the MVar.
  -- That release action should be run if an exception is thrown before `releaseGuard` is called,
  -- which ensures the MVar will "rollback" to its original state.
  --
  -- However, if `releaseGuard` is called explicitly by the user,
  -- we want to update the release action to put `newValue` back into the MVar instead.
  -- Therefore, we must call `release'` with a _new release action_ that puts `newValue` into the MVar.
  release' (Internal.UnsafeResource key mr) L.do
    L.void L.$ L.fromSystemIO L.$ MVar.putMVar mr.var newValue

mkMutex :: forall a. forall lvl -> a -> IO (Mutex lvl a)
mkMutex _lvl a = do
  var <- MVar.newMVar a
  newId <- Atomic.incrCounter 1 mutexIdCounter
  pure
    Mutex
      { var = var,
        id = MutexId newId
      }

-- | Creates a new lock scope with a key of level 0, and runs the given function with it.
--  The key can be used to lock mutexes with `lock`.
-- The final key must be returned.
--
-- Will throw a `NestedLocksScopeException` if a nested `lockScope` is created at runtime.
lockScope ::
  forall a lvl.
  -- Note: The key is linearly typed and must be returned; this prevents it from escaping the scope of the `lockScope` function.
  --
  -- The use of `Ur` also prevents any linear values from escaping the scope via the variable `a`.
  -- See: https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/#sticky-ends-of-scopes
  (MutexKey 0 %1 -> RIO (Ur a, MutexKey lvl)) ->
  IO a
lockScope run = do
  ensureNotNested do
    RIO.run L.do
      let key = UnsafeMutexKey @0
      (a, UnsafeMutexKey) <- run key
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

----------------------------------------------------------------------------
-- Global variables
----------------------------------------------------------------------------

-- | A set of the ThreadIds currently holding a lock scope.
-- We use this to prevent nested lock scopes at runtime.
{-# NOINLINE lockScopes #-}
lockScopes :: StmSet.Set ThreadId
lockScopes =
  -- See: https://wiki.haskell.org/index.php?oldid=64612
  unsafePerformIO StmSet.newIO

-- | An atomic counter used to generate unique IDs for mutexes.
{-# NOINLINE mutexIdCounter #-}
mutexIdCounter :: AtomicCounter
mutexIdCounter =
  unsafePerformIO $ Atomic.newCounter 0

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Similar to 'RIO.release', except it uses a different release action than the one registered by 'unsafeAcquire'.
release' :: RIO.Resource a %1 -> L.IO () -> RIO ()
release' (Internal.UnsafeResource key _) release = Internal.RIO (\st -> L.mask_ (releaseWith key st))
  where
    releaseWith key rrm = L.do
      Ur (Internal.ReleaseMap releaseMap) <- L.readIORef rrm
      () <- release
      L.writeIORef rrm (Internal.ReleaseMap (IntMap.delete key releaseMap))
