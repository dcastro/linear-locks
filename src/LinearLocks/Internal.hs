{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

#if !MIN_VERSION_linear_base(0,7,1)
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

module LinearLocks.Internal where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (Exception (..), bracket_, throw)
import Control.Functor.Linear qualified as L
import Control.Monad.IO.Class.Linear qualified as L
import Data.Atomics.Counter (AtomicCounter)
import Data.Atomics.Counter qualified as Atomic
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Focus qualified
import GHC.Base (Type)
import GHC.Conc (atomically)
import GHC.IO (unsafePerformIO)
import GHC.TypeLits (Nat, type (+), type (<=))
import Prelude.Linear (Ur (..))
import StmContainers.Set qualified as StmSet
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
#if !MIN_VERSION_linear_base(0,7,1)
import System.IO.Resource.Linear.Internal qualified as RIOInternal
#endif

-- | A key used to acquire locks.
-- A key of level @n@ can only acquire locks of level @n@ or higher.
--
-- Acquiring a mutex with `lock` or `LinearLocks.lockMany` will consume the key and return a new key with an increased level,
-- ensuring locks are always acquired in a consistent order.
data MutexKey (lvl :: Nat)
  = -- Notes:
    --  * Do not export the constructor
    --  * Do not implement `Consumable` / `Dupable` / `Movable`
    UnsafeMutexKey

-- | A unique identifier for a mutex.
newtype MutexId = MutexId Int
  deriving newtype (Eq, Ord, Show)

newtype instance VU.MVector s MutexId = MV_MutexId (VP.MVector s Int)

newtype instance VU.Vector MutexId = V_MutexId (VP.Vector Int)

deriving via (VU.UnboxViaPrim Int) instance VGM.MVector VU.MVector MutexId

deriving via (VU.UnboxViaPrim Int) instance VG.Vector VU.Vector MutexId

instance VU.Unbox MutexId

-- | Creates a new lock scope with a key of level 0, and runs the given function with it.
--  The key can be used to lock mutexes with `lock` and `LinearLocks.lockMany`.
-- The final key must be returned.
--
-- Will throw a t`NestedLocksScopeException` if a nested `lockScope` is created at runtime.
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

-- | Acquire a mutex.
-- Consumes the key and return a new key (with an increased level).
lock ::
  forall keyLvl lockable.
  (Lockable lockable) =>
  (keyLvl <= Level lockable) =>
  MutexKey keyLvl %1 ->
  lockable ->
  RIO (Guard lockable, MutexKey (Level lockable + 1))
lock UnsafeMutexKey m = L.do
  guard <- unsafeLock m
  L.pure (guard, UnsafeMutexKey)

class (Releasable (Guard lockable)) => Lockable lockable where
  type Guard lockable :: Type
  type Level lockable :: Nat

  getId :: lockable -> MutexId

  -- | This is marked as unsafe because it does not consume a `MutexKey`.
  unsafeLock :: lockable -> RIO (Guard lockable)

class Releasable guard where
  -- Design decision: `doRelease` generalizes over releasing any kind of mutex, but we don't export it.
  -- We only export the monomorphic `release` functions for each mutex type, because they might have
  -- important notes in their haddock docs (e.g. `StrictMutex.release` does deep evaluation and might throw an exception as a result),
  -- so it's important those docs are easily discoverable and not hidden behind a more general `doRelease` function.
  doRelease :: guard %1 -> RIO ()

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

-- Only provide this orphan instance for linear-base <= 0.7.0
-- The next release will come with this instance built-in: https://github.com/tweag/linear-base/pull/505
#if !MIN_VERSION_linear_base(0,7,1)
instance L.MonadIO RIO where
  liftIO action = RIOInternal.RIO (\_ -> action)
#endif
