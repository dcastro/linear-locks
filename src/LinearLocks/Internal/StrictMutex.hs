{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal.StrictMutex where

import Control.Concurrent (MVar)
import Control.Concurrent qualified as MVar
import Control.DeepSeq (NFData, force)
import Control.Functor.Linear qualified as L
import GHC.TypeLits (Nat)
import LinearLocks.Internal
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Linear qualified as L
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
import System.IO.Resource.Linear.Internal qualified as Internal

-- | A strict version of "LinearLocks.Mutex".
data Mutex (lvl :: Nat) a = Mutex
  { -- NOTE: we're using `MVar (NF a)` instead of e.g. `Control.Concurrent.MVar.Strict.MVar` (from the `strict-concurrency` package)
    -- because we don't want to require `NFData` when taking the mvar's (already evaluated) value and putting it right back in, unmodified.
    --
    -- In other words, this allows `lock` to not require `NFData` to setup the "release on exception" action.
    var :: MVar (NF a),
    -- | The unique ID for this mutex. It's used to ensure t'LinearLocks.MutexSet's don't contain duplicate mutexes, see 'LinearLocks.newMutexSet'.
    id :: LockId
  }

-- | A t`MutexGuard` represents the ownership of a locked mutex.
--
-- It can be used to read/write the mutex while the lock is held.
--
-- It must be released with `release`, after which the guard will be consumed and can no longer be used.
data MutexGuard a = MutexGuard
  { resource :: RIO.Resource (MutexResource a),
    -- | The latest value set by the user.
    -- This will be comitted to the MVar when the guard is released.
    newValue :: Ur a
  }

data MutexResource a = MutexResource
  { -- | The value that was read from the `MVar` when it was acquired.
    --
    -- If an exception occurs before the mutex guard is manually released, this value will be put back into the `MVar`.
    initialValue :: (NF a),
    var :: MVar (NF a)
  }

instance (NFData a) => Acquirable (Mutex lvl a) where
  type Guard (Mutex lvl a) = MutexGuard a
  type Level (Mutex lvl a) = lvl

  getId m = m.id

  unsafeAcquire :: forall lvl a. Mutex lvl a -> RIO (MutexGuard a)
  unsafeAcquire m = L.do
    -- Note: we have to match on `UnsafeResource` so we can extract the `guard.initialValue`
    Internal.UnsafeResource key guard <- RIO.unsafeAcquire acq rel
    L.pure
      MutexGuard
        { resource = Internal.UnsafeResource key guard,
          newValue = Ur guard.initialValue.unNF
        }
    where
      acq :: L.IO (Ur (MutexResource a))
      acq = L.do
        Ur a <- L.fromSystemIOU L.$ MVar.takeMVar m.var
        L.pure (Ur (MutexResource {initialValue = a, var = m.var}))

      -- The action to run if an exception is thrown before the guard is manually released with `release`.
      rel :: MutexResource a -> L.IO ()
      rel (MutexResource initialValue var) =
        L.void L.$ L.fromSystemIO L.$ MVar.putMVar var initialValue

instance (NFData a) => Releasable (MutexGuard a) where
  doRelease = release

read :: MutexGuard a %1 -> RIO (Ur a, MutexGuard a)
read (MutexGuard resource (Ur newValue)) =
  L.pure (Ur newValue, MutexGuard {resource, newValue = Ur newValue})

-- | Writes a new value to the mutex, which will be committed when the guard is released.
--
-- If an exception is thrown after `write` but before `release`,
-- the mutex will be rolled back to its original state.
--
-- Note: The value will only be evaluated to Normal Form when the mutex is released, not when it's written.
write :: MutexGuard a %1 -> a -> RIO (MutexGuard a)
write (MutexGuard resource (Ur _)) newValue =
  L.pure (MutexGuard {resource, newValue = Ur newValue})

-- | Releases the mutex and commits the latest value set by `write`.
--
-- Fully evaluates the value to Normal Form before releasing the mutex.
release :: (NFData a) => MutexGuard a %1 -> RIO ()
release (MutexGuard ((Internal.UnsafeResource key mr)) (Ur (mkNF -> !newValue))) = L.do
  -- Note: the resource was initially registered with a release action that puts the original value back into the MVar.
  -- That release action should be run if an exception is thrown before `release` is called,
  -- which ensures the MVar will "rollback" to its original state.
  --
  -- However, if `release` is called explicitly by the user,
  -- we want to update the release action to put `newValue` back into the MVar instead.
  -- Therefore, we must call `release'` with a _new release action_ that puts `newValue` into the MVar.
  release' (Internal.UnsafeResource key mr) L.do
    L.void L.$ L.fromSystemIO L.$ MVar.putMVar mr.var newValue

-- | Creates a new mutex with the given initial value.
--
-- The @lvl@ parameter determines the order in which this mutex can be acquired relative to other mutexes.
--
-- It does not have to be unique, multiple mutexes can have the same level.
-- Mutexes with the same level can be added to a t`LinearLocks.MutexSet` and acquired with 'LinearLocks.acquireMany'.
--
-- This function fully evaluates the initial value to Normal Form.
new :: forall a. (NFData a) => forall (lvl :: Nat) -> a -> IO (Mutex lvl a)
new _lvl (mkNF -> !a) = do
  var <- MVar.newMVar a
  id <- nextLockId
  pure Mutex {var, id}

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | A wrapper type to force the contents to be fully evaluated before being put back into the MVar.
--
-- NOTE: `NF` will only turn "shallow evaluation" into "deep evaluation".
-- You must still use a bang pattern on `NF` to force it.
newtype NF a = UnsafeNF {unNF :: a}
  deriving newtype (Show, Eq)

mkNF :: (NFData a) => a -> NF a
mkNF = UnsafeNF . force
