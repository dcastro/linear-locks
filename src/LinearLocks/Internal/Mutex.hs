{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal.Mutex where

import Control.Concurrent (MVar)
import Control.Concurrent qualified as MVar
import Control.Functor.Linear qualified as L
import Data.Atomics.Counter qualified as Atomic
import Data.IntMap.Strict qualified as IntMap
import GHC.TypeLits (Nat)
import LinearLocks.Internal
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Linear qualified as L
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
import System.IO.Resource.Linear.Internal qualified as Internal

-- | A deadlock-free mutex.
--
-- This implementation is lazy.
-- This means that if you place an expensive unevaluated thunk inside a t`Mutex`,
-- it will be evaluated by the thread that consumes it, not the thread that produced it.
-- To avoid this, use "LinearLocks.Mutex.Strict" instead.
data Mutex (lvl :: Nat) a = Mutex
  { var :: MVar a,
    -- | The unique ID for this mutex. It's used to ensure t'LinearLocks.MutexSet's don't contain duplicate mutexes, see 'LinearLocks.newMutexSet'.
    id :: MutexId
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
    initialValue :: a,
    var :: MVar a
  }

instance Lockable (Mutex lvl a) where
  type Guard (Mutex lvl a) = MutexGuard a
  type Level (Mutex lvl a) = lvl

  getId m = m.id

  unsafeLock :: forall lvl a. Mutex lvl a -> RIO (MutexGuard a)
  unsafeLock m = L.do
    -- Note: we have to match on `UnsafeResource` so we can extract the `guard.initialValue`
    Internal.UnsafeResource key guard <- RIO.unsafeAcquire acq rel
    L.pure
      MutexGuard
        { resource = Internal.UnsafeResource key guard,
          newValue = Ur guard.initialValue
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

instance Releasable (MutexGuard a) where
  doRelease = release

read :: MutexGuard a %1 -> RIO (Ur a, MutexGuard a)
read (MutexGuard resource (Ur newValue)) =
  L.pure (Ur newValue, MutexGuard {resource, newValue = Ur newValue})

-- | Writes a new value to the mutex, which will be committed when the guard is released.
--
-- If an exception is thrown after `write` but before `release`,
-- the mutex will be rolled back to its original state.
write :: MutexGuard a %1 -> a -> RIO (MutexGuard a)
write (MutexGuard resource (Ur _)) newValue =
  L.pure (MutexGuard {resource, newValue = Ur newValue})

-- | Releases a mutex and commits the latest value set by `write`.
release :: MutexGuard a %1 -> RIO ()
release (MutexGuard ((Internal.UnsafeResource key mr)) (Ur newValue)) = L.do
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
-- Mutexes with the same level can be added to a t`LinearLocks.MutexSet` and acquired with 'LinearLocks.lockMany'.
new :: forall a. forall (lvl :: Nat) -> a -> IO (Mutex lvl a)
new _lvl a = do
  var <- MVar.newMVar a
  newId <- Atomic.incrCounter 1 mutexIdCounter
  pure
    Mutex
      { var = var,
        id = MutexId newId
      }

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Similar to 'System.IO.Resource.Linear.release', except it uses a different release action than the one registered by 'System.IO.Resource.Linear.unsafeAcquire'.
release' :: RIO.Resource a %1 -> L.IO () -> RIO ()
release' (Internal.UnsafeResource key _) release = Internal.RIO (\st -> L.mask_ (releaseWith key st))
  where
    releaseWith key rrm = L.do
      Ur (Internal.ReleaseMap releaseMap) <- L.readIORef rrm
      () <- release
      L.writeIORef rrm (Internal.ReleaseMap (IntMap.delete key releaseMap))
