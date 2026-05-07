{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal.RWLock where

import Control.Concurrent (MVar)
import Control.Concurrent qualified as MVar
import Control.Concurrent.ReadWriteLock qualified as Conc
import Control.Functor.Linear qualified as L
import Control.Monad.IO.Class.Linear qualified as L
import Data.Atomics.Counter qualified as Atomic
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import GHC.TypeLits (Nat)
import LinearLocks.Internal
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Linear qualified as L
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
import System.IO.Resource.Linear.Internal qualified as Internal

-- | A deadlock-free lock that allows multiple concurrent readers or a single writer.
data RWLock (lvl :: Nat) a = RWLock
  { var :: IORef a,
    -- | A read-write lock gating access to the `IORef`.
    lock :: Conc.RWLock,
    -- | The unique ID for this lock. It's used to ensure t'LinearLocks.MutexSet's don't contain duplicate mutexes, see 'LinearLocks.newMutexSet'.
    id :: MutexId
  }

-- | Creates a new read-write lock with the given initial value.
--
-- The @lvl@ parameter determines the order in which this mutex can be acquired relative to other mutexes.
--
-- It does not have to be unique, multiple mutexes can have the same level.
-- Mutexes with the same level can be added to a t`LinearLocks.MutexSet` and acquired with 'LinearLocks.lockMany'.
new :: forall a. forall (lvl :: Nat) -> a -> IO (RWLock lvl a)
new _lvl a = do
  lock <- Conc.new
  var <- IORef.newIORef a
  newId <- Atomic.incrCounter 1 mutexIdCounter
  pure
    RWLock
      { var,
        lock,
        id = MutexId newId
      }

class Readable guard where
  type Elem guard :: Type
  read :: guard %1 -> RIO (Ur (Elem guard), guard)

----------------------------------------------------------------------------
-- Read mode
----------------------------------------------------------------------------

-- | A t`ReadGuard` represents the ownership of a RWLock in read mode.
--
-- It must be released with `release`, after which the guard will be consumed and can no longer be used.
data ReadGuard a = ReadGuard
  { resource :: RIO.Resource ReadResource,
    -- | The value that was read when the lock was acquired.
    readValue :: Ur a
  }

newtype ReadResource = ReadResource
  { lock :: Conc.RWLock
  }

newtype AsRead lvl a = AsRead (RWLock lvl a)

instance Lockable (AsRead lvl a) where
  type Guard (AsRead lvl a) = ReadGuard a
  type Level (AsRead lvl a) = lvl

  getId (AsRead m) = m.id

  unsafeLock :: forall lvl a. AsRead lvl a -> RIO (ReadGuard a)
  unsafeLock (AsRead m) = L.do
    -- Acquire the rwlock in "read mode" and *then* read the `IORef`.
    resource <- RIO.unsafeAcquire acq rel
    Ur readValue <- L.liftSystemIOU (IORef.readIORef m.var)
    L.pure
      ReadGuard
        { resource = resource,
          readValue = Ur readValue
        }
    where
      acq :: L.IO (Ur ReadResource)
      acq = L.do
        L.fromSystemIO L.$ Conc.acquireRead m.lock
        L.pure (Ur (ReadResource {lock = m.lock}))

      rel :: ReadResource -> L.IO ()
      rel (ReadResource lock) =
        L.fromSystemIO L.$ Conc.releaseRead lock

instance Releasable (ReadGuard a) where
  doRelease = releaseRead

instance Readable (ReadGuard a) where
  type Elem (ReadGuard a) = a

  read :: ReadGuard a %1 -> RIO (Ur a, ReadGuard a)
  read (ReadGuard resource (Ur readValue)) =
    L.pure (Ur readValue, ReadGuard {resource, readValue = Ur readValue})

-- | Releases the lock.
releaseRead :: ReadGuard a %1 -> RIO ()
releaseRead (ReadGuard resource (Ur _readValue)) =
  RIO.release resource
