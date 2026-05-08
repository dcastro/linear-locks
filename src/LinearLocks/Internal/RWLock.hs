{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal.RWLock where

import Control.Concurrent.ReadWriteLock qualified as Conc
import Control.Functor.Linear qualified as L
import Control.Monad.IO.Class.Linear qualified as L
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Kind (Type)
import GHC.TypeLits (Nat)
import LinearLocks.Internal
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Linear qualified as L
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO

-- $setup
-- >>> data Config = Config { verbose :: Bool }

{- ORMOLU_DISABLE -}
{- | A deadlock-free lock that allows multiple concurrent readers or a single writer.

>>> import LinearLocks
>>> import LinearLocks.RWLock qualified as RWLock
>>> import Prelude.Linear (Ur (..))
>>> import Control.Functor.Linear qualified as Linear

>>> :{
example :: IO ()
example = do
  configLock <- RWLock.new 0 Config { verbose = True }
  --
  -- Enter a lockscope
  lockScope \key -> Linear.do
    -- Acquire the lock in "write mode"
    (guard, key) <- lock key (RWLock.AsWrite configLock)
    --
    -- Read/write
    (Ur config, guard) <- RWLock.read guard
    guard <- RWLock.write guard config { verbose = False }
    --
    -- Release lock
    RWLock.releaseWrite guard
    Linear.pure (Ur (), key)
:}
-}
{- ORMOLU_ENABLE -}
data RWLock (lvl :: Nat) a = RWLock
  { var :: IORef a,
    -- | A read-write lock gating access to the `IORef`.
    lock :: Conc.RWLock,
    -- | The unique ID for this lock. It's used to ensure t'LinearLocks.MutexSet's don't contain duplicate locks, see 'LinearLocks.newMutexSet'.
    id :: MutexId
  }

-- | Creates a new read-write lock with the given initial value.
--
-- The @lvl@ parameter determines the order in which this lock can be acquired relative to other locks.
--
-- It does not have to be unique, multiple locks can have the same level.
-- Locks with the same level can be added to a t`LinearLocks.MutexSet` and acquired with 'LinearLocks.lockMany'.
new :: forall a. forall (lvl :: Nat) -> a -> IO (RWLock lvl a)
new _lvl a = do
  lock <- Conc.new
  var <- IORef.newIORef a
  id <- nextMutexId
  pure RWLock {var, lock, id}

class Readable guard where
  type Elem guard :: Type
  read :: guard %1 -> RIO (Ur (Elem guard), guard)

----------------------------------------------------------------------------
-- Read mode
----------------------------------------------------------------------------

-- | A t`ReadGuard` represents the ownership of a RWLock in read mode.
--
-- It must be released with `releaseRead`, after which the guard will be consumed and can no longer be used.
data ReadGuard a = ReadGuard
  { resource :: RIO.Resource Resource,
    -- | The value that was read when the lock was acquired.
    readValue :: Ur a
  }

newtype Resource = Resource
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
      acq :: L.IO (Ur Resource)
      acq = L.do
        L.fromSystemIO L.$ Conc.acquireRead m.lock
        L.pure (Ur (Resource {lock = m.lock}))

      rel :: Resource -> L.IO ()
      rel (Resource lock) =
        L.fromSystemIO L.$ Conc.releaseRead lock

-- | Releases the lock.
releaseRead :: ReadGuard a %1 -> RIO ()
releaseRead (ReadGuard resource (Ur _readValue)) =
  RIO.release resource

instance Releasable (ReadGuard a) where
  doRelease = releaseRead

instance Readable (ReadGuard a) where
  type Elem (ReadGuard a) = a

  read :: ReadGuard a %1 -> RIO (Ur a, ReadGuard a)
  read (ReadGuard resource (Ur readValue)) =
    L.pure (Ur readValue, ReadGuard {resource, readValue = Ur readValue})

----------------------------------------------------------------------------
-- Write mode
----------------------------------------------------------------------------

-- | A t`WriteGuard` represents the ownership of a RWLock in write mode.
--
-- It must be released with `releaseWrite`, after which the guard will be consumed and can no longer be used.
data WriteGuard a = WriteGuard
  { resource :: RIO.Resource Resource,
    -- | The latest value set by the user.
    -- This will be comitted when the guard is released.
    newValue :: Ur a,
    var :: Ur (IORef a)
  }

newtype AsWrite lvl a = AsWrite (RWLock lvl a)

instance Lockable (AsWrite lvl a) where
  type Guard (AsWrite lvl a) = WriteGuard a
  type Level (AsWrite lvl a) = lvl

  getId (AsWrite m) = m.id

  unsafeLock :: forall lvl a. AsWrite lvl a -> RIO (WriteGuard a)
  unsafeLock (AsWrite m) = L.do
    -- Acquire the rwlock in "write mode" and *then* read the `IORef`.
    resource <- RIO.unsafeAcquire acq rel
    Ur initialValue <- L.liftSystemIOU (IORef.readIORef m.var)
    L.pure
      WriteGuard
        { resource = resource,
          newValue = Ur initialValue,
          var = Ur m.var
        }
    where
      acq :: L.IO (Ur Resource)
      acq = L.do
        L.fromSystemIO L.$ Conc.acquireWrite m.lock
        L.pure (Ur (Resource {lock = m.lock}))

      rel :: Resource -> L.IO ()
      rel (Resource lock) =
        L.fromSystemIO L.$ Conc.releaseWrite lock

-- | Releases the lock and commits the latest value set by `write`.
releaseWrite :: WriteGuard a %1 -> RIO ()
releaseWrite (WriteGuard resource (Ur newValue) (Ur var)) = L.do
  L.liftSystemIO $ IORef.writeIORef var newValue
  RIO.release resource

instance Releasable (WriteGuard a) where
  doRelease = releaseWrite

instance Readable (WriteGuard a) where
  type Elem (WriteGuard a) = a

  read :: WriteGuard a %1 -> RIO (Ur a, WriteGuard a)
  read (WriteGuard resource (Ur newValue) var) =
    L.pure (Ur newValue, WriteGuard {resource, newValue = Ur newValue, var})

-- | Writes a new value to the t'RWLock', which will be committed when the guard is released.
--
-- If an exception is thrown after `write` but before `releaseWrite`,
-- the t'RWLock' will be rolled back to its original state.
write :: WriteGuard a %1 -> a -> RIO (WriteGuard a)
write (WriteGuard resource (Ur _) var) newValue =
  L.pure (WriteGuard {resource, newValue = Ur newValue, var})
