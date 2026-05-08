module LinearLocks.RWLock.Strict
  ( -- * RWLock
    RWLock,
    new,
    acquireRead,
    acquireWrite,

    -- * Read mode
    ReadGuard,
    RWLock.read,
    releaseRead,

    -- * Write mode
    WriteGuard,
    write,
    releaseWrite,

    -- * Lock sets

    -- | The t'AsRead' and t'AsWrite' newtypes can be used to add t'RWLock's to t'LinearLocks.LockSet's.
    --
    -- >>> import LinearLocks
    -- >>> import LinearLocks.RWLock.Strict qualified as RWLock
    -- >>> rw1 <- RWLock.new 0 "hello"
    -- >>> rw2 <- RWLock.new 0 "world"
    -- >>> set <- newLockSet (RWLock.AsRead rw1, RWLock.AsWrite rw2)
    AsRead (..),
    AsWrite (..),
  )
where

import LinearLocks.Internal.StrictRWLock as RWLock
