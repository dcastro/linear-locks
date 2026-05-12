module LinearLocks.RWLock
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

    -- | To add t'RWLock's to t'LinearLocks.LockSet's, you can use the t'AsRead' and t'AsWrite' newtypes.
    --
    -- >>> import LinearLocks
    -- >>> import LinearLocks.RWLock qualified as RWLock
    -- >>> rw1 <- RWLock.new 0 "hello"
    -- >>> rw2 <- RWLock.new 0 "world"
    -- >>> set <- newLockSet (RWLock.AsRead rw1, RWLock.AsWrite rw2)
    --
    -- Or the 'GHC.Records.HasField' instances:
    --
    -- >>> :set -XOverloadedRecordDot
    -- >>> set <- newLockSet (rw1.asRead, rw2.asWrite)
    AsRead (..),
    AsWrite (..),
  )
where

import LinearLocks.Internal.RWLock as RWLock
