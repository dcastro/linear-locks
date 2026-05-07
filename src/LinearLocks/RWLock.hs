module LinearLocks.RWLock
  ( -- * RWLock
    RWLock,
    new,

    -- * Read mode
    ReadGuard,
    RWLock.read,
    releaseRead,

    -- * Write mode
    WriteGuard,
    write,
    releaseWrite,
  )
where

import LinearLocks.Internal.RWLock as RWLock
