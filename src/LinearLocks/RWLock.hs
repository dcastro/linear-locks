module LinearLocks.RWLock
  ( -- * RWLock
    RWLock,
    new,

    -- * Read mode
    AsRead (..),
    ReadGuard,
    RWLock.read,
    releaseRead,

    -- * Write mode
    AsWrite (..),
    WriteGuard,
    write,
    releaseWrite,
  )
where

import LinearLocks.Internal.RWLock as RWLock
