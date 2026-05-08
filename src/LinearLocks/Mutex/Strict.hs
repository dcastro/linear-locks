module LinearLocks.Mutex.Strict
  ( -- * Mutex
    Mutex,
    new,
    acquire,

    -- * Mutex guards
    MutexGuard,
    StrictMutex.read,
    write,
    release,
  )
where

import LinearLocks.Internal (acquire)
import LinearLocks.Internal.StrictMutex as StrictMutex
