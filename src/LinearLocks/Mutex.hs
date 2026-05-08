module LinearLocks.Mutex
  ( -- * Mutex
    Mutex,
    new,
    acquire,

    -- * Mutex guards
    MutexGuard,
    Mutex.read,
    write,
    release,
  )
where

import LinearLocks.Internal (acquire)
import LinearLocks.Internal.Mutex as Mutex
