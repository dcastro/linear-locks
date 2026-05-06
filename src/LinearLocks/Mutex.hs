module LinearLocks.Mutex
  ( -- * Mutex
    Mutex,
    new,

    -- * Mutex guards
    MutexGuard,
    Mutex.read,
    write,
    release,
  )
where

import LinearLocks.Internal.Mutex as Mutex
