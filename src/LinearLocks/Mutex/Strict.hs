module LinearLocks.Mutex.Strict
  ( -- * Mutex
    new,
    Mutex,

    -- * Mutex guards
    MutexGuard,
    StrictMutex.read,
    write,
    release,
  )
where

import LinearLocks.Internal.StrictMutex as StrictMutex
