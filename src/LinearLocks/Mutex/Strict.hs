module LinearLocks.Mutex.Strict
  ( -- * Mutex
    Mutex,
    new,

    -- * Mutex guards
    MutexGuard,
    StrictMutex.read,
    write,
    release,
  )
where

import LinearLocks.Internal.StrictMutex as StrictMutex
