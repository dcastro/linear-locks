module LinearLocks.Mutex.Strict
  ( -- * Mutex
    new,
    Mutex,

    -- * Mutex guards
    MutexGuard,
    Mutex.read,
    write,
    release,
  )
where

import LinearLocks.Internal.Mutex as Mutex
