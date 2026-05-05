module LinearLocks.Mutex.Strict
  ( -- * Mutex
    new,
    Mutex,
    lock,

    -- * Mutex guards
    MutexGuard,
    Mutex.read,
    write,
    release,
  )
where

import LinearLocks.Internal.Mutex as Mutex
