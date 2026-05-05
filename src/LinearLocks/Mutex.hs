module LinearLocks.Mutex
  ( -- * Mutex
    mkMutex,
    Mutex,
    lock,

    -- * Mutex guards
    MutexGuard,
    Mutex.read,
    write,
    release,

    -- * Mutex sets
    MutexSet,
    IsMutexSet (), -- Note: do not export the typeclass members
    mkMutexSet,
    lockMany,
  )
where

import LinearLocks.Internal.Mutex as Mutex
import LinearLocks.Internal.MutexSet
