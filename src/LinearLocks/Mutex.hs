module LinearLocks.Mutex
  ( -- * Mutex
    new,
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
    newMutexSet,
    lockMany,
  )
where

import LinearLocks.Internal.Mutex as Mutex
import LinearLocks.Internal.MutexSet
