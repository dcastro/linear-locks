module LinearLocks.Mutex
  ( -- * Mutex
    mkMutex,
    Mutex,
    lock,

    -- * Mutex guards
    MutexGuard,
    readGuard,
    writeGuard,
    releaseGuard,

    -- * Mutex sets
    MutexSet,
    IsMutexSet (), -- Note: do not export the typeclass members
    mkMutexSet,
    lockMany,
  )
where

import LinearLocks.Internal.Mutex
import LinearLocks.Internal.MutexSet
