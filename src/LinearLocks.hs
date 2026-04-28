module LinearLocks
  ( mkMutex,
    Mutex,
    lockScope,
    MutexKey,
    NestedLocksScopeException (..),
    lock,
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

import LinearLocks.Internal
import LinearLocks.Internal.MutexSet
