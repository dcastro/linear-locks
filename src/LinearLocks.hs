module LinearLocks
  ( MutexKey,
    Mutex,
    MutexGuard,
    mkMutex,
    lockScope,
    NestedLocksScopeException (..),
    lock,
    readGuard,
    writeGuard,
    releaseGuard,
  )
where

import LinearLocks.Internal
