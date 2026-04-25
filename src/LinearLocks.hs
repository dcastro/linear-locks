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
  )
where

import LinearLocks.Internal
