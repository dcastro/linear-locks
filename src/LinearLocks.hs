{- ORMOLU_DISABLE -}
{- |

@linear-locks@ provides locking primitives that are statically guaranteed to not lead to deadlocks.

An in-depth description and tutorial can be found in the [README](https://github.com/dcastro/linear-locks#readme).

It is meant to be used with @QualifiedDo@ and these imports:

>>> :set -XQualifiedDo -XGHC2024 -XBlockArguments
>>> import LinearLocks
>>> import LinearLocks.Mutex qualified as Mutex
>>> import Prelude.Linear (Ur (..))
>>> import Control.Functor.Linear qualified as Linear
>>> import Control.Monad.IO.Class.Linear qualified as Linear


>>> :{
example :: IO ()
example = do
  -- Create mutexes with a chosen level
  configMutex <- Mutex.new 0 Config { verbose = True }
  dbMutex <- Mutex.new 1 DbConn {}
  --
  -- Enter a lockscope
  lockScope \key -> Linear.do
    -- Acquire mutexes
    (configGuard, key) <- acquire key configMutex
    (dbGuard, key) <- acquire key dbMutex
    --
    -- Read/write
    (Ur config, configGuard) <- Mutex.read configGuard
    configGuard <- Mutex.write configGuard config { verbose = False }
    --
    -- IO actions
    Linear.liftSystemIO do
      putStrLn $ "Verbose mode was: " <> show (verbose config)
    --
    -- Release mutexes
    Mutex.release configGuard
    Mutex.release dbGuard
    Linear.pure (Ur (), key)
:}

-}
{- ORMOLU_ENABLE -}
module LinearLocks
  ( -- * Lock scope
    lockScope,
    LockKey,
    NestedLocksScopeException (..),
    acquire,
    Acquirable (), -- Note: do not export the typeclass members

    -- * Mutex sets
    MutexSet,
    IsMutexSet (), -- Note: do not export the typeclass members
    newMutexSet,
    acquireMany,
  )
where

import LinearLocks.Internal
import LinearLocks.Internal.MutexSet

-- $setup
-- >>> data Config = Config { verbose :: Bool }
-- >>> data DbConn = DbConn
