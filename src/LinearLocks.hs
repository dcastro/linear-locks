{- ORMOLU_DISABLE -}
{- |

@linear-locks@ provides a locking primitive t`Mutex` that is statically guaranteed to not lead to deadlocks.

An in-depth description and tutorial can be found in the [README](https://github.com/dcastro/linear-locks#readme).

It is meant to be used with @QualifiedDo@ and these imports:

>>> :set -XQualifiedDo -XGHC2024 -XBlockArguments
>>> import Prelude.Linear (Ur (..))
>>> import Control.Functor.Linear qualified as Linear
>>> import System.IO.Resource.Linear.Internal qualified as Internal (unsafeFromSystemIO)
>>> :{
example :: IO ()
example = do
  -- Create mutexes with a chosen level
  configMutex <- new 0 Config { verbose = True }
  dbMutex <- new 1 DbConn {}
  --
  -- Enter a lockscope
  lockScope \key -> Linear.do
    -- Acquire mutexes
    (configGuard, key) <- lock key configMutex
    (dbGuard, key) <- lock key dbMutex
    --
    -- Read/write
    (Ur config, configGuard) <- read configGuard
    configGuard <- write configGuard config { verbose = False }
    --
    -- IO actions
    Internal.unsafeFromSystemIO do
      putStrLn $ "Verbose mode was: " <> show (verbose config)
    --
    -- Release mutexes
    release configGuard
    release dbGuard
    Linear.pure (Ur (), key)
:}

-}
{- ORMOLU_ENABLE -}
module LinearLocks
  ( -- * Lock scope
    lockScope,
    MutexKey,
    NestedLocksScopeException (..),
    lock,
    Lockable (), -- Note: do not export the typeclass members

    -- * Mutex sets
    MutexSet,
    IsMutexSet (), -- Note: do not export the typeclass members
    newMutexSet,
    lockMany,
  )
where

import LinearLocks.Internal
import LinearLocks.Internal.MutexSet

-- $setup
-- >>> data Config = Config { verbose :: Bool }
-- >>> data DbConn = DbConn
