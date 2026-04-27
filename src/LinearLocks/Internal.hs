{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal where

import Control.Functor.Linear qualified as L
import GHC.TypeLits (type (+), type (<=))
import LinearLocks.Internal.Mutex
import LinearLocks.Internal.MutexSet
import Prelude.Linear (Ur (..))
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
import System.IO.Resource.Linear.Internal qualified as Internal

-- | Acquire a mutex.
-- Consumes the key and return a new key (with an increased level).
lock ::
  forall a keyLvl mutexLvl scope.
  (keyLvl <= mutexLvl) =>
  MutexKey keyLvl scope %1 ->
  Mutex mutexLvl a ->
  RIO (MutexGuard a, MutexKey (mutexLvl + 1) scope)
lock MutexKey m = L.do
  guard <- unsafeLock m
  L.pure (guard, MutexKey)

lockMany ::
  forall keyLvl mutexLvl set scope.
  (IsMutexSet set, mutexLvl ~ MutexSetLevel set, keyLvl <= mutexLvl) =>
  MutexKey keyLvl scope %1 ->
  MutexSet set ->
  RIO (MutexGuardSet set, MutexKey (mutexLvl + 1) scope)
lockMany MutexKey (MutexSet set indices) = L.do
  guards <- lockInOrder indices set
  L.pure (guards, MutexKey)

readGuard :: MutexGuard a %1 -> RIO (Ur a, MutexGuard a)
readGuard (MutexGuard resource (Ur newValue)) =
  L.pure (Ur newValue, MutexGuard {resource, newValue = Ur newValue})

writeGuard :: MutexGuard a %1 -> a -> RIO (MutexGuard a)
writeGuard (MutexGuard resource (Ur _)) newValue =
  L.pure (MutexGuard {resource, newValue = Ur newValue})

releaseGuard :: MutexGuard a %1 -> RIO ()
releaseGuard (MutexGuard (Internal.UnsafeResource key mr) (Ur newValue)) =
  RIO.release (Internal.UnsafeResource key (mr {commitValue = newValue}))
