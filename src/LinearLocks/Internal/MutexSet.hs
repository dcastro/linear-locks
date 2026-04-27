{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal.MutexSet where

import Control.Functor.Linear qualified as L
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List qualified as List
import GHC.TypeLits (Nat, type (+), type (<=))
import LinearLocks.Internal
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear.Internal qualified as Internal

-- | The index of a mutex in a mutex set.
newtype MutexSetIndex = MutexSetIndex Int

-- | A set of mutexes with the same level that can be locked together with `lockMany`.
data MutexSet set where
  MutexSet :: set -> [MutexSetIndex] -> MutexSet set

mkMutexSet :: (IsMutexSet set) => set -> MutexSet set
mkMutexSet set = MutexSet set sortedIndices
  where
    ids = collectIds set
    indices = coerce @_ @[MutexSetIndex] [0 .. length ids - 1]
    sortedIndices = ids `zip` indices & List.sortOn fst <&> snd

lockMany ::
  forall keyLvl mutexLvl set scope.
  (IsMutexSet set, mutexLvl ~ MutexSetLevel set, keyLvl <= mutexLvl) =>
  MutexKey keyLvl scope %1 ->
  MutexSet set ->
  RIO (MutexGuardSet set, MutexKey (mutexLvl + 1) scope)
lockMany MutexKey (MutexSet set indices) = L.do
  guards <- lockInOrder indices set
  L.pure (guards, MutexKey)

class IsMutexSet set where
  type MutexGuardSet set :: Type
  type MutexSetLevel set :: Nat

  collectIds :: set -> [MutexId]

  -- | Locks the mutexes in the set in the given order.
  -- E.g. `lockInOrder [1, 3, 2]` will lock the first mutex in the set, then the third, then the second.
  --
  -- Invariants:
  --   * The indices must contain every index in the set, without duplicates.
  lockInOrder :: [MutexSetIndex] -> set -> RIO (MutexGuardSet set)

instance IsMutexSet (Mutex lvl a, Mutex lvl b) where
  type MutexGuardSet (Mutex lvl a, Mutex lvl b) = (MutexGuard a, MutexGuard b)
  type MutexSetLevel (Mutex lvl a, Mutex lvl b) = lvl

  collectIds (m1, m2) = [m1.id, m2.id]

  lockInOrder indices (m1, m2) = L.do
    guards <- L.execStateT (runLocks indices) (Nothing, Nothing)
    case guards of
      (Just guard1, Just guard2) -> L.pure (guard1, guard2)
      guards -> releaseAndFail guards "Invalid indices or duplicate indices"
    where
      runLocks :: [MutexSetIndex] -> L.StateT (Maybe (MutexGuard a), Maybe (MutexGuard b)) RIO ()
      runLocks [] = L.pure ()
      runLocks (MutexSetIndex index : rest) = case index of
        0 -> L.do
          guard1 <- L.lift (unsafeLock m1)
          modifyM \case
            (Nothing, b) -> L.pure (Just guard1, b)
            guards -> L.do
              releaseGuard guard1
              releaseAndFail guards "Invalid indices or duplicate indices"
          runLocks rest
        1 -> L.do
          guard2 <- L.lift (unsafeLock m2)
          modifyM \case
            (a, Nothing) -> L.pure (a, Just guard2)
            guards -> L.do
              releaseGuard guard2
              releaseAndFail guards "Invalid indices or duplicate indices"
          runLocks rest
        _ -> L.lift (failRIO "Invalid index")

      releaseAndFail :: (Maybe (MutexGuard a), Maybe (MutexGuard b)) %1 -> String -> RIO x
      releaseAndFail (guard1, guard2) errMsg = L.do
        releaseGuardMb guard1
        releaseGuardMb guard2
        failRIO errMsg

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

releaseGuardMb :: Maybe (MutexGuard a) %1 -> RIO ()
releaseGuardMb = \case
  Nothing -> L.pure ()
  Just guard -> releaseGuard guard

failRIO :: String -> RIO a
failRIO msg = L.do
  Internal.unsafeFromSystemIO (fail msg)

modifyM :: forall m s. (L.Functor m) => (s %1 -> m s) %1 -> L.StateT s m ()
modifyM f =
  L.StateT \s -> L.do
    f s L.<&> \s' -> ((), s')
