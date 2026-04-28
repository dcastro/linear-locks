{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal.MutexSet where

import Control.Functor.Linear qualified as L
import Control.Monad.ST (runST)
import Data.Function (on)
import Data.Kind (Type)
import Data.Vector.Algorithms.Insertion qualified as Sort
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import GHC.TypeLits (Nat, type (+), type (<=))
import LinearLocks.Internal
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear.Internal qualified as Internal

-- | The index of a mutex in a mutex set.
newtype MutexSetIndex = MutexSetIndex Int
  deriving newtype (Enum)

newtype instance VU.MVector s MutexSetIndex = MV_MutexSetIndex (VP.MVector s Int)

newtype instance VU.Vector MutexSetIndex = V_MutexSetIndex (VP.Vector Int)

deriving via (VU.UnboxViaPrim Int) instance VGM.MVector VU.MVector MutexSetIndex

deriving via (VU.UnboxViaPrim Int) instance VG.Vector VU.Vector MutexSetIndex

instance VU.Unbox MutexSetIndex

-- | A set of mutexes with the same level that can be locked together with `lockMany`.
data MutexSet set where
  MutexSet :: set -> VU.Vector MutexSetIndex -> MutexSet set

mkMutexSet :: (IsMutexSet set) => set -> MutexSet set
mkMutexSet set =
  MutexSet set sortedIndices
  where
    sortedIndices = runST do
      idsAndIndices <- VU.thaw $ VU.fromList $ collectIds set `zip` [MutexSetIndex 0 ..]

      -- Sort by mutex IDs
      Sort.sortBy (compare `on` fst) idsAndIndices

      VU.map snd <$> VU.unsafeFreeze idsAndIndices

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
  lockInOrder :: VU.Vector MutexSetIndex -> set -> RIO (MutexGuardSet set)

instance IsMutexSet (Mutex lvl a, Mutex lvl b) where
  type MutexGuardSet (Mutex lvl a, Mutex lvl b) = (MutexGuard a, MutexGuard b)
  type MutexSetLevel (Mutex lvl a, Mutex lvl b) = lvl

  collectIds (m1, m2) = [m1.id, m2.id]

  lockInOrder indices (m1, m2) = L.do
    guards <- L.execStateT (forM_' indices lockAt) (Nothing, Nothing)
    case guards of
      (Just guard1, Just guard2) -> L.pure (guard1, guard2)
      guards -> releaseAndFail guards missingIndices
    where
      lockAt :: MutexSetIndex -> L.StateT (Maybe (MutexGuard a), Maybe (MutexGuard b)) RIO ()
      lockAt (MutexSetIndex index) =
        case index of
          0 -> L.do
            modifyM \case
              (Nothing, b) -> L.do
                guard1 <- unsafeLock m1
                L.pure (Just guard1, b)
              guards -> releaseAndFail guards (dupIndex index)
          1 -> L.do
            modifyM \case
              (a, Nothing) -> L.do
                guard2 <- unsafeLock m2
                L.pure (a, Just guard2)
              guards -> releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (MutexGuard a), Maybe (MutexGuard b)) %1 -> String -> RIO x
      releaseAndFail (guard1, guard2) errMsg = L.do
        releaseGuardMb guard1
        releaseGuardMb guard2
        failRIO errMsg

instance IsMutexSet (Mutex lvl a, Mutex lvl b, Mutex lvl c) where
  type MutexGuardSet (Mutex lvl a, Mutex lvl b, Mutex lvl c) = (MutexGuard a, MutexGuard b, MutexGuard c)
  type MutexSetLevel (Mutex lvl a, Mutex lvl b, Mutex lvl c) = lvl

  collectIds (m1, m2, m3) = [m1.id, m2.id, m3.id]

  lockInOrder indices (m1, m2, m3) = L.do
    guards <- L.execStateT (forM_' indices lockAt) (Nothing, Nothing, Nothing)
    case guards of
      (Just guard1, Just guard2, Just guard3) -> L.pure (guard1, guard2, guard3)
      guards -> releaseAndFail guards missingIndices
    where
      lockAt :: MutexSetIndex -> L.StateT (Maybe (MutexGuard a), Maybe (MutexGuard b), Maybe (MutexGuard c)) RIO ()
      lockAt (MutexSetIndex index) =
        case index of
          0 -> L.do
            modifyM \case
              (Nothing, b, c) -> L.do
                guard1 <- unsafeLock m1
                L.pure (Just guard1, b, c)
              guards -> L.do
                releaseAndFail guards (dupIndex index)
          1 -> L.do
            modifyM \case
              (a, Nothing, c) -> L.do
                guard2 <- unsafeLock m2
                L.pure (a, Just guard2, c)
              guards -> L.do
                releaseAndFail guards (dupIndex index)
          2 -> L.do
            modifyM \case
              (a, b, Nothing) -> L.do
                guard3 <- unsafeLock m3
                L.pure (a, b, Just guard3)
              guards -> L.do
                releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (MutexGuard a), Maybe (MutexGuard b), Maybe (MutexGuard c)) %1 -> String -> RIO x
      releaseAndFail (guard1, guard2, guard3) errMsg = L.do
        releaseGuardMb guard1
        releaseGuardMb guard2
        releaseGuardMb guard3
        failRIO errMsg

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

missingIndices :: String
missingIndices = "MutexSet: missing indices"

dupIndex :: Int -> String
dupIndex index = "MutexSet: duplicate index: " <> show index

invalidIndex :: Int -> String
invalidIndex index = "MutexSet: invalid index: " <> show index

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

-- | A version of `VU.forM_` that runs in a linear monad.
forM_' :: (VU.Unbox a, L.Monad m) => VU.Vector a -> (a -> m ()) -> m ()
forM_' vec action = go 0
  where
    go i
      | i >= VU.length vec = L.pure ()
      | otherwise = L.do
          action (vec VU.! i)
          go (i + 1)
