{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal.MutexSet where

import Control.Functor.Linear qualified as L
import Control.Monad.ST (ST, runST)
import Data.Function (on)
import Data.Kind (Type)
import Data.Vector.Algorithms.Insertion qualified as Sort
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
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

-- | Creates a `MutexSet` from a set of mutexes.
-- All mutexes must have the same level.
--
-- Mutexes in a 'MutexSet' can be locked simultaneously using 'lockMany'.
--
-- Fails if the set contains duplicate mutexes.
--
-- >>> m1 <- mkMutex 1 "a"
-- >>> m2 <- mkMutex 1 "b"
-- >>> m3 <- mkMutex 1 "c"
-- >>> set <- mkMutexSet (m1, m2, m3)
mkMutexSet :: forall m set. (IsMutexSet set, MonadFail m) => set -> m (MutexSet set)
mkMutexSet set =
  if hasDups
    then fail "MutexSet: duplicate mutexes are not allowed"
    else pure $ MutexSet set sortedIndices
  where
    (hasDups, sortedIndices) = runST do
      idsAndIndices <- VU.thaw $ VU.fromList $ collectIds set `zip` [MutexSetIndex 0 ..]

      -- Sort by mutex IDs
      Sort.sortBy (compare `on` fst) idsAndIndices

      -- Check whether this set contains duplicate mutexes.
      -- NOTE: the vector must already be sorted.
      hasDups <- hasDuplicateIds idsAndIndices

      sortedIndices <- VU.map snd <$> VU.unsafeFreeze idsAndIndices

      pure (hasDups, sortedIndices)

    hasDuplicateIds :: VUM.MVector (VUM.PrimState (ST s)) (MutexId, MutexSetIndex) -> ST s Bool
    hasDuplicateIds idsAndIndices = do
      let go i =
            if i >= VUM.length idsAndIndices - 1
              then pure False
              else do
                (id1, _) <- VUM.read idsAndIndices i
                (id2, _) <- VUM.read idsAndIndices (i + 1)
                if id1 == id2
                  then pure True
                  else go (i + 1)
      go 0

lockMany ::
  forall keyLvl mutexLvl set.
  (IsMutexSet set, mutexLvl ~ MutexSetLevel set, keyLvl <= mutexLvl) =>
  MutexKey keyLvl %1 ->
  MutexSet set ->
  RIO (MutexGuardSet set, MutexKey (mutexLvl + 1))
lockMany UnsafeMutexKey (MutexSet set indices) = L.do
  guards <- lockInOrder indices set
  L.pure (guards, UnsafeMutexKey)

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
      (Just g1, Just g2) -> L.pure (g1, g2)
      guards -> releaseAndFail guards missingIndices
    where
      lockAt :: MutexSetIndex -> L.StateT (Maybe (MutexGuard a), Maybe (MutexGuard b)) RIO ()
      lockAt (MutexSetIndex index) =
        case index of
          0 -> modifyM \case
            (Nothing, g2) -> L.do
              g1 <- unsafeLock m1
              L.pure (Just g1, g2)
            guards -> releaseAndFail guards (dupIndex index)
          1 -> modifyM \case
            (g1, Nothing) -> L.do
              g2 <- unsafeLock m2
              L.pure (g1, Just g2)
            guards -> releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (MutexGuard a), Maybe (MutexGuard b)) %1 -> String -> RIO x
      releaseAndFail (g1, g2) errMsg = L.do
        releaseGuardMb g1
        releaseGuardMb g2
        failRIO errMsg

instance IsMutexSet (Mutex lvl a, Mutex lvl b, Mutex lvl c) where
  type MutexGuardSet (Mutex lvl a, Mutex lvl b, Mutex lvl c) = (MutexGuard a, MutexGuard b, MutexGuard c)
  type MutexSetLevel (Mutex lvl a, Mutex lvl b, Mutex lvl c) = lvl

  collectIds (m1, m2, m3) = [m1.id, m2.id, m3.id]

  lockInOrder indices (m1, m2, m3) = L.do
    guards <- L.execStateT (forM_' indices lockAt) (Nothing, Nothing, Nothing)
    case guards of
      (Just g1, Just g2, Just g3) -> L.pure (g1, g2, g3)
      guards -> releaseAndFail guards missingIndices
    where
      lockAt :: MutexSetIndex -> L.StateT (Maybe (MutexGuard a), Maybe (MutexGuard b), Maybe (MutexGuard c)) RIO ()
      lockAt (MutexSetIndex index) =
        case index of
          0 -> modifyM \case
            (Nothing, g2, g3) -> L.do
              g1 <- unsafeLock m1
              L.pure (Just g1, g2, g3)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          1 -> modifyM \case
            (g1, Nothing, g3) -> L.do
              g2 <- unsafeLock m2
              L.pure (g1, Just g2, g3)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          2 -> modifyM \case
            (g1, g2, Nothing) -> L.do
              g3 <- unsafeLock m3
              L.pure (g1, g2, Just g3)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (MutexGuard a), Maybe (MutexGuard b), Maybe (MutexGuard c)) %1 -> String -> RIO x
      releaseAndFail (g1, g2, g3) errMsg = L.do
        releaseGuardMb g1
        releaseGuardMb g2
        releaseGuardMb g3
        failRIO errMsg

instance IsMutexSet (Mutex lvl a, Mutex lvl b, Mutex lvl c, Mutex lvl d) where
  type MutexGuardSet (Mutex lvl a, Mutex lvl b, Mutex lvl c, Mutex lvl d) = (MutexGuard a, MutexGuard b, MutexGuard c, MutexGuard d)
  type MutexSetLevel (Mutex lvl a, Mutex lvl b, Mutex lvl c, Mutex lvl d) = lvl

  collectIds (m1, m2, m3, m4) = [m1.id, m2.id, m3.id, m4.id]

  lockInOrder indices (m1, m2, m3, m4) = L.do
    guards <- L.execStateT (forM_' indices lockAt) (Nothing, Nothing, Nothing, Nothing)
    case guards of
      (Just g1, Just g2, Just g3, Just g4) -> L.pure (g1, g2, g3, g4)
      guards -> releaseAndFail guards missingIndices
    where
      lockAt :: MutexSetIndex -> L.StateT (Maybe (MutexGuard a), Maybe (MutexGuard b), Maybe (MutexGuard c), Maybe (MutexGuard d)) RIO ()
      lockAt (MutexSetIndex index) =
        case index of
          0 -> modifyM \case
            (Nothing, g2, g3, g4) -> L.do
              g1 <- unsafeLock m1
              L.pure (Just g1, g2, g3, g4)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          1 -> modifyM \case
            (g1, Nothing, g3, g4) -> L.do
              g2 <- unsafeLock m2
              L.pure (g1, Just g2, g3, g4)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          2 -> modifyM \case
            (g1, g2, Nothing, g4) -> L.do
              g3 <- unsafeLock m3
              L.pure (g1, g2, Just g3, g4)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          3 -> modifyM \case
            (g1, g2, g3, Nothing) -> L.do
              g4 <- unsafeLock m4
              L.pure (g1, g2, g3, Just g4)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (MutexGuard a), Maybe (MutexGuard b), Maybe (MutexGuard c), Maybe (MutexGuard d)) %1 -> String -> RIO x
      releaseAndFail (g1, g2, g3, g4) errMsg = L.do
        releaseGuardMb g1
        releaseGuardMb g2
        releaseGuardMb g3
        releaseGuardMb g4
        failRIO errMsg

instance IsMutexSet (Mutex lvl a, Mutex lvl b, Mutex lvl c, Mutex lvl d, Mutex lvl e) where
  type MutexGuardSet (Mutex lvl a, Mutex lvl b, Mutex lvl c, Mutex lvl d, Mutex lvl e) = (MutexGuard a, MutexGuard b, MutexGuard c, MutexGuard d, MutexGuard e)
  type MutexSetLevel (Mutex lvl a, Mutex lvl b, Mutex lvl c, Mutex lvl d, Mutex lvl e) = lvl

  collectIds (m1, m2, m3, m4, m5) = [m1.id, m2.id, m3.id, m4.id, m5.id]

  lockInOrder indices (m1, m2, m3, m4, m5) = L.do
    guards <- L.execStateT (forM_' indices lockAt) (Nothing, Nothing, Nothing, Nothing, Nothing)
    case guards of
      (Just g1, Just g2, Just g3, Just g4, Just g5) -> L.pure (g1, g2, g3, g4, g5)
      guards -> releaseAndFail guards missingIndices
    where
      lockAt :: MutexSetIndex -> L.StateT (Maybe (MutexGuard a), Maybe (MutexGuard b), Maybe (MutexGuard c), Maybe (MutexGuard d), Maybe (MutexGuard e)) RIO ()
      lockAt (MutexSetIndex index) =
        case index of
          0 -> modifyM \case
            (Nothing, g2, g3, g4, g5) -> L.do
              g1 <- unsafeLock m1
              L.pure (Just g1, g2, g3, g4, g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          1 -> modifyM \case
            (g1, Nothing, g3, g4, g5) -> L.do
              g2 <- unsafeLock m2
              L.pure (g1, Just g2, g3, g4, g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          2 -> modifyM \case
            (g1, g2, Nothing, g4, g5) -> L.do
              g3 <- unsafeLock m3
              L.pure (g1, g2, Just g3, g4, g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          3 -> modifyM \case
            (g1, g2, g3, Nothing, g5) -> L.do
              g4 <- unsafeLock m4
              L.pure (g1, g2, g3, Just g4, g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          4 -> modifyM \case
            (g1, g2, g3, g4, Nothing) -> L.do
              g5 <- unsafeLock m5
              L.pure (g1, g2, g3, g4, Just g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (MutexGuard a), Maybe (MutexGuard b), Maybe (MutexGuard c), Maybe (MutexGuard d), Maybe (MutexGuard e)) %1 -> String -> RIO x
      releaseAndFail (g1, g2, g3, g4, g5) errMsg = L.do
        releaseGuardMb g1
        releaseGuardMb g2
        releaseGuardMb g3
        releaseGuardMb g4
        releaseGuardMb g5
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
