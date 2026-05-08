{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

module LinearLocks.Internal.LockSet where

import Control.Functor.Linear qualified as L
import Control.Monad.IO.Class.Linear qualified as L
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

-- | The index of a lock in a lock set.
newtype LockSetIndex = LockSetIndex Int
  deriving newtype (Enum)

newtype instance VU.MVector s LockSetIndex = MV_LockSetIndex (VP.MVector s Int)

newtype instance VU.Vector LockSetIndex = V_LockSetIndex (VP.Vector Int)

deriving via (VU.UnboxViaPrim Int) instance VGM.MVector VU.MVector LockSetIndex

deriving via (VU.UnboxViaPrim Int) instance VG.Vector VU.Vector LockSetIndex

instance VU.Unbox LockSetIndex

-- | A set of locks with the same level that can be acquired together with 'acquireMany'.
data LockSet set where
  MkLockSet :: set -> VU.Vector LockSetIndex -> LockSet set

-- | Creates a 'LockSet' from a set of locks.
-- All locks must have the same level.
--
-- Locks in a 'LockSet' can be acquired simultaneously using 'acquireMany'.
--
-- Fails if the set contains duplicate locks.
--
-- >>> import LinearLocks.Mutex qualified as Mutex
-- >>> m1 <- Mutex.new 1 "a"
-- >>> m2 <- Mutex.new 1 "b"
-- >>> m3 <- Mutex.new 1 "c"
-- >>> set <- newLockSet (m1, m2, m3)
newLockSet :: forall m set. (IsLockSet set, MonadFail m) => set -> m (LockSet set)
newLockSet set =
  if hasDups
    then fail "LockSet: duplicate locks are not allowed"
    else pure $ MkLockSet set sortedIndices
  where
    (hasDups, sortedIndices) = runST do
      idsAndIndices <- VU.thaw $ VU.fromList $ collectIds set `zip` [LockSetIndex 0 ..]

      -- Sort by lock IDs
      Sort.sortBy (compare `on` fst) idsAndIndices

      -- Check whether this set contains duplicate locks.
      -- NOTE: the vector must already be sorted.
      hasDups <- hasDuplicateIds idsAndIndices

      sortedIndices <- VU.map snd <$> VU.unsafeFreeze idsAndIndices

      pure (hasDups, sortedIndices)

    hasDuplicateIds :: VUM.MVector (VUM.PrimState (ST s)) (LockId, LockSetIndex) -> ST s Bool
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

acquireMany ::
  forall keyLvl lockLvl set.
  (IsLockSet set, lockLvl ~ LockSetLevel set, keyLvl <= lockLvl) =>
  LockKey keyLvl %1 ->
  LockSet set ->
  RIO (LockSetGuard set, LockKey (lockLvl + 1))
acquireMany UnsafeLockKey (MkLockSet set indices) = L.do
  guards <- acquireInOrder indices set
  L.pure (guards, UnsafeLockKey)

class IsLockSet set where
  type LockSetGuard set :: Type
  type LockSetLevel set :: Nat

  collectIds :: set -> [LockId]

  -- | Acquires the locks in the set in the given order.
  -- E.g. `acquireInOrder [1, 3, 2]` will acquire the first lock in the set, then the third, then the second.
  --
  -- Invariants:
  --   * The indices must refer to every lock in the set, without duplicates.
  acquireInOrder :: VU.Vector LockSetIndex -> set -> RIO (LockSetGuard set)

instance
  ( Acquirable l1,
    Acquirable l2,
    Level l1 ~ Level l2
  ) =>
  IsLockSet (l1, l2)
  where
  type LockSetGuard (l1, l2) = (Guard l1, Guard l2)
  type LockSetLevel (l1, l2) = Level l1

  collectIds (l1, l2) = [getId l1, getId l2]

  acquireInOrder indices (l1, l2) = L.do
    guards <- L.execStateT (forM_' indices acquireAt) (Nothing, Nothing)
    case guards of
      (Just g1, Just g2) -> L.pure (g1, g2)
      guards -> releaseAndFail guards missingIndices
    where
      acquireAt :: LockSetIndex -> L.StateT (Maybe (Guard l1), Maybe (Guard l2)) RIO ()
      acquireAt (LockSetIndex index) =
        case index of
          0 -> modifyM \case
            (Nothing, g2) -> L.do
              g1 <- unsafeAcquire l1
              L.pure (Just g1, g2)
            guards -> releaseAndFail guards (dupIndex index)
          1 -> modifyM \case
            (g1, Nothing) -> L.do
              g2 <- unsafeAcquire l2
              L.pure (g1, Just g2)
            guards -> releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (Guard l1), Maybe (Guard l2)) %1 -> String -> RIO x
      releaseAndFail (g1, g2) errMsg = L.do
        releaseMb g1
        releaseMb g2
        failRIO errMsg

instance
  ( Acquirable l1,
    Acquirable l2,
    Acquirable l3,
    Level l1 ~ Level l2,
    Level l1 ~ Level l3
  ) =>
  IsLockSet (l1, l2, l3)
  where
  type LockSetGuard (l1, l2, l3) = (Guard l1, Guard l2, Guard l3)
  type LockSetLevel (l1, l2, l3) = Level l1

  collectIds (l1, l2, l3) = [getId l1, getId l2, getId l3]

  acquireInOrder indices (l1, l2, l3) = L.do
    guards <- L.execStateT (forM_' indices acquireAt) (Nothing, Nothing, Nothing)
    case guards of
      (Just g1, Just g2, Just g3) -> L.pure (g1, g2, g3)
      guards -> releaseAndFail guards missingIndices
    where
      acquireAt :: LockSetIndex -> L.StateT (Maybe (Guard l1), Maybe (Guard l2), Maybe (Guard l3)) RIO ()
      acquireAt (LockSetIndex index) =
        case index of
          0 -> modifyM \case
            (Nothing, g2, g3) -> L.do
              g1 <- unsafeAcquire l1
              L.pure (Just g1, g2, g3)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          1 -> modifyM \case
            (g1, Nothing, g3) -> L.do
              g2 <- unsafeAcquire l2
              L.pure (g1, Just g2, g3)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          2 -> modifyM \case
            (g1, g2, Nothing) -> L.do
              g3 <- unsafeAcquire l3
              L.pure (g1, g2, Just g3)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (Guard l1), Maybe (Guard l2), Maybe (Guard l3)) %1 -> String -> RIO x
      releaseAndFail (g1, g2, g3) errMsg = L.do
        releaseMb g1
        releaseMb g2
        releaseMb g3
        failRIO errMsg

instance
  ( Acquirable l1,
    Acquirable l2,
    Acquirable l3,
    Acquirable l4,
    Level l1 ~ Level l2,
    Level l1 ~ Level l3,
    Level l1 ~ Level l4
  ) =>
  IsLockSet (l1, l2, l3, l4)
  where
  type LockSetGuard (l1, l2, l3, l4) = (Guard l1, Guard l2, Guard l3, Guard l4)
  type LockSetLevel (l1, l2, l3, l4) = Level l1

  collectIds (l1, l2, l3, l4) = [getId l1, getId l2, getId l3, getId l4]

  acquireInOrder indices (l1, l2, l3, l4) = L.do
    guards <- L.execStateT (forM_' indices acquireAt) (Nothing, Nothing, Nothing, Nothing)
    case guards of
      (Just g1, Just g2, Just g3, Just g4) -> L.pure (g1, g2, g3, g4)
      guards -> releaseAndFail guards missingIndices
    where
      acquireAt :: LockSetIndex -> L.StateT (Maybe (Guard l1), Maybe (Guard l2), Maybe (Guard l3), Maybe (Guard l4)) RIO ()
      acquireAt (LockSetIndex index) =
        case index of
          0 -> modifyM \case
            (Nothing, g2, g3, g4) -> L.do
              g1 <- unsafeAcquire l1
              L.pure (Just g1, g2, g3, g4)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          1 -> modifyM \case
            (g1, Nothing, g3, g4) -> L.do
              g2 <- unsafeAcquire l2
              L.pure (g1, Just g2, g3, g4)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          2 -> modifyM \case
            (g1, g2, Nothing, g4) -> L.do
              g3 <- unsafeAcquire l3
              L.pure (g1, g2, Just g3, g4)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          3 -> modifyM \case
            (g1, g2, g3, Nothing) -> L.do
              g4 <- unsafeAcquire l4
              L.pure (g1, g2, g3, Just g4)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (Guard l1), Maybe (Guard l2), Maybe (Guard l3), Maybe (Guard l4)) %1 -> String -> RIO x
      releaseAndFail (g1, g2, g3, g4) errMsg = L.do
        releaseMb g1
        releaseMb g2
        releaseMb g3
        releaseMb g4
        failRIO errMsg

instance
  ( Acquirable l1,
    Acquirable l2,
    Acquirable l3,
    Acquirable l4,
    Acquirable l5,
    Level l1 ~ Level l2,
    Level l1 ~ Level l3,
    Level l1 ~ Level l4,
    Level l1 ~ Level l5
  ) =>
  IsLockSet (l1, l2, l3, l4, l5)
  where
  type LockSetGuard (l1, l2, l3, l4, l5) = (Guard l1, Guard l2, Guard l3, Guard l4, Guard l5)
  type LockSetLevel (l1, l2, l3, l4, l5) = Level l1

  collectIds (l1, l2, l3, l4, l5) = [getId l1, getId l2, getId l3, getId l4, getId l5]

  acquireInOrder indices (l1, l2, l3, l4, l5) = L.do
    guards <- L.execStateT (forM_' indices acquireAt) (Nothing, Nothing, Nothing, Nothing, Nothing)
    case guards of
      (Just g1, Just g2, Just g3, Just g4, Just g5) -> L.pure (g1, g2, g3, g4, g5)
      guards -> releaseAndFail guards missingIndices
    where
      acquireAt :: LockSetIndex -> L.StateT (Maybe (Guard l1), Maybe (Guard l2), Maybe (Guard l3), Maybe (Guard l4), Maybe (Guard l5)) RIO ()
      acquireAt (LockSetIndex index) =
        case index of
          0 -> modifyM \case
            (Nothing, g2, g3, g4, g5) -> L.do
              g1 <- unsafeAcquire l1
              L.pure (Just g1, g2, g3, g4, g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          1 -> modifyM \case
            (g1, Nothing, g3, g4, g5) -> L.do
              g2 <- unsafeAcquire l2
              L.pure (g1, Just g2, g3, g4, g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          2 -> modifyM \case
            (g1, g2, Nothing, g4, g5) -> L.do
              g3 <- unsafeAcquire l3
              L.pure (g1, g2, Just g3, g4, g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          3 -> modifyM \case
            (g1, g2, g3, Nothing, g5) -> L.do
              g4 <- unsafeAcquire l4
              L.pure (g1, g2, g3, Just g4, g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          4 -> modifyM \case
            (g1, g2, g3, g4, Nothing) -> L.do
              g5 <- unsafeAcquire l5
              L.pure (g1, g2, g3, g4, Just g5)
            guards -> L.do
              releaseAndFail guards (dupIndex index)
          _ -> L.lift (failRIO (invalidIndex index))

      releaseAndFail :: (Maybe (Guard l1), Maybe (Guard l2), Maybe (Guard l3), Maybe (Guard l4), Maybe (Guard l5)) %1 -> String -> RIO x
      releaseAndFail (g1, g2, g3, g4, g5) errMsg = L.do
        releaseMb g1
        releaseMb g2
        releaseMb g3
        releaseMb g4
        releaseMb g5
        failRIO errMsg

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

missingIndices :: String
missingIndices = "LockSet: missing indices"

dupIndex :: Int -> String
dupIndex index = "LockSet: duplicate index: " <> show index

invalidIndex :: Int -> String
invalidIndex index = "LockSet: invalid index: " <> show index

releaseMb :: (Releasable g) => Maybe g %1 -> RIO ()
releaseMb = \case
  Nothing -> L.pure ()
  Just guard -> doRelease guard

failRIO :: String -> RIO a
failRIO msg = L.do
  L.liftSystemIO (fail msg)

modifyM :: forall m s. (L.Functor m) => (s %1 -> m s) %1 -> L.StateT s m ()
modifyM f =
  L.StateT \s -> L.do
    f s L.<&> \s' -> ((), s')

-- | A version of 'Data.Vector.Unboxed.forM_' that runs in a linear monad.
forM_' :: (VU.Unbox a, L.Monad m) => VU.Vector a -> (a -> m ()) -> m ()
forM_' vec action = go 0
  where
    go i
      | i >= VU.length vec = L.pure ()
      | otherwise = L.do
          action (vec VU.! i)
          go (i + 1)
