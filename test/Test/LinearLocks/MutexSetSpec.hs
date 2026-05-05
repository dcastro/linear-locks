{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.LinearLocks.MutexSetSpec where

import Control.Functor.Linear qualified as L
import Data.Vector.Unboxed qualified as VU
import LinearLocks
import LinearLocks.Internal.MutexSet qualified as Internal
import LinearLocks.Internal.Mutex qualified as Internal
import Prelude.Linear (Ur (..))
import System.IO.Resource.Linear.Internal qualified as Internal (unsafeFromSystemIO)
import Test.Hspec.Expectations.Pretty (shouldNotBe, shouldThrow)
import "tasty-hunit-compat" Test.Tasty.HUnit

-- | Doctests
--
-- >>> :{
-- >>> unit_mutexes_in_a_set_must_have_the_same_level :: IO ()
-- >>> unit_mutexes_in_a_set_must_have_the_same_level = do
-- >>>   m1 <- mkMutex 2 "hello"
-- >>>   m2 <- mkMutex 3 "world"
-- >>>   set <- mkMutexSet (m1, m2)
-- >>>   lockScope \key -> L.do
-- >>>     ((mg1, mg2), key) <- lockMany key set
-- >>>     releaseGuard mg1
-- >>>     releaseGuard mg2
-- >>>     L.pure (Ur (), key)
-- >>> :}
-- ...
-- ... • Cannot satisfy: 0 <= Internal.MutexSetLevel
-- ...
unit_read_mutex_set :: IO ()
unit_read_mutex_set = do
  m1 <- mkMutex 0 "m1"
  m2 <- mkMutex 0 "m2"
  m3 <- mkMutex 0 "m3"
  set <- mkMutexSet (m1, m2, m3)

  lockScope \key -> L.do
    ((mg1, mg2, mg3), key) <- lockMany key set

    (Ur str1, mg1) <- readGuard mg1
    (Ur str2, mg2) <- readGuard mg2
    (Ur str3, mg3) <- readGuard mg3

    Internal.unsafeFromSystemIO do
      str1 @?= "m1"
      str2 @?= "m2"
      str3 @?= "m3"

    releaseGuard mg1
    releaseGuard mg2
    releaseGuard mg3
    L.pure (Ur (), key)

unit_write_mutex_set :: IO ()
unit_write_mutex_set = do
  m1 <- mkMutex 0 "m1"
  m2 <- mkMutex 0 "m2"
  m3 <- mkMutex 0 "m3"
  set <- mkMutexSet (m3, m2, m1)

  lockScope \key -> L.do
    ((mg3, mg2, mg1), key) <- lockMany key set

    mg3 <- writeGuard mg3 "m3 updated"
    mg2 <- writeGuard mg2 "m2 updated"
    mg1 <- writeGuard mg1 "m1 updated"

    releaseGuard mg3
    releaseGuard mg2
    releaseGuard mg1
    L.pure (Ur (), key)

  lockScope \key -> L.do
    ((mg3, mg2, mg1), key) <- lockMany key set

    (Ur str3, mg3) <- readGuard mg3
    (Ur str2, mg2) <- readGuard mg2
    (Ur str1, mg1) <- readGuard mg1

    Internal.unsafeFromSystemIO do
      str3 @?= "m3 updated"
      str2 @?= "m2 updated"
      str1 @?= "m1 updated"

    releaseGuard mg3
    releaseGuard mg2
    releaseGuard mg1
    L.pure (Ur (), key)

unit_assigns_unique_mutex_ids :: IO ()
unit_assigns_unique_mutex_ids = do
  m1 <- mkMutex 0 ""
  m2 <- mkMutex 0 ""
  m3 <- mkMutex 0 ""

  m1.id `shouldNotBe` m2.id
  m2.id `shouldNotBe` m3.id
  m1.id `shouldNotBe` m3.id

unit_throws_when_mutex_set_contains_duplicates :: IO ()
unit_throws_when_mutex_set_contains_duplicates = do
  m1 <- mkMutex 0 ""
  m2 <- mkMutex 0 ""

  mkMutexSet (m1, m2, m1) `shouldThrow` \(err :: IOError) -> err == userError "MutexSet: duplicate mutexes are not allowed"

unit_sorts_mutexes_deterministically :: IO ()
unit_sorts_mutexes_deterministically = do
  m1 <- mkMutex 0 ""
  m2 <- mkMutex 0 ""
  m3 <- mkMutex 0 ""

  mkMutexSet (m1, m2, m3) >>= \set -> sortedIndices set @?= VU.fromList [0, 1, 2]
  mkMutexSet (m2, m1, m3) >>= \set -> sortedIndices set @?= VU.fromList [1, 0, 2]
  mkMutexSet (m3, m1, m2) >>= \set -> sortedIndices set @?= VU.fromList [1, 2, 0]
  mkMutexSet (m1, m3, m2) >>= \set -> sortedIndices set @?= VU.fromList [0, 2, 1]
  mkMutexSet (m2, m3, m1) >>= \set -> sortedIndices set @?= VU.fromList [2, 0, 1]
  mkMutexSet (m3, m2, m1) >>= \set -> sortedIndices set @?= VU.fromList [2, 1, 0]
  where
    sortedIndices :: forall set. MutexSet set -> VU.Vector Int
    sortedIndices (Internal.MkMutexSet _ indices) = VU.map (\(Internal.MutexSetIndex i) -> i) indices
