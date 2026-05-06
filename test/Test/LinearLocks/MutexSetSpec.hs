{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.LinearLocks.MutexSetSpec where

import Control.Functor.Linear qualified as L
import Control.Monad.IO.Class.Linear qualified as L
import Data.Vector.Unboxed qualified as VU
import LinearLocks
import LinearLocks.Internal.Mutex qualified as Internal
import LinearLocks.Internal.MutexSet qualified as Internal
import LinearLocks.Mutex qualified as Mutex
import LinearLocks.Mutex.Strict qualified as StrictMutex
import Prelude.Linear (Ur (..))
import Test.Hspec.Expectations.Pretty (shouldNotBe, shouldThrow)
import "tasty-hunit-compat" Test.Tasty.HUnit

-- | Doctests
--
-- >>> :{
-- >>> unit_mutexes_in_a_set_must_have_the_same_level :: IO ()
-- >>> unit_mutexes_in_a_set_must_have_the_same_level = do
-- >>>   m1 <- Mutex.new 2 "hello"
-- >>>   m2 <- Mutex.new 3 "world"
-- >>>   set <- newMutexSet (m1, m2)
-- >>>   pure ()
-- >>> :}
-- ...
-- ... • Couldn't match type ‘2’ with ‘3’
-- ...     arising from a use of ‘newMutexSet’
-- ...
unit_read_mutex_set :: IO ()
unit_read_mutex_set = do
  m1 <- Mutex.new 0 "m1"
  m2 <- Mutex.new 0 "m2"
  m3 <- Mutex.new 0 "m3"
  set <- newMutexSet (m1, m2, m3)

  lockScope \key -> L.do
    ((mg1, mg2, mg3), key) <- lockMany key set

    (Ur str1, mg1) <- Mutex.read mg1
    (Ur str2, mg2) <- Mutex.read mg2
    (Ur str3, mg3) <- Mutex.read mg3

    L.liftSystemIO do
      str1 @?= "m1"
      str2 @?= "m2"
      str3 @?= "m3"

    Mutex.release mg1
    Mutex.release mg2
    Mutex.release mg3
    L.pure (Ur (), key)

unit_write_mutex_set :: IO ()
unit_write_mutex_set = do
  m1 <- Mutex.new 0 "m1"
  m2 <- Mutex.new 0 "m2"
  m3 <- Mutex.new 0 "m3"
  set <- newMutexSet (m3, m2, m1)

  lockScope \key -> L.do
    ((mg3, mg2, mg1), key) <- lockMany key set

    mg3 <- Mutex.write mg3 "m3 updated"
    mg2 <- Mutex.write mg2 "m2 updated"
    mg1 <- Mutex.write mg1 "m1 updated"

    Mutex.release mg3
    Mutex.release mg2
    Mutex.release mg1
    L.pure (Ur (), key)

  lockScope \key -> L.do
    ((mg3, mg2, mg1), key) <- lockMany key set

    (Ur str3, mg3) <- Mutex.read mg3
    (Ur str2, mg2) <- Mutex.read mg2
    (Ur str1, mg1) <- Mutex.read mg1

    L.liftSystemIO do
      str3 @?= "m3 updated"
      str2 @?= "m2 updated"
      str1 @?= "m1 updated"

    Mutex.release mg3
    Mutex.release mg2
    Mutex.release mg1
    L.pure (Ur (), key)

unit_assigns_unique_mutex_ids :: IO ()
unit_assigns_unique_mutex_ids = do
  m1 <- Mutex.new 0 ""
  m2 <- Mutex.new 0 ""
  m3 <- Mutex.new 0 ""

  m1.id `shouldNotBe` m2.id
  m2.id `shouldNotBe` m3.id
  m1.id `shouldNotBe` m3.id

unit_throws_when_mutex_set_contains_duplicates :: IO ()
unit_throws_when_mutex_set_contains_duplicates = do
  m1 <- Mutex.new 0 ""
  m2 <- Mutex.new 0 ""

  newMutexSet (m1, m2, m1) `shouldThrow` \(err :: IOError) -> err == userError "MutexSet: duplicate mutexes are not allowed"

unit_sorts_mutexes_deterministically :: IO ()
unit_sorts_mutexes_deterministically = do
  m1 <- Mutex.new 0 ""
  m2 <- Mutex.new 0 ""
  m3 <- Mutex.new 0 ""

  newMutexSet (m1, m2, m3) >>= \set -> sortedIndices set @?= VU.fromList [0, 1, 2]
  newMutexSet (m2, m1, m3) >>= \set -> sortedIndices set @?= VU.fromList [1, 0, 2]
  newMutexSet (m3, m1, m2) >>= \set -> sortedIndices set @?= VU.fromList [1, 2, 0]
  newMutexSet (m1, m3, m2) >>= \set -> sortedIndices set @?= VU.fromList [0, 2, 1]
  newMutexSet (m2, m3, m1) >>= \set -> sortedIndices set @?= VU.fromList [2, 0, 1]
  newMutexSet (m3, m2, m1) >>= \set -> sortedIndices set @?= VU.fromList [2, 1, 0]
  where
    sortedIndices :: forall set. MutexSet set -> VU.Vector Int
    sortedIndices (Internal.MkMutexSet _ indices) = VU.map (\(Internal.MutexSetIndex i) -> i) indices

unit_sets_can_have_mixed_mutex_types :: IO ()
unit_sets_can_have_mixed_mutex_types = do
  m1 <- StrictMutex.new 0 "hello"
  m2 <- Mutex.new @Int 0 99
  set <- newMutexSet (m1, m2)

  lockScope \key -> L.do
    ((mg1, mg2), key) <- lockMany key set

    (Ur res1, mg1) <- StrictMutex.read mg1
    (Ur res2, mg2) <- Mutex.read mg2

    L.liftSystemIO do
      res1 @?= "hello"
      res2 @?= 99

    StrictMutex.release mg1
    Mutex.release mg2
    L.pure (Ur (), key)
