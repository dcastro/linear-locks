{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.LinearLocks.LockSetSpec where

import Control.Functor.Linear qualified as L
import Control.Monad.IO.Class.Linear qualified as L
import Data.Vector.Unboxed qualified as VU
import LinearLocks
import LinearLocks.Internal.LockSet qualified as Internal
import LinearLocks.Internal.Mutex qualified as Internal
import LinearLocks.Mutex qualified as Mutex
import LinearLocks.Mutex.Strict qualified as StrictMutex
import LinearLocks.RWLock qualified as RWLock
import LinearLocks.RWLock.Strict qualified as StrictRWLock
import Prelude.Linear (Ur (..))
import Test.Syd

-- | Doctests
--
-- >>> :{
-- >>> unit_locks_in_a_set_must_have_the_same_level :: IO ()
-- >>> unit_locks_in_a_set_must_have_the_same_level = do
-- >>>   m1 <- Mutex.new 2 "hello"
-- >>>   m2 <- Mutex.new 3 "world"
-- >>>   set <- newLockSet (m1, m2)
-- >>>   pure ()
-- >>> :}
-- ...
-- ... • Couldn't match type ‘2’ with ‘3’
-- ...     arising from a use of ‘newLockSet’
-- ...
spec :: Spec
spec = describe "LockSet" do
  it "read lock set" do
    m1 <- Mutex.new 0 "m1"
    m2 <- Mutex.new 0 "m2"
    m3 <- Mutex.new 0 "m3"
    set <- newLockSet (m1, m2, m3)

    lockScope \key -> L.do
      ((mg1, mg2, mg3), key) <- acquireMany key set

      (Ur str1, mg1) <- Mutex.read mg1
      (Ur str2, mg2) <- Mutex.read mg2
      (Ur str3, mg3) <- Mutex.read mg3

      L.liftSystemIO do
        str1 `shouldBe` "m1"
        str2 `shouldBe` "m2"
        str3 `shouldBe` "m3"

      Mutex.release mg1
      Mutex.release mg2
      Mutex.release mg3
      dropKeyAndReturn key ()

  it "write lock set" do
    m1 <- Mutex.new 0 "m1"
    m2 <- Mutex.new 0 "m2"
    m3 <- Mutex.new 0 "m3"
    set <- newLockSet (m3, m2, m1)

    lockScope \key -> L.do
      ((mg3, mg2, mg1), key) <- acquireMany key set

      mg3 <- Mutex.write mg3 "m3 updated"
      mg2 <- Mutex.write mg2 "m2 updated"
      mg1 <- Mutex.write mg1 "m1 updated"

      Mutex.release mg3
      Mutex.release mg2
      Mutex.release mg1
      dropKeyAndReturn key ()

    lockScope \key -> L.do
      ((mg3, mg2, mg1), key) <- acquireMany key set

      (Ur str3, mg3) <- Mutex.read mg3
      (Ur str2, mg2) <- Mutex.read mg2
      (Ur str1, mg1) <- Mutex.read mg1

      L.liftSystemIO do
        str3 `shouldBe` "m3 updated"
        str2 `shouldBe` "m2 updated"
        str1 `shouldBe` "m1 updated"

      Mutex.release mg3
      Mutex.release mg2
      Mutex.release mg1
      dropKeyAndReturn key ()

  it "assigns unique lock ids" do
    m1 <- Mutex.new 0 ""
    m2 <- Mutex.new 0 ""
    m3 <- Mutex.new 0 ""

    m1.id `shouldNotBe` m2.id
    m2.id `shouldNotBe` m3.id
    m1.id `shouldNotBe` m3.id

  it "throws when lock set contains duplicates" do
    m1 <- Mutex.new 0 ""
    m2 <- Mutex.new 0 ""

    newLockSet (m1, m2, m1) `shouldThrow` \(err :: IOError) -> err == userError "LockSet: duplicate locks are not allowed"

  it "sorts locks deterministically" do
    let sortedIndices :: forall set. LockSet set -> VU.Vector Int
        sortedIndices (Internal.MkLockSet _ indices) = VU.map (\(Internal.LockSetIndex i) -> i) indices

    m1 <- Mutex.new 0 ""
    m2 <- Mutex.new 0 ""
    m3 <- Mutex.new 0 ""

    newLockSet (m1, m2, m3) >>= \set -> sortedIndices set `shouldBe` VU.fromList [0, 1, 2]
    newLockSet (m2, m1, m3) >>= \set -> sortedIndices set `shouldBe` VU.fromList [1, 0, 2]
    newLockSet (m3, m1, m2) >>= \set -> sortedIndices set `shouldBe` VU.fromList [1, 2, 0]
    newLockSet (m1, m3, m2) >>= \set -> sortedIndices set `shouldBe` VU.fromList [0, 2, 1]
    newLockSet (m2, m3, m1) >>= \set -> sortedIndices set `shouldBe` VU.fromList [2, 0, 1]
    newLockSet (m3, m2, m1) >>= \set -> sortedIndices set `shouldBe` VU.fromList [2, 1, 0]

  it "sets can have mixed lock types" do
    m1 <- StrictMutex.new 0 "hello"
    m2 <- Mutex.new @Int 0 99
    m3 <- RWLock.new 0 True
    m4 <- StrictRWLock.new 0 'a'
    set <- newLockSet (m1, m2, m3.asRead, m4.asWrite)

    lockScope \key -> L.do
      ((g1, g2, g3, g4), key) <- acquireMany key set

      (Ur res1, g1) <- StrictMutex.read g1
      (Ur res2, g2) <- Mutex.read g2
      (Ur res3, g3) <- RWLock.read g3
      (Ur res4, g4) <- StrictRWLock.read g4

      L.liftSystemIO do
        res1 `shouldBe` "hello"
        res2 `shouldBe` 99
        res3 `shouldBe` True
        res4 `shouldBe` 'a'

      StrictMutex.release g1
      Mutex.release g2
      RWLock.releaseRead g3
      StrictRWLock.releaseWrite g4
      dropKeyAndReturn key ()
