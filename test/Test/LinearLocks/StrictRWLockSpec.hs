{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.LinearLocks.StrictRWLockSpec where

import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.ReadWriteLock qualified as Conc
import Control.Exception (SomeException, throwIO, try)
import Control.Functor.Linear qualified as L
import Control.Monad (when)
import Control.Monad.IO.Class.Linear qualified as L
import Data.IORef qualified as IORef
import LinearLocks
import LinearLocks.Internal qualified as Internal
import LinearLocks.Internal.StrictRWLock qualified as Internal
import LinearLocks.RWLock.Strict qualified as RWLock
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Resource.Linear (RIO)
import Test.Syd

-- | Doctests
--
-- >>> :{
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order :: IO ()
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order = do
-- >>>   m1 <- RWLock.new 2 "hello"
-- >>>   m2 <- RWLock.new 4 "world"
-- >>>   lockScope \key -> L.do
-- >>>     (g2, key) <- RWLock.acquireRead key m2
-- >>>     (g1, key) <- RWLock.acquireRead key m1
-- >>>     RWLock.releaseRead g1
-- >>>     RWLock.releaseRead g2
-- >>>     dropKeyAndReturn key ()
-- >>> :}
-- ...
-- ... • Cannot satisfy: 5 <= 2
-- ... • In a stmt of a 'do' block: (g1, key) <- RWLock.acquireRead key m1
-- ...
spec :: Spec
spec = describe "Strict RWLock" do
  it "read mutex" do
    rwl <- RWLock.new 0 "hello"
    -- Read in "read mode"
    str <- lockScope \key -> L.do
      (guard, key) <- RWLock.acquireRead key rwl
      (Ur str, guard) <- RWLock.read guard
      RWLock.releaseRead guard
      dropKeyAndReturn key str
    str `shouldBe` "hello"

    -- Read in "write mode"
    str <- lockScope \key -> L.do
      (guard, key) <- RWLock.acquireWrite key rwl
      (Ur str, guard) <- RWLock.read guard
      RWLock.releaseWrite guard
      dropKeyAndReturn key str
    str `shouldBe` "hello"

  it "write mutex" do
    rwl <- RWLock.new 0 "hello"

    -- Write in "write mode"
    lockScope \key -> L.do
      (guard, key) <- RWLock.acquireWrite key rwl
      guard <- RWLock.write guard "world"
      RWLock.releaseWrite guard
      dropKeyAndReturn key ()

    -- Read in "read mode"
    str <- lockScope \key -> L.do
      (guard, key) <- RWLock.acquireRead key rwl
      (Ur str, guard) <- RWLock.read guard
      RWLock.releaseRead guard
      dropKeyAndReturn key str
    str `shouldBe` "world"

    -- Read in "write mode"
    str <- lockScope \key -> L.do
      (guard, key) <- RWLock.acquireWrite key rwl
      (Ur str, guard) <- RWLock.read guard
      RWLock.releaseWrite guard
      dropKeyAndReturn key str
    str `shouldBe` "world"

    str <- IORef.readIORef rwl.var
    str.unNF `shouldBe` "world"

  it "realeases ioref in read mode" do
    rwl <- RWLock.new 0 "hello"
    lockScope \key -> L.do
      (mg, key) <- RWLock.acquireRead key rwl

      -- If the lock was acquired in "read mode",
      -- we shouldn't be able to acquire it again in "write mode",
      -- but we should be able to acquire it in "read mode".
      L.liftSystemIO do
        assertCanRead rwl True
        assertCanWrite rwl False

      RWLock.releaseRead mg

      --  The lock was released, we should be able to acquire it in both "read mode" and "write mode".
      L.liftSystemIO do
        assertCanRead rwl True
        assertCanWrite rwl True

      dropKeyAndReturn key ()

    --  The lock was released, we should be able to acquire it in both "read mode" and "write mode".
    assertCanRead rwl True
    assertCanWrite rwl True

  it "realeases ioref in write mode" do
    rwl <- RWLock.new 0 "hello"
    lockScope \key -> L.do
      (mg, key) <- RWLock.acquireWrite key rwl

      -- If the lock was acquired in "write mode",
      -- we shouldn't be able to acquire it again in "write mode" or "read mode".
      L.liftSystemIO do
        assertCanRead rwl False
        assertCanWrite rwl False

      RWLock.releaseWrite mg

      --  The lock was released, we should be able to acquire it in both "read mode" and "write mode".
      L.liftSystemIO do
        assertCanRead rwl True
        assertCanWrite rwl True

      dropKeyAndReturn key ()

    --  The lock was released, we should be able to acquire it in both "read mode" and "write mode".
    assertCanRead rwl True
    assertCanWrite rwl True

  it "rolls back on exception" do
    rwl <- RWLock.new 0 "hello"
    Left _ <- try @SomeException $ lockScope \key -> L.do
      (mg, key) <- RWLock.acquireWrite key rwl
      mg <- RWLock.write mg "world"
      L.liftSystemIO L.$ throwIO (userError "oops")
      RWLock.releaseWrite mg
      dropKeyAndReturn key ()

    -- The IORef should have been released, and the original value should have been put back into the IORef.
    assertCanRead rwl True
    assertCanWrite rwl True
    mbResult <- IORef.readIORef rwl.var
    mbResult.unNF `shouldBe` "hello"

  it "rolls back on imprecise exception" do
    rwl <- RWLock.new 0 "hello"
    Left _ <- try @SomeException $ lockScope \key -> L.do
      (mg, key) <- RWLock.acquireWrite key rwl
      mg <- RWLock.write mg "world"
      error "err"
      RWLock.releaseWrite mg
      dropKeyAndReturn key ()

    -- The IORef should have been released, and the original value should have been put back into the IORef.
    assertCanRead rwl True
    assertCanWrite rwl True
    mbResult <- IORef.readIORef rwl.var
    mbResult.unNF `shouldBe` "hello"

  it "new evaluates value to normal form" do
    RWLock.new @[Int] 0 [1, 2, error "oops", 4]
      `shouldThrow` errorCall "oops"

  it "release evaluates value to normal form" do
    mutex <- RWLock.new @[Int] 0 [1]

    logs <- MVar.newMVar @[String] []
    let logMsg :: String -> RIO ()
        logMsg msg = L.liftSystemIO do
          MVar.modifyMVar_ logs \logs -> pure (logs <> [msg])

    let run =
          lockScope \key -> L.do
            (mg, key) <- RWLock.acquireWrite key mutex
            logMsg "ran 'acquire'"
            mg <- RWLock.write mg [1, 2, error "oops", 4]
            logMsg "ran 'write'"
            RWLock.releaseWrite mg
            logMsg "ran 'release'"
            dropKeyAndReturn key ()

    run `shouldThrow` errorCall "oops"

    -- The exception should be thrown WHILE running `release`.
    -- `write` should NOT throw.
    msgs <- MVar.takeMVar logs
    msgs `shouldBe` ["ran 'acquire'", "ran 'write'"]

assertCanRead :: RWLock.RWLock lvl a -> Bool -> IO ()
assertCanRead rwl expected = do
  canRead <- Conc.tryAcquireRead rwl.lock
  canRead `shouldBe` expected
  -- Release the lock if it was acquired.
  when canRead do
    Conc.releaseRead rwl.lock

assertCanWrite :: RWLock.RWLock lvl a -> Bool -> IO ()
assertCanWrite rwl expected = do
  canWrite <- Conc.tryAcquireWrite rwl.lock
  canWrite `shouldBe` expected
  -- Release the lock if it was acquired.
  when canWrite do
    Conc.releaseWrite rwl.lock
