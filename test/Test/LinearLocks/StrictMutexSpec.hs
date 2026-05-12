{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.LinearLocks.StrictMutexSpec where

import Control.Concurrent.MVar qualified as MVar
import Control.Exception (SomeException, throwIO, try)
import Control.Functor.Linear qualified as L
import Control.Monad.IO.Class.Linear qualified as L
import LinearLocks
import LinearLocks.Internal qualified as Internal
import LinearLocks.Internal.StrictMutex qualified as Internal
import LinearLocks.Mutex.Strict qualified as Mutex
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Resource.Linear (RIO)
import Test.Syd

-- | Doctests
--
-- >>> :{
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order :: IO ()
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order = do
-- >>>   m1 <- Mutex.new 2 "hello"
-- >>>   m2 <- Mutex.new 4 "world"
-- >>>   lockScope \key -> L.do
-- >>>     (mg2, key) <- Mutex.acquire key m2
-- >>>     (mg1, key) <- Mutex.acquire key m1
-- >>>     Mutex.release mg1
-- >>>     Mutex.release mg2
-- >>>     dropKeyAndReturn key ()
-- >>> :}
-- ...
-- ... • Cannot satisfy: 5 <= 2
-- ... • In a stmt of a 'do' block: (mg1, key) <- Mutex.acquire key m1
-- ...
spec :: Spec
spec = describe "Strict Mutex" do
  it "read mutex" do
    mutex <- Mutex.new 0 "hello"
    str <- lockScope \key -> L.do
      (mg, key) <- Mutex.acquire key mutex
      (Ur str, mg) <- Mutex.read mg
      Mutex.release mg
      dropKeyAndReturn key str
    str `shouldBe` "hello"

  it "write mutex" do
    mutex <- Mutex.new 0 "hello"
    lockScope \key -> L.do
      (mg, key) <- Mutex.acquire key mutex
      mg <- Mutex.write mg "world"
      Mutex.release mg
      dropKeyAndReturn key ()

    str <- lockScope \key -> L.do
      (mg, key) <- Mutex.acquire key mutex
      (Ur str, mg) <- Mutex.read mg
      Mutex.release mg
      dropKeyAndReturn key str

    str `shouldBe` "world"

    str <- MVar.readMVar mutex.var
    str.unNF `shouldBe` "world"

  it "realeases mvar" do
    mutex <- Mutex.new 0 "hello"
    lockScope \key -> L.do
      (mg, key) <- Mutex.acquire key mutex

      L.liftSystemIO do
        isEmpty <- MVar.isEmptyMVar mutex.var
        isEmpty `shouldBe` True

      Mutex.release mg

      L.liftSystemIO do
        isEmpty <- MVar.isEmptyMVar mutex.var
        isEmpty `shouldBe` False

      dropKeyAndReturn key ()

    isEmpty <- MVar.isEmptyMVar mutex.var
    isEmpty `shouldBe` False

  it "rolls back on exception" do
    mutex <- Mutex.new 0 "hello"
    Left _ <- try @SomeException $ lockScope \key -> L.do
      (mg, key) <- Mutex.acquire key mutex
      mg <- Mutex.write mg "world"
      L.liftSystemIO L.$ throwIO (userError "oops")
      Mutex.release mg
      dropKeyAndReturn key ()

    -- The MVar should have been released, and the original value should have been put back into the MVar.
    mbResult <- MVar.tryTakeMVar mutex.var
    mbResult `shouldBe` Just (Internal.mkNF "hello")

  it "rolls back on imprecise exception" do
    mutex <- Mutex.new 0 "hello"
    Left _ <- try @SomeException $ lockScope \key -> L.do
      (mg, key) <- Mutex.acquire key mutex
      mg <- Mutex.write mg "world"
      error "err"
      Mutex.release mg
      dropKeyAndReturn key ()

    -- The MVar should have been released, and the original value should have been put back into the MVar.
    mbResult <- MVar.tryTakeMVar mutex.var
    mbResult `shouldBe` Just (Internal.mkNF "hello")

  it "new evaluates value to normal form" do
    Mutex.new @[Int] 0 [1, 2, error "oops", 4]
      `shouldThrow` errorCall "oops"

  it "release evaluates value to normal form" do
    mutex <- Mutex.new @[Int] 0 [1]

    logs <- MVar.newMVar @[String] []
    let logMsg :: String -> RIO ()
        logMsg msg = L.liftSystemIO do
          MVar.modifyMVar_ logs \logs -> pure (logs <> [msg])

    let run =
          lockScope \key -> L.do
            (mg, key) <- Mutex.acquire key mutex
            logMsg "ran 'acquire'"
            mg <- Mutex.write mg [1, 2, error "oops", 4]
            logMsg "ran 'write'"
            Mutex.release mg
            logMsg "ran 'release'"
            dropKeyAndReturn key ()

    run `shouldThrow` errorCall "oops"

    -- The exception should be thrown WHILE running `release`.
    -- `write` should NOT throw.
    msgs <- MVar.takeMVar logs
    msgs `shouldBe` ["ran 'acquire'", "ran 'write'"]
