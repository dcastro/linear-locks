{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
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
import Test.Hspec.Expectations.Pretty (errorCall, shouldThrow)
import "tasty-hunit-compat" Test.Tasty.HUnit

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
-- >>>     L.pure (Ur (), key)
-- >>> :}
-- ...
-- ... • Cannot satisfy: 5 <= 2
-- ... • In a stmt of a 'do' block: (mg1, key) <- Mutex.acquire key m1
-- ...
unit_read_mutex :: IO ()
unit_read_mutex = do
  mutex <- Mutex.new 0 "hello"
  str <- lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex
    (Ur str, mg) <- Mutex.read mg
    Mutex.release mg
    L.pure (Ur str, key)
  str @?= "hello"

unit_write_mutex :: IO ()
unit_write_mutex = do
  mutex <- Mutex.new 0 "hello"
  lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex
    mg <- Mutex.write mg "world"
    Mutex.release mg
    L.pure (Ur (), key)

  str <- lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex
    (Ur str, mg) <- Mutex.read mg
    Mutex.release mg
    L.pure (Ur str, key)

  str @?= "world"

  str <- MVar.readMVar mutex.var
  str.unNF @?= "world"

unit_realeases_mvar :: IO ()
unit_realeases_mvar = do
  mutex <- Mutex.new 0 "hello"
  lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex

    L.liftSystemIO do
      isEmpty <- MVar.isEmptyMVar mutex.var
      isEmpty @?= True

    Mutex.release mg

    L.liftSystemIO do
      isEmpty <- MVar.isEmptyMVar mutex.var
      isEmpty @?= False

    L.pure (Ur (), key)

  isEmpty <- MVar.isEmptyMVar mutex.var
  isEmpty @?= False

unit_rolls_back_on_exception :: IO ()
unit_rolls_back_on_exception = do
  mutex <- Mutex.new 0 "hello"
  Left _ <- try @SomeException $ lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex
    mg <- Mutex.write mg "world"
    L.liftSystemIO L.$ throwIO (userError "oops")
    Mutex.release mg
    L.pure (Ur (), key)

  -- The MVar should have been released, and the original value should have been put back into the MVar.
  mbResult <- MVar.tryTakeMVar mutex.var
  mbResult @?= Just (Internal.mkNF "hello")

unit_rolls_back_on_imprecise_exception :: IO ()
unit_rolls_back_on_imprecise_exception = do
  mutex <- Mutex.new 0 "hello"
  Left _ <- try @SomeException $ lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex
    mg <- Mutex.write mg "world"
    error "err"
    Mutex.release mg
    L.pure (Ur (), key)

  -- The MVar should have been released, and the original value should have been put back into the MVar.
  mbResult <- MVar.tryTakeMVar mutex.var
  mbResult @?= Just (Internal.mkNF "hello")

unit_new_evaluates_value_to_normal_form :: IO ()
unit_new_evaluates_value_to_normal_form = do
  Mutex.new @[Int] 0 [1, 2, error "oops", 4]
    `shouldThrow` errorCall "oops"

unit_release_evaluates_value_to_normal_form :: IO ()
unit_release_evaluates_value_to_normal_form = do
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
          L.pure (Ur (), key)

  run `shouldThrow` errorCall "oops"

  -- The exception should be thrown WHILE running `release`.
  -- `write` should NOT throw.
  msgs <- MVar.takeMVar logs
  msgs @?= ["ran 'acquire'", "ran 'write'"]
