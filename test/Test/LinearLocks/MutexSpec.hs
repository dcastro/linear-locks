{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.LinearLocks.MutexSpec where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (SomeException, throwIO, try)
import Control.Functor.Linear qualified as L
import Control.Monad (void)
import Control.Monad.IO.Class.Linear qualified as L
import Data.Function ((&))
import GHC.Conc (atomically)
import LinearLocks
import LinearLocks.Internal qualified as Internal
import LinearLocks.Internal.Mutex qualified as Internal
import LinearLocks.Mutex qualified as Mutex
import ListT qualified
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import StmContainers.Set qualified as StmSet
import Test.Hspec.Expectations.Pretty (anyIOException, shouldThrow)
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
-- >>>     dropKeyAndReturn key ()
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
    dropKeyAndReturn key str
  str @?= "hello"

unit_write_mutex :: IO ()
unit_write_mutex = do
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

  str @?= "world"

  str <- MVar.readMVar mutex.var
  str @?= "world"

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

    dropKeyAndReturn key ()

  isEmpty <- MVar.isEmptyMVar mutex.var
  isEmpty @?= False

unit_cant_nest_lockscopes :: IO ()
unit_cant_nest_lockscopes = do
  let run =
        lockScope \key -> L.do
          L.liftSystemIO do
            lockScope \key -> dropKeyAndReturn key ()
          dropKeyAndReturn key ()

  run `shouldThrow` \(_ :: NestedLocksScopeException) -> True

unit_updates_thread_ids :: IO ()
unit_updates_thread_ids = do
  tid <- myThreadId

  getThreadIds >>= \tids -> tids @?= []
  lockScope \key -> L.do
    L.liftSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
    dropKeyAndReturn key ()
  getThreadIds >>= \tids -> tids @?= []

  -- Check that the thread ID is removed even if an exception is thrown.
  let run =
        lockScope \key -> L.do
          L.liftSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
          L.liftSystemIO L.$ throwIO (userError "oops")
          dropKeyAndReturn key ()
  run `shouldThrow` anyIOException
  getThreadIds >>= \tids -> tids @?= []

  -- Check that the thread ID is removed even if when a nested lock scope is attempted
  let run =
        lockScope \key -> L.do
          L.liftSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
          L.liftSystemIO do
            lockScope \key -> dropKeyAndReturn key ()
          dropKeyAndReturn key ()
  run `shouldThrow` \(_ :: NestedLocksScopeException) -> True
  getThreadIds >>= \tids -> tids @?= []

  -- Check that the thread ID is NOT removed if a nested lock scope is caught
  lockScope \key -> L.do
    L.liftSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
    L.liftSystemIO do
      Left _ <- try @SomeException $ lockScope \key -> dropKeyAndReturn key ()
      pure ()
    L.liftSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
    dropKeyAndReturn key ()
  getThreadIds >>= \tids -> tids @?= []
  where
    getThreadIds :: IO [ThreadId]
    getThreadIds =
      Internal.lockScopes & StmSet.listT & ListT.toList & atomically

unit_rolls_back_on_exception :: IO ()
unit_rolls_back_on_exception = do
  mutex <- Mutex.new 0 "hello"
  Left _ <- try @SomeException $ lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex
    mg <- Mutex.write mg "world"
    L.liftSystemIO L.$ throwIO (userError "oops")
    Mutex.release mg
    dropKeyAndReturn key ()

  -- The MVar should have been released, and the original value should have been put back into the MVar.
  mbResult <- MVar.tryTakeMVar mutex.var
  mbResult @?= Just "hello"

unit_rolls_back_on_imprecise_exception :: IO ()
unit_rolls_back_on_imprecise_exception = do
  mutex <- Mutex.new 0 "hello"
  Left _ <- try @SomeException $ lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex
    mg <- Mutex.write mg "world"
    error "err"
    Mutex.release mg
    dropKeyAndReturn key ()

  -- The MVar should have been released, and the original value should have been put back into the MVar.
  mbResult <- MVar.tryTakeMVar mutex.var
  mbResult @?= Just "hello"

unit_new_doesnt_evaluate_value_to_normal_form :: IO ()
unit_new_doesnt_evaluate_value_to_normal_form = do
  -- This should not throw, the "error" thunk should not be evaluated
  void $ Mutex.new @[Int] 0 [1, 2, error "oops", 4]

unit_release_doesnt_evaluate_value_to_normal_form :: IO ()
unit_release_doesnt_evaluate_value_to_normal_form = do
  mutex <- Mutex.new @[Int] 0 [1]

  lockScope \key -> L.do
    (mg, key) <- Mutex.acquire key mutex
    -- This should not throw, the "error" thunk should not be evaluated
    mg <- Mutex.write mg [1, 2, error "oops", 4]
    -- This should not throw
    Mutex.release mg
    dropKeyAndReturn key ()
