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
import Data.Function ((&))
import GHC.Conc (atomically)
import LinearLocks
import LinearLocks.Internal qualified as Internal
import ListT qualified
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import StmContainers.Set qualified as StmSet
import System.IO.Resource.Linear.Internal qualified as Internal (unsafeFromSystemIO)
import Test.Hspec.Expectations.Pretty (anyIOException, shouldThrow)
import "tasty-hunit-compat" Test.Tasty.HUnit

-- | Doctests
--
-- >>> :{
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order :: IO ()
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order = do
-- >>>   m1 <- mkMutex 2 "hello"
-- >>>   m2 <- mkMutex 4 "world"
-- >>>   lockScope \key -> L.do
-- >>>     (mg2, key) <- lock key m2
-- >>>     (mg1, key) <- lock key m1
-- >>>     releaseGuard mg1
-- >>>     releaseGuard mg2
-- >>>     L.pure (Ur (), key)
-- >>> :}
-- ...
-- ... • Cannot satisfy: 5 <= 2
-- ... • In a stmt of a 'do' block: (mg1, key) <- lock key m1
-- ...
unit_read_mutex :: IO ()
unit_read_mutex = do
  mutex <- mkMutex 0 "hello"
  str <- lockScope \key -> L.do
    (mg, key) <- lock key mutex
    (Ur str, mg) <- readGuard mg
    releaseGuard mg
    L.pure (Ur str, key)
  str @?= "hello"

unit_write_mutex :: IO ()
unit_write_mutex = do
  mutex <- mkMutex 0 "hello"
  lockScope \key -> L.do
    (mg, key) <- lock key mutex
    mg <- writeGuard mg "world"
    releaseGuard mg
    L.pure (Ur (), key)

  str <- lockScope \key -> L.do
    (mg, key) <- lock key mutex
    (Ur str, mg) <- readGuard mg
    releaseGuard mg
    L.pure (Ur str, key)

  str @?= "world"

  str <- MVar.readMVar mutex.var
  str @?= "world"

unit_realeases_mvar :: IO ()
unit_realeases_mvar = do
  mutex <- mkMutex 0 "hello"
  lockScope \key -> L.do
    (mg, key) <- lock key mutex

    Internal.unsafeFromSystemIO do
      isEmpty <- MVar.isEmptyMVar mutex.var
      isEmpty @?= True

    releaseGuard mg

    Internal.unsafeFromSystemIO do
      isEmpty <- MVar.isEmptyMVar mutex.var
      isEmpty @?= False

    L.pure (Ur (), key)

  isEmpty <- MVar.isEmptyMVar mutex.var
  isEmpty @?= False

unit_cant_nest_lockscopes :: IO ()
unit_cant_nest_lockscopes = do
  let run =
        lockScope \key -> L.do
          Internal.unsafeFromSystemIO do
            lockScope \key -> L.pure (Ur (), key)
          L.pure (Ur (), key)

  run `shouldThrow` \(_ :: NestedLocksScopeException) -> True

unit_updates_thread_ids :: IO ()
unit_updates_thread_ids = do
  tid <- myThreadId

  getThreadIds >>= \tids -> tids @?= []
  lockScope \key -> L.do
    Internal.unsafeFromSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
    L.pure (Ur (), key)
  getThreadIds >>= \tids -> tids @?= []

  -- Check that the thread ID is removed even if an exception is thrown.
  let run =
        lockScope \key -> L.do
          Internal.unsafeFromSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
          Internal.unsafeFromSystemIO L.$ throwIO (userError "oops")
          L.pure (Ur (), key)
  run `shouldThrow` anyIOException
  getThreadIds >>= \tids -> tids @?= []

  -- Check that the thread ID is removed even if when a nested lock scope is attempted
  let run =
        lockScope \key -> L.do
          Internal.unsafeFromSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
          Internal.unsafeFromSystemIO do
            lockScope \key -> L.pure (Ur (), key)
          L.pure (Ur (), key)
  run `shouldThrow` \(_ :: NestedLocksScopeException) -> True
  getThreadIds >>= \tids -> tids @?= []

  -- Check that the thread ID is NOT removed if a nested lock scope is caught
  lockScope \key -> L.do
    Internal.unsafeFromSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
    Internal.unsafeFromSystemIO do
      Left _ <- try @SomeException $ lockScope \key -> L.pure (Ur (), key)
      pure ()
    Internal.unsafeFromSystemIO L.$ getThreadIds >>= \tids -> tids @?= [tid]
    L.pure (Ur (), key)
  getThreadIds >>= \tids -> tids @?= []
  where
    getThreadIds :: IO [ThreadId]
    getThreadIds =
      Internal.lockScopes & StmSet.listT & ListT.toList & atomically

unit_rolls_back_on_exception :: IO ()
unit_rolls_back_on_exception = do
  mutex <- mkMutex 0 "hello"
  Left _ <- try @SomeException $ lockScope \key -> L.do
    (mg, key) <- lock key mutex
    mg <- writeGuard mg "world"
    Internal.unsafeFromSystemIO L.$ throwIO (userError "oops")
    releaseGuard mg
    L.pure (Ur (), key)

  -- The MVar should have been released, and the original value should have been put back into the MVar.
  mbResult <- MVar.tryTakeMVar mutex.var
  mbResult @?= Just "hello"

unit_rolls_back_on_imprecise_exception :: IO ()
unit_rolls_back_on_imprecise_exception = do
  mutex <- mkMutex 0 "hello"
  Left _ <- try @SomeException $ lockScope \key -> L.do
    (mg, key) <- lock key mutex
    mg <- writeGuard mg "world"
    error "err"
    releaseGuard mg
    L.pure (Ur (), key)

  -- The MVar should have been released, and the original value should have been put back into the MVar.
  mbResult <- MVar.tryTakeMVar mutex.var
  mbResult @?= Just "hello"
