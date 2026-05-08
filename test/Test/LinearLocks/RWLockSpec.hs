{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.LinearLocks.RWLockSpec where

import Control.Concurrent.ReadWriteLock qualified as Conc
import Control.Exception (SomeException, throwIO, try)
import Control.Functor.Linear qualified as L
import Control.Monad (void, when)
import Control.Monad.IO.Class.Linear qualified as L
import Data.IORef qualified as IORef
import LinearLocks
import LinearLocks.Internal.RWLock qualified as Internal
import LinearLocks.RWLock qualified as RWLock
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import "tasty-hunit-compat" Test.Tasty.HUnit

-- | Doctests
--
-- >>> :{
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order :: IO ()
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order = do
-- >>>   m1 <- RWLock.new 2 "hello"
-- >>>   m2 <- RWLock.new 4 "world"
-- >>>   lockScope \key -> L.do
-- >>>     (mg2, key) <- RWLock.acquireRead key m2
-- >>>     (mg1, key) <- RWLock.acquireRead key m1
-- >>>     RWLock.releaseRead mg1
-- >>>     RWLock.releaseRead mg2
-- >>>     L.pure (Ur (), key)
-- >>> :}
-- ...
-- ... • Cannot satisfy: 5 <= 2
-- ... • In a stmt of a 'do' block:
-- ... (mg1, key) <- RWLock.acquireRead key m1
-- ...
unit_read_mutex :: IO ()
unit_read_mutex = do
  rwl <- RWLock.new 0 "hello"
  -- Read in "read mode"
  str <- lockScope \key -> L.do
    (guard, key) <- RWLock.acquireRead key rwl
    (Ur str, guard) <- RWLock.read guard
    RWLock.releaseRead guard
    L.pure (Ur str, key)
  str @?= "hello"

  -- Read in "write mode"
  str <- lockScope \key -> L.do
    (guard, key) <- RWLock.acquireWrite key rwl
    (Ur str, guard) <- RWLock.read guard
    RWLock.releaseWrite guard
    L.pure (Ur str, key)
  str @?= "hello"

unit_write_mutex :: IO ()
unit_write_mutex = do
  rwl <- RWLock.new 0 "hello"

  -- Write in "write mode"
  lockScope \key -> L.do
    (guard, key) <- RWLock.acquireWrite key rwl
    guard <- RWLock.write guard "world"
    RWLock.releaseWrite guard
    L.pure (Ur (), key)

  -- Read in "read mode"
  str <- lockScope \key -> L.do
    (guard, key) <- RWLock.acquireRead key rwl
    (Ur str, guard) <- RWLock.read guard
    RWLock.releaseRead guard
    L.pure (Ur str, key)
  str @?= "world"

  -- Read in "write mode"
  str <- lockScope \key -> L.do
    (guard, key) <- RWLock.acquireWrite key rwl
    (Ur str, guard) <- RWLock.read guard
    RWLock.releaseWrite guard
    L.pure (Ur str, key)
  str @?= "world"

  str <- IORef.readIORef rwl.var
  str @?= "world"

unit_realeases_ioref_in_read_mode :: IO ()
unit_realeases_ioref_in_read_mode = do
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

    L.pure (Ur (), key)

  --  The lock was released, we should be able to acquire it in both "read mode" and "write mode".
  assertCanRead rwl True
  assertCanWrite rwl True

unit_realeases_ioref_in_write_mode :: IO ()
unit_realeases_ioref_in_write_mode = do
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

    L.pure (Ur (), key)

  --  The lock was released, we should be able to acquire it in both "read mode" and "write mode".
  assertCanRead rwl True
  assertCanWrite rwl True

unit_rolls_back_on_exception :: IO ()
unit_rolls_back_on_exception = do
  rwl <- RWLock.new 0 "hello"
  Left _ <- try @SomeException $ lockScope \key -> L.do
    (mg, key) <- RWLock.acquireWrite key rwl
    mg <- RWLock.write mg "world"
    L.liftSystemIO L.$ throwIO (userError "oops")
    RWLock.releaseWrite mg
    L.pure (Ur (), key)

  -- The IORef should have been released, and the original value should have been put back into the IORef.
  assertCanRead rwl True
  assertCanWrite rwl True
  mbResult <- IORef.readIORef rwl.var
  mbResult @?= "hello"

unit_rolls_back_on_imprecise_exception :: IO ()
unit_rolls_back_on_imprecise_exception = do
  rwl <- RWLock.new 0 "hello"
  Left _ <- try @SomeException $ lockScope \key -> L.do
    (mg, key) <- RWLock.acquireWrite key rwl
    mg <- RWLock.write mg "world"
    error "err"
    RWLock.releaseWrite mg
    L.pure (Ur (), key)

  -- The IORef should have been released, and the original value should have been put back into the IORef.
  assertCanRead rwl True
  assertCanWrite rwl True
  mbResult <- IORef.readIORef rwl.var
  mbResult @?= "hello"

unit_new_doesnt_evaluate_value_to_normal_form :: IO ()
unit_new_doesnt_evaluate_value_to_normal_form = do
  -- This should not throw, the "error" thunk should not be evaluated
  void $ RWLock.new @[Int] 0 [1, 2, error "oops", 4]

unit_release_doesnt_evaluate_value_to_normal_form :: IO ()
unit_release_doesnt_evaluate_value_to_normal_form = do
  mutex <- RWLock.new @[Int] 0 [1]

  lockScope \key -> L.do
    (mg, key) <- RWLock.acquireWrite key mutex
    -- This should not throw, the "error" thunk should not be evaluated
    mg <- RWLock.write mg [1, 2, error "oops", 4]
    -- This should not throw
    RWLock.releaseWrite mg
    L.pure (Ur (), key)

assertCanRead :: RWLock.RWLock lvl a -> Bool -> IO ()
assertCanRead rwl expected = do
  canRead <- Conc.tryAcquireRead rwl.lock
  canRead @?= expected
  -- Release the lock if it was acquired.
  when canRead do
    Conc.releaseRead rwl.lock

assertCanWrite :: RWLock.RWLock lvl a -> Bool -> IO ()
assertCanWrite rwl expected = do
  canWrite <- Conc.tryAcquireWrite rwl.lock
  canWrite @?= expected
  -- Release the lock if it was acquired.
  when canWrite do
    Conc.releaseWrite rwl.lock
