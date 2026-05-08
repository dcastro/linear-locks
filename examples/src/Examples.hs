{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}

module Examples where

import Control.Functor.Linear qualified as Linear
import Control.Monad (replicateM_)
import Control.Monad.IO.Class.Linear qualified as Linear
import LinearLocks
import LinearLocks.Mutex qualified as Mutex
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as Linear hiding (IO)

-- | Acquire 1 lock
--
-- >>> example1
-- hello
example1 :: IO ()
example1 = do
  mutex <- Mutex.new 0 "hello"
  lockScope \key -> Linear.do
    (mg, key) <- acquire key mutex
    (Ur str, mg) <- Mutex.read mg
    Linear.liftSystemIO (putStrLn str)
    mg <- Mutex.write mg "world"
    Mutex.release mg
    Linear.pure (Ur (), key)

-- This doesn't compile, we can't acquire locks out of order
-- example2 :: IO ()
-- example2 = do
--   m1 <- Mutex.new 0 "hello"
--   m2 <- Mutex.new 1 "world"
--   lockScope \key -> Linear.do
--     (mg2, key) <- acquire key m2
--     (mg1, key) <- acquire key m1
--     Mutex.release mg1
--     Mutex.release mg2
--     Linear.pure (Ur (), key)

-- | Acquire 2 locks in order
--
-- >>> example3
-- hello world
example3 :: IO ()
example3 = do
  m1 <- Mutex.new 0 "hello"
  m2 <- Mutex.new 1 "world"
  lockScope \key -> Linear.do
    (mg1, key) <- acquire key m1
    (mg2, key) <- acquire key m2
    (Ur str1, mg1) <- Mutex.read mg1
    (Ur str2, mg2) <- Mutex.read mg2

    Linear.liftSystemIO (putStrLn $ str1 <> " " <> str2)

    Mutex.release mg1
    Mutex.release mg2

    Linear.pure (Ur (), key)

-- | Nested `lockScope`s.
-- This should throw an exception.
--
-- >>> example4
-- *** Exception: NestedLocksScopeException
example4 :: IO ()
example4 = do
  m1 <- Mutex.new 0 "hello"
  m2 <- Mutex.new 1 "world"
  lockScope \key -> Linear.do
    (mg2, key) <- acquire key m2

    -- Attempt to use nested lockScopes to acquire locks out of order.
    Linear.liftSystemIO Linear.$ lockScope \key -> Linear.do
      (mg1, key) <- acquire key m1
      Mutex.release mg1
      Linear.pure (Ur (), key)

    Mutex.release mg2

    Linear.pure (Ur (), key)

-- | Acquire many locks with the same lvl using a `LockSet`
--
-- >>> example5
-- hello world
-- hello world
-- hello world
example5 :: IO ()
example5 = do
  m1 <- Mutex.new 0 3
  m2 <- Mutex.new 0 "hello world"
  mutexSet <- newLockSet (m1, m2)
  lockScope \key -> Linear.do
    ((mg1, mg2), key) <- acquireMany key mutexSet
    (Ur count, mg1) <- Mutex.read mg1
    (Ur str, mg2) <- Mutex.read mg2

    Linear.liftSystemIO do
      replicateM_ count $ putStrLn str

    Mutex.release mg1
    Mutex.release mg2

    Linear.pure (Ur (), key)
