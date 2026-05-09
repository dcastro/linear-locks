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

-- | Acquire 2 locks in order
--
-- >>> simpleExample
-- hello world
simpleExample :: IO ()
simpleExample = do
  m1 <- Mutex.new 0 "hello"
  m2 <- Mutex.new 1 "world"

  lockScope \key -> Linear.do
    (mg1, key) <- Mutex.acquire key m1
    (mg2, key) <- Mutex.acquire key m2
    (Ur str1, mg1) <- Mutex.read mg1
    (Ur str2, mg2) <- Mutex.read mg2

    Linear.liftSystemIO (putStrLn $ str1 <> " " <> str2)

    Mutex.release mg1
    Mutex.release mg2

    Linear.pure (Ur (), key)

-- This doesn't compile, we can't acquire locks out of order
-- outOfOrder :: IO ()
-- outOfOrder = do
--   m1 <- Mutex.new 0 "hello"
--   m2 <- Mutex.new 1 "world"
--   lockScope \key -> Linear.do
--     (mg2, key) <- Mutex.acquire key m2
--     (mg1, key) <- Mutex.acquire key m1
--     Mutex.release mg1
--     Mutex.release mg2
--     Linear.pure (Ur (), key)

-- | Acquire many locks with the same lvl using a `LockSet`
--
-- >>> lockSets
-- hello world
-- hello world
-- hello world
lockSets :: IO ()
lockSets = do
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
