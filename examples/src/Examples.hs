{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}

module Examples where

import Control.Functor.Linear qualified as L
import Control.Monad (replicateM_)
import LinearLocks
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import System.IO.Resource.Linear.Internal qualified as Internal (unsafeFromSystemIO)

-- | Acquire 1 lock
--
-- >>> example1
-- hello
example1 :: IO ()
example1 = do
  mutex <- mkMutex 0 "hello"
  lockScope \key -> L.do
    (mg, key) <- lock key mutex
    (Ur str, mg) <- readGuard mg
    Internal.unsafeFromSystemIO (putStrLn str)
    mg <- writeGuard mg "world"
    releaseGuard mg
    L.pure (Ur (), key)

-- This doesn't compile, we can't acquire locks out of order
-- example2 :: IO ()
-- example2 = do
--   m1 <- mkMutex 0 "hello"
--   m2 <- mkMutex 1 "world"
--   lockScope \key -> L.do
--     (mg2, key) <- lock key m2
--     (mg1, key) <- lock key m1
--     releaseGuard mg1
--     releaseGuard mg2
--     L.pure (Ur (), key)

-- | Acquire 2 locks in order
--
-- >>> example3
-- hello world
example3 :: IO ()
example3 = do
  m1 <- mkMutex 0 "hello"
  m2 <- mkMutex 1 "world"
  lockScope \key -> L.do
    (mg1, key) <- lock key m1
    (mg2, key) <- lock key m2
    (Ur str1, mg1) <- readGuard mg1
    (Ur str2, mg2) <- readGuard mg2

    Internal.unsafeFromSystemIO (putStrLn $ str1 <> " " <> str2)

    releaseGuard mg1
    releaseGuard mg2

    L.pure (Ur (), key)

-- | Nested `lockScope`s.
-- This should throw an exception.
--
-- >>> example4
-- *** Exception: NestedLocksScopeException
example4 :: IO ()
example4 = do
  m1 <- mkMutex 0 "hello"
  m2 <- mkMutex 1 "world"
  lockScope \key -> L.do
    (mg2, key) <- lock key m2

    -- Attempt to use nested lockScopes to acquire locks out of order.
    Internal.unsafeFromSystemIO L.$ lockScope \key -> L.do
      (mg1, key) <- lock key m1
      releaseGuard mg1
      L.pure (Ur (), key)

    releaseGuard mg2

    L.pure (Ur (), key)

-- | Lock many locks with the same lvl using a `MutexSet`
--
-- >>> example5
-- hello world
-- hello world
-- hello world
example5 :: IO ()
example5 = do
  m1 <- mkMutex 0 3
  m2 <- mkMutex 0 "hello world"
  mutexSet <- mkMutexSet (m1, m2)
  lockScope \key -> L.do
    ((mg1, mg2), key) <- lockMany key mutexSet
    (Ur count, mg1) <- readGuard mg1
    (Ur str, mg2) <- readGuard mg2

    Internal.unsafeFromSystemIO do
      replicateM_ count $ putStrLn str

    releaseGuard mg1
    releaseGuard mg2

    L.pure (Ur (), key)
