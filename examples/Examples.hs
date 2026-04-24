{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Examples where

import LinearLocks
import Control.Functor.Linear qualified as L
import System.IO.Resource.Linear.Internal qualified as Internal
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)

-- Acquire 1 lock
example1 :: IO ()
example1 = do
  mutex <- mkMutex @0 "hello"
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
--   m1 <- mkMutex @0 "hello"
--   m2 <- mkMutex @1 "world"
--   lockScope \key -> L.do
--     (mg2, key) <- lock key m2
--     (mg1, key) <- lock key m1
--     releaseGuard mg1
--     releaseGuard mg2
--     L.pure (Ur (), key)

-- Acquire 2 locks in order
example3 :: IO ()
example3 = do
  m1 <- mkMutex @0 "hello"
  m2 <- mkMutex @1 "world"
  lockScope \key -> L.do
    (mg1, key) <- lock key m1
    (mg2, key) <- lock key m2
    (Ur str1, mg1) <- readGuard mg1
    (Ur str2, mg2) <- readGuard mg2

    Internal.unsafeFromSystemIO (putStrLn $ str1 <> " " <> str2)

    releaseGuard mg1
    releaseGuard mg2

    L.pure (Ur (), key)

-- Nested `lockScope`s.
-- This should throw an exception.
example4 :: IO ()
example4 = do
  m1 <- mkMutex @0 "hello"
  m2 <- mkMutex @1 "world"
  lockScope \key -> L.do
    (mg2, key) <- lock key m2

    -- Attempt to use nested lockScopes to acquire locks out of order.
    Internal.unsafeFromSystemIO L.$ lockScope \key -> L.do
      (mg1, key) <- lock key m1
      releaseGuard mg1
      L.pure (Ur (), key)

    releaseGuard mg2

    L.pure (Ur (), key)
