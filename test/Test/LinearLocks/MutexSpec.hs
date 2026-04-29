{-# LANGUAGE QualifiedDo #-}

module Test.LinearLocks.MutexSpec where

import Control.Functor.Linear qualified as L
import LinearLocks
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import Test.Tasty.HUnit

unit_read_mutex :: IO ()
unit_read_mutex = do
  mutex <- mkMutex 0 "hello"
  str <- lockScope \key -> L.do
    (mg, key) <- lock key mutex
    (Ur str, mg) <- readGuard mg
    releaseGuard mg
    L.pure (Ur str, key)
  str @?= "hello"
