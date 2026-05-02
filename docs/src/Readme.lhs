linear-locks
===

A port of [Surelock](https://notes.brooklynzelenka.com/Blog/Surelock) to Haskell.


Some examples can be found in the [`Examples` module](examples/Examples.hs).

Getting started
---

We'll need `QualifiedDo`:

\begin{code}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
\end{code}


\begin{code}%hidden
module Readme where
\end{code}

And the following imports:

\begin{code}
import Control.Functor.Linear qualified as Linear
import Prelude.Linear (Ur (..))
import LinearLocks
import System.IO.Resource.Linear.Internal qualified as Internal (unsafeFromSystemIO)
\end{code}


\begin{code}
example1 :: IO ()
example1 = do
  mutex <- mkMutex 0 "hello"
  lockScope \key -> Linear.do
    (mg, key) <- lock key mutex
    (Ur str, mg) <- readGuard mg
    Internal.unsafeFromSystemIO (putStrLn str)
    mg <- writeGuard mg "world"
    releaseGuard mg
    Linear.pure (Ur (), key)
\end{code}

Roadmap
---

- [ ] Allow backtracking of `MutexKey`'s level when a lock is released
