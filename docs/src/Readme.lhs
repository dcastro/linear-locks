linear-locks
===

`linear-locks` is a port of the [Surelock] Rust crate to Linear Haskell.

The package provides a `Mutex a` type (based on `MVar a`) that is statically guaranteed to not lead to deadlocks.

It achieves this by breaking one of the [Coffman conditions for deadlocks][Coffman]: the "circular wait" condition.
`linear-locks` ensures mutexes are always acquired in a consistent order.

Motivation
---

In Haskell, [`STM` is the holy grail][STM] for synchronizing access to multiple shared resources without risking deadlocks,
and it should absolutely be the first thing on your mind when writing concurrent code.

Still, `STM` does have its limitations:

* You cannot run arbitrary `IO` actions within `STM` transactions, which can be a roadblock if you need to interact with the outside world while holding locks.
* Due to its optimistic nature, scenarios with high contention can lead to excessive transaction retries and livelocks.

Locking primitives like `MVar`s solve both of these issues,
but juggling multiple `MVar`s is a sure way to sooner or later hit a deadlock.

Enter `linear-locks`: it provides a locking primitive `Mutex a` that is statically guaranteed to be free of deadlocks.

Getting started
---

`linear-locks` is meant to be used alongside the [`linear-base`][linear-base] package.

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
import LinearLocks

-- From `linear-base`:
import Prelude.Linear (Ur (..))
import Control.Functor.Linear qualified as Linear
import System.IO.Resource.Linear.Internal qualified as Internal (unsafeFromSystemIO)
\end{code}


\begin{code}%hidden
-- Dummy types
data Config = Config { verbose :: Bool }
data DbConn = DbConn
data User = User { balance :: Int }

-- A dummy IO block where we can run the example code below
example1 :: IO ()
example1 = do
\end{code}

Each mutex is assigned a "level" at compile-time.

\begin{code}
  -- `Mutex 0 Config`
  configMutex <- mkMutex 0 Config { verbose = True }

  -- `Mutex 1 DbConn`
  dbMutex <- mkMutex 1 DbConn {}
\end{code}

We can then enter a "lock scope".

We're given a `MutexKey lvl` that we can use to acquire mutexes.
The key starts off with level 0 (`MutexKey 0`) and it can be used to acquire any mutex with level 0 or above.

Every time we acquire a mutex, the key's level increases. Acquiring `Mutex 0 Config` consumes our `MutexKey 0` and gives us a `MutexKey 1`. Acquiring `Mutex 1 DbConn` then gives us a `MutexKey 2`.

Acquiring mutexes in the wrong order (e.g. trying to acquire a mutex of level 0 with a key of level 2) would be a type error.
This ensures locks are always acquired in order of increasing level, preventing circular waits and thus deadlocks.

The key is linearly typed, it must be consumed _exactly once_.
Using the same key to acquire 2 mutexes would be a type error.

\begin{code}
  lockScope \key -> Linear.do
    --                          ↓ Consumes `MutexKey 0` to lock a `Mutex 0`
    (configGuard, key) <- lock key configMutex
    --             ↑ Returns `MutexKey 1`


    --                      ↓ Consumes `MutexKey 1` to lock a `Mutex 1`
    (dbGuard, key) <- lock key dbMutex
    --         ↑ Returns `MutexKey 2`

    releaseGuard configGuard
    releaseGuard dbGuard
    Linear.pure (Ur (), key)
\end{code}

Notice how we had to use `Linear.do` (enabled by the `QualifiedDo` extension) and `Linear.pure` instead of `Prelude.pure` to chain our actions together.
This is because the lock scope action runs in [`RIO`][RIO], and `RIO` does not implement `Prelude.Monad`; instead, it implements [`Linear.Monad`][Linear.Monad] from `linear-base`.
This ensures values bound by `>>=` must be consumed exactly once.

<h3>MutexGuard</h3>

When we acquire a lock, we get back a `MutexGuard a` that represents our ownership of the lock.
We can freely read from / write to it while the lock is held.

The guard is also linearly typed, thus ensuring:

* We can never forget to release it with `releaseGuard`.
* It cannot be used after being released.

\begin{code}
  lockScope \key -> Linear.do
    (configGuard, key) <- lock key configMutex

    (Ur config, configGuard) <- readGuard configGuard

    configGuard <- writeGuard configGuard config { verbose = False }

    releaseGuard configGuard
    Linear.pure (Ur (), key)
\end{code}

Since the guard is linear, `readGuard` and `writeGuard` must consume the guard and return a new one.

`readGuard configGuard` returns a `Ur Config`.
`Ur` is short for "unrestricted", meaning the value is _not_ linear
and can be freely used as many times as needed.

<h3>MutexSet</h3>

Mutexes with the same level must be acquired simultaneously by adding them to a `MutexSet` and using `lockMany`.

\begin{code}
  alice <- mkMutex 3 User { balance = 100 }
  bob <- mkMutex 3 User { balance = 100 }

  users <- mkMutexSet (alice, bob)

  lockScope \key -> Linear.do
    ((aliceGuard, bobGuard), key) <- lockMany key users
    (Ur alice, aliceGuard) <- readGuard aliceGuard
    (Ur bob, bobGuard) <- readGuard bobGuard

    bobGuard <- writeGuard bobGuard bob { balance = balance bob + 10 }
    aliceGuard <- writeGuard aliceGuard alice { balance = balance alice - 10 }

    releaseGuard bobGuard
    releaseGuard aliceGuard
    Linear.pure (Ur (), key)
\end{code}

To prevent deadlocks, mutexes in a set are always acquired in a deterministic order.
Creating a set with `(alice, bob)` or `(bob, alice)` will always result
in them being acquired in the same order.

<h3>IO</h3>

For the time being, in order to perform IO actions within a lock scope,
we need to use `linear-base`'s `Internal.unsafeFromSystemIO`.

Note, however, that this function is in fact safe.
The [upcoming `linear-base` release][PR] will include public `fromSystemIO` and `liftSystemIO` functions.

\begin{code}
  lockScope \key -> Linear.do
    (configGuard, key) <- lock key configMutex
    (Ur config, configGuard) <- readGuard configGuard

    Ur newVerbose <- Internal.unsafeFromSystemIO do
      putStrLn $ "Verbose mode is: " <> show (verbose config)
      putStrLn $ "Enter new verbose mode: "
      Ur <$> readLn @Bool

    configGuard <- writeGuard configGuard config { verbose = newVerbose }
    releaseGuard configGuard
    Linear.pure (Ur (), key)
\end{code}


Roadmap
---

- [ ] Allow backtracking of `MutexKey`'s level when a lock is released


 [Surelock]: https://notes.brooklynzelenka.com/Blog/Surelock
 [linear-base]: https://hackage.haskell.org/package/linear-base
 [Linear.IO]: https://hackage-content.haskell.org/package/linear-base/docs/System-IO-Linear.html
 [Linear.Monad]: https://hackage-content.haskell.org/package/linear-base/docs/Control-Functor-Linear.html#t:Monad
 [RIO]: https://hackage-content.haskell.org/package/linear-base/docs/System-IO-Resource-Linear.html
 [Ur]: https://hackage-content.haskell.org/package/linear-base/docs/Data-Unrestricted-Linear.html#t:Ur
 [Coffman]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)#Prevention
 [PR]: https://github.com/tweag/linear-base/pull/505
 [STM]: https://chrispenner.ca/posts/mutexes
