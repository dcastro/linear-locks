linear-locks
===

`linear-locks` is a port of the [Surelock] Rust crate to Linear Haskell.

The package provides locking primitives that are statically guaranteed not to lead to deadlocks.

It achieves this by breaking one of the [Coffman conditions for deadlocks][Coffman]: the "circular wait" condition.
`linear-locks` ensures locks are always acquired in a consistent order.


Currently supported lock types:

  * "LinearLocks.Mutex"
  * "LinearLocks.Mutex.Strict"
  * "LinearLocks.RWLock"
  * "LinearLocks.RWLock.Strict"

Motivation
---

In Haskell, [`STM` is the holy grail][STM] for synchronizing access to multiple shared resources without risking deadlocks,
and it should absolutely be the first thing on your mind when writing concurrent code.

Still, `STM` does have its limitations:

* You cannot run arbitrary `IO` actions within `STM` transactions, which can be a roadblock if you need to interact with the outside world while holding locks.
* Due to its optimistic nature, scenarios with high contention can lead to excessive transaction retries and livelocks.

Locking primitives like `MVar`s solve both of these issues,
but juggling multiple `MVar`s is a sure way to hit a deadlock sooner or later.

Enter `linear-locks`: it provides locking primitives that are statically guaranteed to be free of deadlocks.

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
import LinearLocks.Mutex qualified as Mutex

-- From `linear-base`:
import Prelude.Linear (Ur (..))
import Control.Functor.Linear qualified as Linear
import Control.Monad.IO.Class.Linear qualified as Linear
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

Each lock is assigned a "level" at compile-time.

\begin{code}
  -- `Mutex 0 Config`
  configMutex <- Mutex.new 0 Config { verbose = True }

  -- `Mutex 1 DbConn`
  dbMutex <- Mutex.new 1 DbConn {}
\end{code}

We can then enter a "lock scope".

We're given a `LockKey lvl` that we can use to acquire locks.
The key starts off with level 0 (`LockKey 0`) and it can be used to acquire any lock with level 0 or above.

Every time we acquire a lock, the key's level increases.
Acquiring `Mutex 0 Config` consumes our `LockKey 0` and gives us a `LockKey 1` back.
Acquiring `Mutex 1 DbConn` then gives us a `LockKey 2`.


\begin{code}
  lockScope \key -> Linear.do
    --                                   ↓ Consumes `LockKey 0` to acquire a `Mutex 0`
    (configGuard, key) <- Mutex.acquire key configMutex
    --             ↑ Returns `LockKey 1`


    --                               ↓ Consumes `LockKey 1` to acquire a `Mutex 1`
    (dbGuard, key) <- Mutex.acquire key dbMutex
    --         ↑ Returns `LockKey 2`

    Mutex.release configGuard
    Mutex.release dbGuard
    Linear.pure (Ur (), key)
\end{code}

Acquiring locks in the wrong order (e.g. trying to acquire a lock of level 0 with a key of level 2) would be a type error.
This ensures locks are always acquired in order of increasing level, preventing circular waits and thus deadlocks.

The key is linearly typed; it must be consumed _exactly once_.
Using the same key to acquire 2 locks would be a type error.

Notice how we had to use `Linear.do` (enabled by the `QualifiedDo` extension) and `Linear.pure` instead of `Prelude.pure` to chain our actions together.
This is because the lock scope action runs in [`RIO`][RIO], and `RIO` does not implement `Prelude.Monad`; instead, it implements [`Linear.Monad`][Linear.Monad] from `linear-base`.
This ensures values bound by `>>=` must be consumed exactly once.

<h3>Guards</h3>

When we acquire a mutex, we get back a `MutexGuard a` that represents our ownership of the lock.
We can freely read from / write to it while the lock is held.

The guard is also linearly typed, thus ensuring:

* We can never forget to release it with `release`.
* It cannot be used after being released.

\begin{code}
  lockScope \key -> Linear.do
    (configGuard, key) <- Mutex.acquire key configMutex

    (Ur config, configGuard) <- Mutex.read configGuard

    configGuard <- Mutex.write configGuard config { verbose = False }

    Mutex.release configGuard
    Linear.pure (Ur (), key)
\end{code}

Since the guard is linear, `read` and `write` must consume the guard and return a new one.

`read configGuard` returns a `Ur Config`.
`Ur` is short for "unrestricted", meaning the value is _not_ linear
and can be freely used as many times as needed.

<h3>LockSet</h3>

Locks with the same level must be acquired simultaneously by adding them to a `LockSet` and using `acquireMany`.

\begin{code}
  alice <- Mutex.new 3 User { balance = 100 }
  bob <- Mutex.new 3 User { balance = 100 }

  users <- newLockSet (alice, bob)

  lockScope \key -> Linear.do
    ((aliceGuard, bobGuard), key) <- acquireMany key users
    (Ur alice, aliceGuard) <- Mutex.read aliceGuard
    (Ur bob, bobGuard) <- Mutex.read bobGuard

    bobGuard <- Mutex.write bobGuard bob { balance = balance bob + 10 }
    aliceGuard <- Mutex.write aliceGuard alice { balance = balance alice - 10 }

    Mutex.release bobGuard
    Mutex.release aliceGuard
    Linear.pure (Ur (), key)
\end{code}

To prevent deadlocks, locks in a set are always acquired in a deterministic order.
Creating a set with `(alice, bob)` or `(bob, alice)` will always result
in them being acquired in the same order.

<h3>IO</h3>

You can use the linear [`MonadIO` from `linear-base`][MonadIO] to lift `IO` actions into the lock scope.

\begin{code}
  lockScope \key -> Linear.do
    (configGuard, key) <- Mutex.acquire key configMutex
    (Ur config, configGuard) <- Mutex.read configGuard

    Ur newVerbose <- Linear.liftSystemIOU do
      putStrLn $ "Verbose mode is: " <> show (verbose config)
      putStrLn $ "Enter new verbose mode: "
      readLn @Bool

    configGuard <- Mutex.write configGuard config { verbose = newVerbose }
    Mutex.release configGuard
    Linear.pure (Ur (), key)
\end{code}

Note: for the time being, the `linear-locks` package conditionally provides an orphan instance of `MonadIO` for the `RIO` monad
when compiled against `linear-base <= 0.7.0`.
The next version of `linear-base` [will include][PR] a `MonadIO` instance itself.

Roadmap
---

- [ ] Allow backtracking of `LockKey`'s level when a lock is released


 [Surelock]: https://notes.brooklynzelenka.com/Blog/Surelock
 [linear-base]: https://hackage.haskell.org/package/linear-base
 [Linear.IO]: https://hackage-content.haskell.org/package/linear-base/docs/System-IO-Linear.html
 [Linear.Monad]: https://hackage-content.haskell.org/package/linear-base/docs/Control-Functor-Linear.html#t:Monad
 [RIO]: https://hackage-content.haskell.org/package/linear-base/docs/System-IO-Resource-Linear.html
 [Ur]: https://hackage-content.haskell.org/package/linear-base/docs/Data-Unrestricted-Linear.html#t:Ur
 [Coffman]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)#Prevention
 [MonadIO]: https://hackage-content.haskell.org/package/linear-base/docs/Control-Monad-IO-Class-Linear.html
 [PR]: https://github.com/tweag/linear-base/pull/505
 [STM]: https://chrispenner.ca/posts/mutexes
