
## Nested lock scopes

Lock scopes can't be nested. If they were, it would be possible to acquire locks out of order.
We have a runtime check to prevent nested lock scopes from being created.

For this reason, it's important that `LockSet` does NOT implement `Acquirable`.


## `acquire`

We have a polymorphic `acquire` function, and I wondered whether I should simply export it from `LinearLocks`.

It's suitable for acquiring mutexes.
However, for acquiring rwlocks, it must be used with a newtype, e.g. `acquire key (RWLock.AsRead rwlock)`.
I wanted the public API to be simpler to use, so having dedicated `RWLock.acquireRead` and `RWLock.acquireWrite` functions makes sense.

To keep the interfaces for all locks similar, I decided it would be best to:

* re-export `acquire` from the `LinearLocks.Mutex` and `LinearLocks.Mutex.Strict` modules
* re-export `acquireRead` / `acquireWrite` from the `LinearLocks.RWLock` and `LinearLocks.RWLock.Strict` modules
* not export any acquire-like functions from the main module `LinearLocks`.


## `release`

`Releasable.doRelease` generalizes over releasing any kind of guard, but we don't export it.
We only export the monomorphic `release` functions for each guard type, because they might have
important notes in their haddock docs (e.g. `StrictMutex.release` does deep evaluation and might throw an exception as a result),
so it's important those docs are easily discoverable and not hidden behind a more general `doRelease` function.
