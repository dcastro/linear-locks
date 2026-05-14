
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


## `dropKey`

The initial design had no `dropKey` function. `lockScope` required the user to return the key at the end of the block.
This was an issue: if the `lockScope` block branched out, both blocks had to return a key with the same type, and of the same level.
In other words, the last lock acquired by both branches had to have the same level.

To eliminate this restriction, we added `dropKey` to allow branches to acquire different locks and then independently discard the key.


## `withMutex`

An implementation of `withMutex` is _possible_:

```hs
withMutex ::
  (keyLvl <= mutexLvl) =>
  LockKey keyLvl %1 ->
  Mutex mutexLvl a ->
  (a -> LockKey (mutexLvl + 1) %1 -> RIO (Ur a, res, LockKey finalLvl)) ->
  RIO (res, LockKey finalLvl)
withMutex key m action = L.do
  (guard, key) <- acquire key m
  (Ur a, guard) <- read guard
  (Ur newValue, res, key) <- action a key
  guard <- write guard newValue
  release guard
  L.pure (res, key)
```

But I decided not to include it for these reasons:

* The ergonomics start to degrade when handling 2 or more locks, it quickly becomes unbearable.
  * Demo: https://gist.github.com/dcastro/c899eb6cde588a2bdbc45ba442a98fc8
* Unlike the "acquire + release" API, the `withMutex` API would not allow "partial overlaps" of critical sections, e.g.:
  * acquire lock 1
  * acquire lock 2
  * release lock 1
  * release lock 2

Since the point of the package is to allow safely handling many locks, it makes little sense to provide an API that would
optimize for single lock scenarios and be useless otherwise.
