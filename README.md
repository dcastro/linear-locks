# linear-locks

A port of [Surelock](https://notes.brooklynzelenka.com/Blog/Surelock) to Haskell.


Some examples can be found in the [`Examples` module](examples/Examples.hs).


## Roadmap

- [ ] Allow locking multiple mutexes at the same level in a deterministic order
- [ ] Allow backtracking of `MutexKey`'s level when a lock is released
