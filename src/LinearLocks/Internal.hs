{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

#if !MIN_VERSION_linear_base(0,7,1)
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

module LinearLocks.Internal where

import Control.Concurrent (ThreadId, myThreadId)
import Control.DeepSeq (NFData, force)
import Control.Exception (Exception (..), bracket_, throw)
import Control.Functor.Linear qualified as L
import Control.Monad.IO.Class.Linear qualified as L
import Data.Atomics.Counter (AtomicCounter)
import Data.Atomics.Counter qualified as Atomic
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Focus qualified
import GHC.Base (Type)
import GHC.Conc (atomically)
import GHC.IO (unsafePerformIO)
import GHC.TypeLits (Nat, type (+), type (<=))
import Prelude.Linear (Ur (..))
import StmContainers.Set qualified as StmSet
import System.IO.Linear qualified as L
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
import System.IO.Resource.Linear.Internal qualified as RIOInternal

-- | A key used to acquire locks.
-- A key of level @n@ can only acquire locks of level @n@ or higher.
--
-- Acquiring a lock with `acquire` or `LinearLocks.acquireMany` will consume the key and return a new key with an increased level,
-- ensuring locks are always acquired in a consistent order.
data LockKey (lvl :: Nat)
  = -- Notes:
    --  * Do not export the constructor
    --  * Do not implement `Consumable` / `Dupable` / `Movable`
    UnsafeLockKey

-- | A unique identifier for a lock.
newtype LockId = LockId Int
  deriving newtype (Eq, Ord, Show)

newtype instance VU.MVector s LockId = MV_LockId (VP.MVector s Int)

newtype instance VU.Vector LockId = V_LockId (VP.Vector Int)

deriving via (VU.UnboxViaPrim Int) instance VGM.MVector VU.MVector LockId

deriving via (VU.UnboxViaPrim Int) instance VG.Vector VU.Vector LockId

instance VU.Unbox LockId

-- | Creates a new lock scope with a key of level 0, and runs the given function with it.
--  The key can be used to acquire locks with `acquire` and `LinearLocks.acquireMany`.
--
-- After acquiring all the necessary locks, the key must be dropped with
-- `dropKey` or `dropKeyAndReturn`.
--
-- Will throw a t`NestedLocksScopeException` if a nested `lockScope` is created at runtime.
lockScope ::
  forall a.
  -- NOTE: The use of `Ur` prevents the key (and any other linear values) from escaping the scope
  -- of the `lockScope` function via the variable `a`.
  -- See: https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/#sticky-ends-of-scopes
  (LockKey 0 %1 -> RIO (Ur a)) ->
  IO a
lockScope run = do
  ensureNotNested do
    RIO.run L.do
      let key = UnsafeLockKey @0
      run key
  where
    -- Ensures nested lock scopes are not created.
    -- We can't really detect this at compile-time, so we'll make do with a runtime check.
    ensureNotNested :: IO a -> IO a
    ensureNotNested action = do
      tid <- myThreadId
      bracket_
        -- Acquire: register the thread ID in the set of active lock scopes.
        ( do
            success <- atomically do
              StmSet.focus
                ( do
                    -- Check if the thread ID is already in the set.
                    Focus.lookup >>= \case
                      Just () ->
                        -- The thread ID was found in the set, which means we're trying to create a nested lock scope.
                        -- We return `False` to signal an error.
                        pure False
                      Nothing -> do
                        Focus.insert ()
                        pure True
                )
                tid
                lockScopes
            if success
              then pure ()
              else throw NestedLocksScopeException
        )
        -- Release: remove the thread ID from the set of active lock scopes.
        ( atomically do
            StmSet.delete tid lockScopes
        )
        action

-- | Discard a key. Should be used after acquiring all the necessary locks in a lock scope.
dropKey :: LockKey lvl %1 -> RIO ()
dropKey UnsafeLockKey = L.pure ()

-- | Convenience function to drop the key and return a pure value at the end of a lock scope.
dropKeyAndReturn :: LockKey lvl %1 -> a -> RIO (Ur a)
dropKeyAndReturn key a = L.do
  dropKey key
  L.pure (Ur a)

data NestedLocksScopeException = NestedLocksScopeException
  deriving stock (Show)

instance Exception NestedLocksScopeException where
  displayException NestedLocksScopeException = "Nested lock scopes are not allowed"

-- | Acquires a lock.
-- Consumes the key and return a new key (with an increased level).
acquire ::
  forall keyLvl acquirable.
  (Acquirable acquirable) =>
  (keyLvl <= Level acquirable) =>
  LockKey keyLvl %1 ->
  acquirable ->
  RIO (Guard acquirable, LockKey (Level acquirable + 1))
acquire UnsafeLockKey m = L.do
  guard <- unsafeAcquire m
  L.pure (guard, UnsafeLockKey)

class (Releasable (Guard acquirable)) => Acquirable acquirable where
  type Guard acquirable :: Type
  type Level acquirable :: Nat

  getId :: acquirable -> LockId

  -- | This is marked as unsafe because it does not consume a `LockKey`.
  unsafeAcquire :: acquirable -> RIO (Guard acquirable)

class Releasable guard where
  -- Design decision: `doRelease` generalizes over releasing any kind of guard, but we don't export it.
  -- We only export the monomorphic `release` functions for each guard type, because they might have
  -- important notes in their haddock docs (e.g. `StrictMutex.release` does deep evaluation and might throw an exception as a result),
  -- so it's important those docs are easily discoverable and not hidden behind a more general `doRelease` function.
  doRelease :: guard %1 -> RIO ()

----------------------------------------------------------------------------
-- Global variables
----------------------------------------------------------------------------

-- | A set of the ThreadIds currently holding a lock scope.
-- We use this to prevent nested lock scopes at runtime.
{-# NOINLINE lockScopes #-}
lockScopes :: StmSet.Set ThreadId
lockScopes =
  -- See: https://wiki.haskell.org/index.php?oldid=64612
  unsafePerformIO StmSet.newIO

-- | An atomic counter used to generate unique IDs for locks.
{-# NOINLINE lockIdCounter #-}
lockIdCounter :: AtomicCounter
lockIdCounter =
  unsafePerformIO $ Atomic.newCounter 0

-- | Generates the next unique lock ID.
nextLockId :: IO LockId
nextLockId = do
  newId <- Atomic.incrCounter 1 lockIdCounter
  pure (LockId newId)

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- Only provide this orphan instance for linear-base <= 0.7.0
-- The next release will come with this instance built-in: https://github.com/tweag/linear-base/pull/505
#if !MIN_VERSION_linear_base(0,7,1)
instance L.MonadIO RIO where
  liftIO action = RIOInternal.RIO (\_ -> action)
#endif

-- | Similar to 'System.IO.Resource.Linear.release', except it uses a different release action than the one registered by 'System.IO.Resource.Linear.unsafeAcquire'.
release' :: RIO.Resource a %1 -> L.IO () -> RIO ()
release' (RIOInternal.UnsafeResource key _) release = RIOInternal.RIO (\st -> L.mask_ (releaseWith key st))
  where
    releaseWith key rrm = L.do
      Ur (RIOInternal.ReleaseMap releaseMap) <- L.readIORef rrm
      () <- release
      L.writeIORef rrm (RIOInternal.ReleaseMap (IntMap.delete key releaseMap))

-- | A wrapper type to force the contents to be fully evaluated before being put back into an MVar / IORef.
--
-- NOTE: `NF` will only turn "shallow evaluation" into "deep evaluation".
-- You must still use a bang pattern on `NF` to force it.
newtype NF a = UnsafeNF {unNF :: a}
  deriving newtype (Show, Eq)

mkNF :: (NFData a) => a -> NF a
mkNF = UnsafeNF . force
