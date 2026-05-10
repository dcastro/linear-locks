{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module ServerExample (run) where

import Control.Functor.Linear qualified as Linear
import Control.Monad (void)
import Control.Monad.IO.Class.Linear qualified as Linear
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Text (Text)
import Data.Yaml qualified as Yaml
import LinearLocks
import LinearLocks.Mutex (Mutex)
import LinearLocks.Mutex qualified as Mutex
import LinearLocks.RWLock (RWLock)
import LinearLocks.RWLock qualified as RWLock
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Prelude.Linear (Ur (..))
import System.Cron qualified as Cron
import System.FSNotify qualified as FSNotify
import System.FilePath ((</>))

{-
This contrived example demonstrates how to use `linear-locks` in a simple server application to juggle 3 shared resources:

- An in-memory config that's periodically refreshed from disk
- A sqlite database connection
- Server metrics that are updated by each request and periodically reported

There are 4 main tasks in this example:
1. A task that watches `config.yaml` for changes.
   When it is modified, it reloads the config from disk and updates the in-memory config.
2. A scheduled task that dumps the server metrics every 1 minute.
   Depending on the config, it might or might not dumps the metrics to the database.
3. A server that handles incoming requests. The request handler may read the config and acquire a db connection.
4. A middleware layer that updates the in-memory metrics after each request is handled.

Things worth noting:
- The in-memory config is stored in a `RWLock`, since it is
  read frequently by the request handlers (task #3) and metrics reporter (task #2),
  but only updated when the config file changes (task #1).
- Both tasks #2 and #3 need to acquire both the config and the db connection.
  `linear-locks` ensures they are always acquired in the same order, guaranteeing that there are no deadlocks.
  The config lock has level 1 and the db lock has level 2, so the config lock is always acquired before the db lock.
- Task #2 acquires the db lock _conditionally_!
  It first acquires the config lock, checks whether the config requires it to report metrics to the database, and only then it acquires the db lock.
  This scenario requires "incremental locking", so we couldn't possibly use a `LockSet` to atomatically acquire all locks.

-}

data SqliteConn = SqliteConn

type Url = Text

newtype Metrics = Metrics
  { requestCount :: Int
  }

newtype Config = Config
  { reportingConfig :: ReportingConfig
  }

-- | Specifies where the server's metrics should be reported to.
data ReportingConfig
  = -- Store the metrics in a remote server
    ReportMetricsToServer Url
  | -- Store the metrics in the database
    ReportMetricsToDatabase

deriveJSON defaultOptions ''ReportingConfig
deriveJSON defaultOptions ''Config

run :: IO ()
run = do
  let configFolder = "./config"
      configFilePath = configFolder </> "config.yaml"

  config <- Yaml.decodeFileThrow @_ @Config configFilePath

  -- Create a lock for each shared resource
  metricsLock <- Mutex.new 0 Metrics {requestCount = 0}
  configLock <- RWLock.new 1 config
  dbLock <- Mutex.new 2 SqliteConn

  -- Setup tasks
  FSNotify.withManager \mgr -> do
    -- 1. Watch the config file for changes, and update the in-memory config when it's modified
    refreshConfig mgr configFolder configFilePath configLock
    -- 2. Schedule a task to report server metrics every 1 minute
    _ <- Cron.execSchedule do
      Cron.addJob (reportMetrics metricsLock configLock dbLock) "* * * * *"
    -- 3 and 4. Start the server with `middleware` that updates the metrics for each request
    Warp.run 8080 $ middleware metricsLock (mkApp configLock dbLock)

-- | 1. A task that watches `config.yaml` for changes.
--      When it is modified, it reloads the config from disk and updates the in-memory config.
refreshConfig :: FSNotify.WatchManager -> FilePath -> FilePath -> RWLock 1 Config -> IO ()
refreshConfig mgr configFolder configFilePath configLock =
  void $
    FSNotify.watchDir
      mgr
      configFolder
      ( \case
          FSNotify.Modified {eventPath} -> eventPath == configFilePath
          _ -> False
      )
      ( \_ -> do
          config <- Yaml.decodeFileThrow @_ @Config configFilePath
          lockScope \key -> Linear.do
            (guard, key) <- RWLock.acquireWrite key configLock
            guard <- RWLock.write guard config
            RWLock.releaseWrite guard
            dropKeyAndReturn key ()
      )

-- | 2. A scheduled task that dumps the server metrics every 1 minute.
--      Depending on the config, it might or might not dumps the metrics to the database.
reportMetrics :: Mutex 0 Metrics -> RWLock 1 Config -> Mutex 2 SqliteConn -> IO ()
reportMetrics metricsLock configLock dbLock = do
  lockScope \key -> Linear.do
    (metricsGuard, key) <- Mutex.acquire key metricsLock
    (Ur _metrics, metricsGuard) <- Mutex.read metricsGuard

    (configGuard, key) <- RWLock.acquireRead key configLock
    (Ur config, configGuard) <- RWLock.read configGuard

    case config.reportingConfig of
      ReportMetricsToDatabase -> Linear.do
        (dbGuard, key) <- Mutex.acquire key dbLock
        (Ur _db, dbGuard) <- Mutex.read dbGuard

        Linear.liftSystemIO do
          -- TODO: Use db connection to report the metrics
          pure ()

        Mutex.release dbGuard
        dropKey key
      ReportMetricsToServer _url -> Linear.do
        Linear.liftSystemIO do
          -- TODO: Report the metrics to the remote server using the configured url
          pure ()
        dropKey key

    Mutex.release metricsGuard
    RWLock.releaseRead configGuard
    Linear.pure (Ur ())

-- | 3. A server that handles incoming requests. The request handler may read the config and acquire a db connection.
mkApp :: RWLock 1 Config -> Mutex 2 SqliteConn -> Wai.Application
mkApp configLock dbLock _req respond = do
  rsp <- lockScope \key -> Linear.do
    -- Acquire the config and db connection to process the request.
    (configGuard, key) <- RWLock.acquireRead key configLock
    (Ur _config, configGuard) <- RWLock.read configGuard

    (dbGuard, key) <- Mutex.acquire key dbLock
    (Ur _db, dbGuard) <- Mutex.read dbGuard

    -- TODO: handle the request
    let response = Wai.responseLBS Http.status200 [] "Hello World"

    RWLock.releaseRead configGuard
    Mutex.release dbGuard
    dropKeyAndReturn key response

  respond rsp

-- | 4. A middleware layer that updates the in-memory metrics after each request is handled.
middleware :: Mutex 0 Metrics -> Wai.Middleware
middleware metricsLock app req respond = do
  -- Run request handler
  response <- app req respond

  -- Update the metrics
  lockScope \key -> Linear.do
    (guard, key) <- Mutex.acquire key metricsLock

    (Ur metrics, guard) <- Mutex.read guard
    guard <- Mutex.write guard metrics {requestCount = metrics.requestCount + 1}

    Mutex.release guard
    dropKeyAndReturn key ()
  pure response
