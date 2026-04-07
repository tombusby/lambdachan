{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.TestHelpers (
  withTestApp,
  withTestPool,
) where

import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Sql (ConnectionPool, runMigrationSilent, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Network.Wai (Application)

import LambdaChan.App (mkApiApp)
import LambdaChan.Config (
  AppConfig (..),
  AppEnv (..),
  DatabaseBackend (..),
  defaultConfig,
 )
import LambdaChan.Database.Schema (migrateAll)

testConfig :: AppConfig
testConfig =
  defaultConfig
    { dbBackend = SQLite ":memory:"
    , tripcodeSalt = "test-salt-12345"
    }

-- | Set up an in-memory SQLite pool with a fresh schema.
withTestPool :: (ConnectionPool -> IO a) -> IO a
withTestPool action = do
  pool <- runNoLoggingT $ do
    p <- createSqlitePool ":memory:" 1
    _ <- runSqlPool (runMigrationSilent migrateAll) p
    return p
  action pool

-- | Build a WAI Application backed by an in-memory SQLite database.
withTestApp :: IO Application
withTestApp = do
  pool <- runNoLoggingT $ do
    p <- createSqlitePool ":memory:" 1
    _ <- runSqlPool (runMigrationSilent migrateAll) p
    return p
  let env = AppEnv{dbPool = pool, appConfig = testConfig}
  return (mkApiApp env)
