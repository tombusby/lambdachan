{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.App (
  mkApp,
  runApp,
  initialisePool,
  seedDatabase,
) where

import Control.Monad (void, when)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (hoistServer, serve)
import System.IO (hPutStrLn, stderr)

import LambdaChan.API.Handlers (appServer)
import LambdaChan.API.Types (lambdaChanAPI)
import LambdaChan.Auth (hashUserPassword)
import LambdaChan.Config
import LambdaChan.Database.Queries (createUser, listUsers)
import LambdaChan.Database.Schema (User (..), migrateAll)
import LambdaChan.Types (UserRole (..))

-- | Create the WAI Application from an AppEnv.
mkApp :: AppEnv -> Application
mkApp env =
  serve lambdaChanAPI $
    hoistServer lambdaChanAPI (appToHandler env) appServer
 where
  appToHandler e app = runReaderT app e

-- | Create the connection pool for the configured backend.
initialisePool :: AppConfig -> IO ConnectionPool
initialisePool cfg = runStderrLoggingT $ case dbBackend cfg of
  SQLite path ->
    createSqlitePool (T.pack path) (poolSize cfg)
  PostgreSQL connStr ->
    createPostgresqlPool (encodeUtf8 connStr) (poolSize cfg)

-- | Run all pending migrations against the pool.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool = runSqlPool (runMigration migrateAll) pool

{- | Create a default admin user if no users exist at all.
Credentials are logged to stderr so they can be changed immediately.
-}
seedDatabase :: AppConfig -> ConnectionPool -> IO ()
seedDatabase _ pool = do
  users <- runSqlPool listUsers pool
  when (null users) $ do
    let adminUser = "admin"
        adminPass = "changeme"
    hPutStrLn stderr ""
    hPutStrLn stderr "=========================================="
    hPutStrLn stderr "  No users found — creating default admin"
    hPutStrLn stderr $ "  Username: " <> adminUser
    hPutStrLn stderr $ "  Password: " <> adminPass
    hPutStrLn stderr "  CHANGE THIS PASSWORD IMMEDIATELY"
    hPutStrLn stderr "=========================================="
    hPutStrLn stderr ""
    passHash <- hashUserPassword (T.pack adminPass)
    now <- getCurrentTime
    void $
      runSqlPool
        ( createUser
            User
              { userUsername = T.pack adminUser
              , userPasswordHash = passHash
              , userRole = AdminRole
              , userCreatedAt = now
              }
        )
        pool

-- | Start the HTTP server.
runApp :: AppConfig -> IO ()
runApp cfg = do
  pool <- initialisePool cfg
  runMigrations pool
  seedDatabase cfg pool
  let env = AppEnv{dbPool = pool, appConfig = cfg}
      port = serverPort cfg
  hPutStrLn stderr $ "lambdachan listening on port " <> show port
  run port (mkApp env)
