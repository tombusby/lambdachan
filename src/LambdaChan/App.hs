{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.App (
  mkApp,
  mkApiApp,
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
import Network.Wai (Application, pathInfo)
import Network.Wai.Application.Static
  ( defaultFileServerSettings, staticApp )
import Network.Wai.Handler.Warp (run)
import Servant (hoistServer, serve)
import WaiAppStatic.Types (ss404Handler, ssIndices, unsafeToPiece)
import System.IO (hPutStrLn, stderr)

import LambdaChan.API.Handlers (appServer)
import LambdaChan.API.Types (lambdaChanAPI)
import LambdaChan.Auth (hashUserPassword)
import LambdaChan.Config
import LambdaChan.Database.Queries (createUser, listUsers)
import LambdaChan.Database.Schema (User (..), migrateAll)
import LambdaChan.Types (UserRole (..))

-- | Pure Servant application — no static file routing.
-- Use this in tests so that the test paths (/boards, /auth/login, etc.)
-- are routed directly to Servant without needing an /api prefix.
mkApiApp :: AppEnv -> Application
mkApiApp env =
  serve lambdaChanAPI $
    hoistServer lambdaChanAPI (\app -> runReaderT app env) appServer

-- | Full WAI Application from an AppEnv.
-- Requests with path starting with "api" are routed to Servant (with the
-- "api" segment stripped).  Everything else is served from frontend/dist/,
-- falling back to index.html for SPA client-side routing.
mkApp :: AppEnv -> Application
mkApp env req respond
  | "api" : rest <- pathInfo req =
      let req' = req { pathInfo = rest }
      in servantApp req' respond
  | otherwise =
      staticFileApp req respond
  where
    servantApp = mkApiApp env
    staticDir =
      case staticFilesDir (appConfig env) of
        Just d  -> d
        Nothing -> "frontend/dist"
    staticFileApp =
      staticApp
        (defaultFileServerSettings staticDir)
          { ssIndices    = [unsafeToPiece "index.html"]
          , ss404Handler = Just (spaFallback staticDir)
          }

-- | Serve index.html for any path that doesn't match a real file,
-- allowing the Elm router to handle client-side routes on browser refresh.
spaFallback :: FilePath -> Application
spaFallback dir req respond =
  staticApp
    (defaultFileServerSettings dir)
      { ssIndices = [unsafeToPiece "index.html"] }
    (req { pathInfo = [] })
    respond

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
