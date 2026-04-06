{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.TestHelpers (
  withTestApp,
  withTestPool,
  testConfig,
  createTestAdmin,
  createTestMod,
  loginAs,
  bearerHeader,
) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (Entity (..))
import Database.Persist.Sql (ConnectionPool, runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Network.Wai (Application)

import LambdaChan.App (mkApp)
import LambdaChan.Auth (hashUserPassword, sessionDuration)
import LambdaChan.Config (
  AppConfig (..),
  AppEnv (..),
  DatabaseBackend (..),
  defaultConfig,
 )
import LambdaChan.Database.Queries (createSession, createUser, getUserByUsername)
import LambdaChan.Database.Schema
import LambdaChan.Types (UserRole (..))

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
    runSqlPool (runMigration migrateAll) p
    return p
  action pool

-- | Build a WAI Application backed by an in-memory SQLite database.
withTestApp :: IO Application
withTestApp = do
  pool <- runNoLoggingT $ do
    p <- createSqlitePool ":memory:" 1
    runSqlPool (runMigration migrateAll) p
    return p
  let env = AppEnv{dbPool = pool, appConfig = testConfig}
  return (mkApp env)

-- | Insert an admin user; return (username, password).
createTestAdmin :: ConnectionPool -> IO (Text, Text)
createTestAdmin pool = do
  let username = "testadmin"
      password = "adminpass"
  passHash <- hashUserPassword password
  now <- getCurrentTime
  _ <-
    runSqlPool
      ( createUser
          User
            { userUsername = username
            , userPasswordHash = passHash
            , userRole = AdminRole
            , userCreatedAt = now
            }
      )
      pool
  return (username, password)

-- | Insert a moderator user; return (username, password).
createTestMod :: ConnectionPool -> IO (Text, Text)
createTestMod pool = do
  let username = "testmod"
      password = "modpass"
  passHash <- hashUserPassword password
  now <- getCurrentTime
  _ <-
    runSqlPool
      ( createUser
          User
            { userUsername = username
            , userPasswordHash = passHash
            , userRole = ModeratorRole
            , userCreatedAt = now
            }
      )
      pool
  return (username, password)

-- | Create a live session for the named user; return the token.
loginAs :: ConnectionPool -> Text -> IO Text
loginAs pool username = do
  mUser <- runSqlPool (getUserByUsername username) pool
  case mUser of
    Nothing -> error $ "loginAs: user not found: " <> T.unpack username
    Just (Entity uid _) -> do
      tok <- toText <$> nextRandom
      now <- getCurrentTime
      _ <-
        runSqlPool
          ( createSession
              Session
                { sessionUserId = uid
                , sessionToken = tok
                , sessionCreatedAt = now
                , sessionExpiresAt = addUTCTime sessionDuration now
                }
          )
          pool
      return tok

-- | Build a "Bearer <token>" ByteString suitable for an Authorization header.
bearerHeader :: Text -> Text
bearerHeader tok = "Bearer " <> tok
