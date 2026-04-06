module Main (main) where

import qualified Data.Text as T
import System.Environment (lookupEnv)

import LambdaChan.App (runApp)
import LambdaChan.Config (AppConfig (..), DatabaseBackend (..), defaultConfig)

main :: IO ()
main = do
  mDbUrl     <- lookupEnv "DATABASE_URL"
  mPort      <- lookupEnv "PORT"
  mSalt      <- lookupEnv "TRIPCODE_SALT"
  mStaticDir <- lookupEnv "STATIC_DIR"

  let backend = case mDbUrl of
        Just url
          | "postgres" `elem` words url -> PostgreSQL (T.pack url)
        Just path -> SQLite path
        Nothing -> dbBackend defaultConfig

      port = maybe (serverPort defaultConfig) read mPort
      salt = maybe (tripcodeSalt defaultConfig) T.pack mSalt

      cfg =
        defaultConfig
          { dbBackend      = backend
          , serverPort     = port
          , tripcodeSalt   = salt
          , staticFilesDir = mStaticDir
          }

  runApp cfg
