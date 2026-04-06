{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.Config
  ( DatabaseBackend (..)
  , AppConfig (..)
  , AppEnv (..)
  , App
  , runDB
  , defaultConfig
  , throwNotFound
  , throwBadRequest
  , throwForbidden
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import Network.HTTP.Types (hContentType)
import Servant (Handler, ServerError (..), err400, err403, err404, throwError)

-- | Which database backend to use. Connection string for PostgreSQL
-- follows the libpq format: "host=... dbname=... user=... password=..."
data DatabaseBackend
  = SQLite FilePath
  | PostgreSQL Text
  deriving (Show, Eq)

data AppConfig = AppConfig
  { dbBackend     :: DatabaseBackend
  , serverPort    :: Int
  , poolSize      :: Int
  , tripcodeSalt  :: Text  -- ^ Server-side salt for secure (##) tripcodes
  } deriving (Show, Eq)

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { dbBackend    = SQLite "lambdachan.db"
  , serverPort   = 8080
  , poolSize     = 10
  , tripcodeSalt = "changeme-in-production"
  }

data AppEnv = AppEnv
  { dbPool    :: ConnectionPool
  , appConfig :: AppConfig
  }

-- | The application monad: a Reader over Handler (which is ExceptT ServerError IO).
type App = ReaderT AppEnv Handler

-- | Run a database action in the App monad.
-- Uses IO as the base monad so MonadUnliftIO is satisfied.
runDB :: SqlPersistT IO a -> App a
runDB query = do
  pool <- asks dbPool
  liftIO $ runSqlPool query pool

-- | Throw a 404 with a JSON body.
throwNotFound :: Text -> App a
throwNotFound msg = throwError $ err404
  { errBody    = encode (object ["error" .= msg])
  , errHeaders = [(hContentType, "application/json")]
  }

-- | Throw a 400 with a JSON body.
throwBadRequest :: Text -> App a
throwBadRequest msg = throwError $ err400
  { errBody    = encode (object ["error" .= msg])
  , errHeaders = [(hContentType, "application/json")]
  }

-- | Throw a 403 with a JSON body.
throwForbidden :: Text -> App a
throwForbidden msg = throwError $ err403
  { errBody    = encode (object ["error" .= msg])
  , errHeaders = [(hContentType, "application/json")]
  }
