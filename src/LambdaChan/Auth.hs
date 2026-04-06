{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.Auth
  ( -- * Tripcodes
    computeTripcode
  , computeSecureTripcode
  , parseAuthorName
    -- * Password hashing
  , hashUserPassword
  , verifyUserPassword
    -- * Session management
  , generateSessionToken
  , sessionDuration
    -- * Request auth helpers
  , validateSessionToken
  , requireAuth
  , requireAdmin
  , requireModOrAdmin
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Int (Int64)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Password.Bcrypt
  ( PasswordCheck (..)
  , PasswordHash (..)
  , checkPassword
  , hashPassword
  , mkPassword
  , unPasswordHash
  )
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Types (hContentType)
import Servant (ServerError (..), err401, err403, throwError)

import LambdaChan.Config (App, runDB, appConfig, tripcodeSalt)
import LambdaChan.Database.Queries (deleteSession, getSessionUser)
import LambdaChan.Types (AuthUser (..), UserRole (..))

-- ---------------------------------------------------------------------------
-- Tripcodes
-- ---------------------------------------------------------------------------

-- | Compute a standard tripcode from a password.
-- Displayed as "!XXXXXXXXXX" (10 base64 chars).
computeTripcode :: ByteString -- ^ raw password bytes
               -> Text
computeTripcode passBytes =
  let hashed  = SHA256.hash passBytes
      encoded = decodeUtf8 (B64.encode hashed)
  in "!" <> T.take 10 encoded

-- | Compute a secure tripcode using a server-side salt.
-- Displayed as "!!XXXXXXXXXX".  The salt prevents tripcodes being computed
-- off-site (mirrors 4chan's ## behaviour).
computeSecureTripcode :: Text    -- ^ server salt
                      -> ByteString -- ^ raw password bytes
                      -> Text
computeSecureTripcode salt passBytes =
  let saltBytes = encodeUtf8 salt
      hashed    = SHA256.hash (passBytes <> saltBytes)
      encoded   = decodeUtf8 (B64.encode hashed)
  in "!!" <> T.take 10 encoded

-- | Parse a raw author name field that may contain a tripcode suffix.
--
-- @"Name##pass"@ → secure tripcode (server-salted)
-- @"Name#pass"@  → standard tripcode
-- @"Name"@       → no tripcode
--
-- Returns @(displayName, maybeTripcode)@.
parseAuthorName :: Text  -- ^ server salt (from AppConfig)
               -> Text  -- ^ raw author name from request
               -> (Text, Maybe Text)
parseAuthorName salt raw =
  -- Check for ## (secure) first, then # (standard).
  case T.breakOn "##" raw of
    (name, rest) | not (T.null rest) ->
      let pass = T.drop 2 rest
          trip = computeSecureTripcode salt (encodeUtf8 pass)
      in (T.strip name, Just trip)
    _ ->
      case T.breakOn "#" raw of
        (name, rest) | not (T.null rest) ->
          let pass = T.drop 1 rest
              trip = computeTripcode (encodeUtf8 pass)
          in (T.strip name, Just trip)
        (name, _) -> (T.strip name, Nothing)

-- ---------------------------------------------------------------------------
-- Password hashing (bcrypt via the `password` package)
-- ---------------------------------------------------------------------------

hashUserPassword :: Text -> IO Text
hashUserPassword pass = do
  hashed <- hashPassword (mkPassword pass)
  return (unPasswordHash hashed)

verifyUserPassword :: Text  -- ^ plaintext
                  -> Text  -- ^ stored hash
                  -> Bool
verifyUserPassword plain stored =
  case checkPassword (mkPassword plain) (PasswordHash stored) of
    PasswordCheckSuccess -> True
    PasswordCheckFail    -> False

-- ---------------------------------------------------------------------------
-- Session tokens
-- ---------------------------------------------------------------------------

-- | Sessions last 7 days.
sessionDuration :: NominalDiffTime
sessionDuration = 7 * 24 * 60 * 60

-- | Generate a random UUID v4 as a session token.
generateSessionToken :: IO Text
generateSessionToken = toText <$> nextRandom

-- ---------------------------------------------------------------------------
-- Request authentication helpers
-- ---------------------------------------------------------------------------

-- | Validate a raw Bearer token; returns the AuthUser if valid.
validateSessionToken :: Text -> App (Maybe AuthUser)
validateSessionToken tok = do
  now <- liftIO getCurrentTime
  runDB (getSessionUser tok now)

-- | Extract and strip "Bearer " prefix from an Authorization header value.
extractToken :: Text -> Text
extractToken h = case T.stripPrefix "Bearer " h of
  Just tok -> T.strip tok
  Nothing  -> T.strip h

jsonErr :: ServerError -> Text -> ServerError
jsonErr base msg = base
  { errBody    = encode (object ["error" .= msg])
  , errHeaders = [(hContentType, "application/json")]
  }

-- | Require a valid session token.  Throws 401 if missing or invalid.
requireAuth :: Maybe Text -> App AuthUser
requireAuth Nothing = throwError (jsonErr err401 "Authentication required")
requireAuth (Just h) = do
  let tok = extractToken h
  mUser <- validateSessionToken tok
  case mUser of
    Nothing   -> throwError (jsonErr err401 "Invalid or expired session token")
    Just user -> return user

-- | Require an admin session.  Throws 401/403 as appropriate.
requireAdmin :: Maybe Text -> App AuthUser
requireAdmin auth = do
  user <- requireAuth auth
  when (authUserRole user /= AdminRole) $
    throwError (jsonErr err403 "Admin privileges required")
  return user

-- | Require a moderator or admin for a specific board (by Int64 board ID).
-- Admins always pass; moderators must be assigned to the board.
requireModOrAdmin :: Int64  -- ^ board primary key
                 -> Maybe Text
                 -> App AuthUser
requireModOrAdmin bid auth = do
  user <- requireAuth auth
  case authUserRole user of
    AdminRole     -> return user
    ModeratorRole ->
      if bid `elem` authModBoards user
        then return user
        else throwError (jsonErr err403 "Moderator access to this board required")
