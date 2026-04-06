{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaChan.Types (
  UserRole (..),
  AppError (..),
  AuthUser (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.TH

-- | User roles in the system. Anonymous users have no role and post without auth.
data UserRole = AdminRole | ModeratorRole
  deriving (Show, Read, Eq, Ord)

derivePersistField "UserRole"

instance ToJSON UserRole where
  toJSON AdminRole = toJSON ("admin" :: Text)
  toJSON ModeratorRole = toJSON ("moderator" :: Text)

instance FromJSON UserRole where
  parseJSON = withText "UserRole" $ \case
    "admin" -> pure AdminRole
    "moderator" -> pure ModeratorRole
    _ -> fail "Expected 'admin' or 'moderator'"

-- | Authenticated user context extracted from a valid session token.
data AuthUser = AuthUser
  { authUserId :: Int64
  , authUsername :: Text
  , authUserRole :: UserRole
  , authModBoards :: [Int64]
  -- ^ Board IDs this user can moderate (empty for admins who have all)
  }
  deriving (Show, Eq)

data AppError
  = AppNotFound Text
  | AppUnauthorized
  | AppForbidden Text
  | AppBadRequest Text
  | AppInternalError Text
  deriving (Show, Eq)
