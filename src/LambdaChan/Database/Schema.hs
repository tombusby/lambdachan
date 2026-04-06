{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Persistent database schema for lambdachan.
Template Haskell generates entity types (Board, BoardId, etc.),
field accessors (boardName, threadIsLocked, etc.), EntityField
constructors, Unique constructors, and the migrateAll migration.
-}
module LambdaChan.Database.Schema where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH

import LambdaChan.Types (UserRole)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

Board
  name        Text          -- short name e.g. "g", "b", "pol"
  title       Text          -- full title e.g. "Technology"
  description Text
  createdAt   UTCTime
  UniqueBoard name
  deriving Show Eq

User
  username     Text
  passwordHash Text
  role         UserRole
  createdAt    UTCTime
  UniqueUsername username
  deriving Show Eq

-- Junction table: which boards a moderator is assigned to.
-- Admins are NOT listed here; they have global authority by role.
ModeratorBoard
  userId  UserId
  boardId BoardId
  UniqueModBoard userId boardId
  deriving Show Eq

Thread
  boardId   BoardId
  subject   Text Maybe
  bumpedAt  UTCTime       -- updated on each non-sage reply (controls catalog order)
  createdAt UTCTime
  isLocked  Bool          -- locked threads accept no new replies
  isSticky  Bool          -- sticky threads appear at top of catalog
  isDeleted Bool
  deriving Show Eq

Post
  threadId      ThreadId
  boardId       BoardId
  authorName    Text          -- "Anonymous" by default
  tripcode      Text Maybe    -- computed tripcode string e.g. "!AbCdEfGhIj"
  content       Text
  imageData     ByteString Maybe
  imageName     Text Maybe
  imageMimeType Text Maybe
  isDeleted     Bool
  createdAt     UTCTime
  deriving Show Eq

Session
  userId    UserId
  token     Text
  createdAt UTCTime
  expiresAt UTCTime
  UniqueToken token
  deriving Show Eq

|]
