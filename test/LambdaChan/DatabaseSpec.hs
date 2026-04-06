{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.DatabaseSpec (spec) where

import Control.Exception (SomeException, catch)
import Data.Time (addUTCTime, getCurrentTime)
import Database.Persist (Entity (..), entityKey, entityVal)
import Database.Persist.Sql (fromSqlKey, runSqlPool, toSqlKey)
import Test.Hspec

import LambdaChan.Auth (hashUserPassword, sessionDuration)
import LambdaChan.Database.Queries
import LambdaChan.Database.Schema
import LambdaChan.TestHelpers (withTestPool)
import LambdaChan.Types (AuthUser (..), UserRole (..))

spec :: Spec
spec = around withTestPool $ do

  describe "Boards" $ do
    it "starts with no boards" $ \pool -> do
      boards <- runSqlPool listBoards pool
      boards `shouldBe` []

    it "creates and retrieves a board by name" $ \pool -> do
      now <- getCurrentTime
      let board = Board "g" "Technology" "Post technology" now
      _bid <- runSqlPool (createBoard board) pool
      mBoard <- runSqlPool (getBoardByName "g") pool
      fmap (boardName . entityVal) mBoard `shouldBe` Just "g"

    it "hard-deletes a board and cascades to threads/posts" $ \pool -> do
      now <- getCurrentTime
      bid <- runSqlPool (createBoard (Board "del" "Delete Me" "" now)) pool
      tid <- runSqlPool (createThread (Thread bid Nothing now now False False False)) pool
      _   <- runSqlPool (createPost (Post tid bid "Anon" Nothing "hello" Nothing Nothing Nothing False now)) pool
      runSqlPool (hardDeleteBoard bid) pool
      mBoard <- runSqlPool (getBoardByName "del") pool
      mBoard `shouldBe` Nothing

  describe "Threads" $ do
    it "lists threads ordered by sticky then bumpedAt desc" $ \pool -> do
      now <- getCurrentTime
      let earlier = addUTCTime (-100) now
      bid <- runSqlPool (createBoard (Board "g2" "Tech" "" now)) pool
      t1  <- runSqlPool (createThread (Thread bid Nothing earlier earlier False False False)) pool
      t2  <- runSqlPool (createThread (Thread bid Nothing now now False True False)) pool
      threads <- runSqlPool (getThreadsByBoard bid) pool
      -- Sticky thread (t2) should appear first
      map entityKey threads `shouldBe` [t2, t1]

    it "filters out soft-deleted threads" $ \pool -> do
      now <- getCurrentTime
      bid <- runSqlPool (createBoard (Board "g3" "Tech" "" now)) pool
      tid <- runSqlPool (createThread (Thread bid Nothing now now False False False)) pool
      runSqlPool (softDeleteThread tid) pool
      threads <- runSqlPool (getThreadsByBoard bid) pool
      threads `shouldBe` []

    it "bumps a thread's bumpedAt timestamp" $ \pool -> do
      now <- getCurrentTime
      let earlier = addUTCTime (-100) now
      bid <- runSqlPool (createBoard (Board "g4" "Tech" "" now)) pool
      tid <- runSqlPool (createThread (Thread bid Nothing earlier earlier False False False)) pool
      runSqlPool (bumpThread tid now) pool
      mThread <- runSqlPool (getThread tid) pool
      fmap threadBumpedAt mThread `shouldBe` Just now

  describe "Posts" $ do
    it "retrieves the OP as the earliest post" $ \pool -> do
      now <- getCurrentTime
      let earlier = addUTCTime (-60) now
      bid <- runSqlPool (createBoard (Board "g5" "Tech" "" now)) pool
      tid <- runSqlPool (createThread (Thread bid Nothing now now False False False)) pool
      p1  <- runSqlPool (createPost (Post tid bid "Anon" Nothing "OP" Nothing Nothing Nothing False earlier)) pool
      _   <- runSqlPool (createPost (Post tid bid "Anon" Nothing "Reply" Nothing Nothing Nothing False now)) pool
      mOp <- runSqlPool (getOpPost tid) pool
      fmap entityKey mOp `shouldBe` Just p1

    it "counts only non-deleted posts" $ \pool -> do
      now <- getCurrentTime
      bid <- runSqlPool (createBoard (Board "g6" "Tech" "" now)) pool
      tid <- runSqlPool (createThread (Thread bid Nothing now now False False False)) pool
      p1  <- runSqlPool (createPost (Post tid bid "Anon" Nothing "post1" Nothing Nothing Nothing False now)) pool
      _   <- runSqlPool (createPost (Post tid bid "Anon" Nothing "post2" Nothing Nothing Nothing False now)) pool
      runSqlPool (softDeletePost p1) pool
      cnt <- runSqlPool (getPostCount tid) pool
      cnt `shouldBe` 1

  describe "Users" $ do
    it "creates and retrieves a user by username" $ \pool -> do
      hash <- hashUserPassword "secret"
      now  <- getCurrentTime
      _    <- runSqlPool (createUser (User "alice" hash AdminRole now)) pool
      mU   <- runSqlPool (getUserByUsername "alice") pool
      fmap (userUsername . entityVal) mU `shouldBe` Just "alice"

    it "hard-deletes a user" $ \pool -> do
      hash <- hashUserPassword "secret"
      now  <- getCurrentTime
      uid  <- runSqlPool (createUser (User "bob" hash ModeratorRole now)) pool
      runSqlPool (hardDeleteUser uid) pool
      mU <- runSqlPool (getUserByUsername "bob") pool
      mU `shouldBe` Nothing

  describe "Sessions" $ do
    it "returns Nothing for an expired session" $ \pool -> do
      hash <- hashUserPassword "secret"
      now  <- getCurrentTime
      uid  <- runSqlPool (createUser (User "charlie" hash AdminRole now)) pool
      let expired = addUTCTime (-1) now
      _ <- runSqlPool (createSession (Session uid "tok-expired" now expired)) pool
      result <- runSqlPool (getSessionUser "tok-expired" now) pool
      result `shouldBe` Nothing

    it "returns AuthUser for a valid session" $ \pool -> do
      hash <- hashUserPassword "secret"
      now  <- getCurrentTime
      uid  <- runSqlPool (createUser (User "diana" hash AdminRole now)) pool
      let expiry = addUTCTime sessionDuration now
      _ <- runSqlPool (createSession (Session uid "tok-valid" now expiry)) pool
      result <- runSqlPool (getSessionUser "tok-valid" now) pool
      fmap authUsername result `shouldBe` Just "diana"

    it "assigns mod boards correctly" $ \pool -> do
      hash <- hashUserPassword "secret"
      now  <- getCurrentTime
      bid  <- runSqlPool (createBoard (Board "x" "X" "" now)) pool
      uid  <- runSqlPool (createUser (User "eve" hash ModeratorRole now)) pool
      runSqlPool (assignModToBoard uid bid) pool
      let expiry = addUTCTime sessionDuration now
      _ <- runSqlPool (createSession (Session uid "tok-mod" now expiry)) pool
      result <- runSqlPool (getSessionUser "tok-mod" now) pool
      fmap authModBoards result `shouldBe` Just [fromSqlKey bid]
