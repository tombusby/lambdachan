{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.APISpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import Network.HTTP.Types (
  Header,
  hContentType,
  methodPost,
 )
import Test.Hspec
import Test.Hspec.Wai

import LambdaChan.TestHelpers (withTestApp)

-- | Standard JSON content-type header.
jsonCT :: Header
jsonCT = (hContentType, "application/json")

spec :: Spec
spec = with withTestApp $ do
  describe "GET /boards" $ do
    it "returns 200" $
      get "/boards" `shouldRespondWith` 200

  describe "POST /boards" $ do
    it "returns 401 without auth" $ do
      let body =
            encode
              ( object
                  [ "name" .= ("g" :: Text)
                  , "title" .= ("Technology" :: Text)
                  , "description" .= ("" :: Text)
                  ]
              )
      request methodPost "/boards" [jsonCT] body
        `shouldRespondWith` 401

  describe "GET /boards/:board" $ do
    it "returns 404 for unknown board" $
      get "/boards/nonexistent" `shouldRespondWith` 404

  describe "POST /auth/login" $ do
    it "returns 401 for missing user" $ do
      let body =
            encode
              ( object
                  [ "username" .= ("nobody" :: Text)
                  , "password" .= ("wrong" :: Text)
                  ]
              )
      request methodPost "/auth/login" [jsonCT] body
        `shouldRespondWith` 401

  describe "POST /boards/:board/threads" $ do
    it "returns 404 when board does not exist" $ do
      let body =
            encode
              ( object
                  [ "content" .= ("hello" :: Text)
                  , "authorName" .= ("Anonymous" :: Text)
                  ]
              )
      request methodPost "/boards/missing/threads" [jsonCT] body
        `shouldRespondWith` 404

-- Full board lifecycle (login → create → list → delete) is covered
-- by the database integration tests in DatabaseSpec.
