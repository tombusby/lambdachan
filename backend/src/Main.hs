{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

import Prelude hiding (id)

import Data.Aeson
import Data.Aeson.Encoding
import Data.Text
import Network.Wai.Handler.Warp (run)
import Servant
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings(..), unsafeToPiece)

data Thread = Thread {
    threadId    :: Int
  , threadTime  :: Int
  , picURL      :: Text
  , threadTitle :: Text
  , threadText  :: Text
  , replies     :: [Comment]
  }

data Comment = Comment {
    commentId   :: Int
  , commentTime :: Int
  , mpicURL     :: Maybe Text
  , commentText :: Text
  }

instance ToJSON Thread where
  toJSON _ = Null -- Redundant - Avoids the need for `deriving Generic`
  toEncoding thread = pairs $
       "id"          .= threadId thread
    <> "time"        .= threadTime thread
    <> "picURL"      .= picURL thread
    <> "threadTitle" .= threadTitle thread
    <> "commentText" .= threadText thread
    <> "replies"     .= replies thread

instance ToJSON Comment where
  toJSON _ = Null -- Redundant - Avoids the need for `deriving Generic`
  toEncoding comemnt = pairs $
       "id"          .= commentId comemnt
    <> "time"        .= commentTime comemnt
    <> "mpicURL"     .= mpicURL comemnt
    <> "commentText" .= commentText comemnt

type API = "api" :> "v1" :> "thread" :> Capture "threadId" Int :> Get '[JSON] Thread
      :<|> Raw

server :: Server API
server = (\threadId -> return $ testThread threadId)
    :<|> serveDirectoryWith staticFileSettings
  where
    staticFileSettings = (defaultWebAppSettings "frontend/") {
        ssIndices = [unsafeToPiece "index.html"]
      }

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  let portNum = 8000
  putStrLn $ "Server running on " ++ show portNum
  run portNum app







testThread :: Int -> Thread
testThread threadId = Thread {
    threadId = threadId
  , threadTime = 1579547696000
  , picURL = "assets/img/banner.png"
  , threadTitle = "Aw shit it's the coronavirus"
  , threadText = "blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost "
      <> "dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg blah blah "
      <> "shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg blah "
      <> "blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg "
      <> "blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost "
      <> "dfgdfgdfgfdsgdsg sdfdsfd dsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg "
      <> "dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdf "
      <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgs dgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
      <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgs dfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsd "
      <> "gsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
      <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
      <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsdgsdgf blah blah "
      <> "shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg blah "
      <> "blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg "
      <> "blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost "
      <> "dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg blah blah "
      <> "shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg sdfdsfd"
      <> " dsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdf "
      <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgs dgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
      <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgs dfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsd "
      <> "gsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
      <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
      <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
      <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg dfgdgsdgsdgf"
    , replies = [
        Comment {
            commentId = 532453
          , commentTime = 1580465003000
          , mpicURL = Nothing
          , commentText = "blah blah shitpost"
        }
      , Comment {
            commentId = 345234
          , commentTime = 1581631692000
          , mpicURL = Just "assets/img/banner.png"
          , commentText = "blah blah shitpost dfgdfgdfgfdsgdsg blah blah "
              <> "shitpost dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg"
              <> " blah blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost "
              <> "dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg blah "
              <> "blah shitpost dfgdfgdfgfdsgdsg blah blah shitpost "
              <> "dfgdfgdfgfdsgdsg blah blah shitpost dfgdfgdfgfdsgdsg sdfdsfd "
              <> "dsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg dsfgdfsgsdfgsdg "
              <> "dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdf "
              <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg "
              <> "dsfgdfsgsdfgsdg dfgdgfdsgs dgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg"
              <> " dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgs dfgsfdg "
              <> "dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
              <> "dfgdgsd gsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg "
              <> "dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
              <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg "
              <> "dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
              <> "dfgdgsdgsdgfdsfgsdfgsdfgsdfgsfd dsfgdsfgsdfgsfdg "
              <> "dsfgdfsgsdfgsdg dfgdgfdsgsdgfdsg dfgdfggsdgsdgfsdgsdgdsgsdfg "
              <> "dfgdgsdgsdgf"
        }
      , Comment {
            commentId = 234523
          , commentTime = 1582901165000
          , mpicURL = Just "assets/img/banner.png"
          , commentText = "blah blah shitpost"
        }
    ]
  }
