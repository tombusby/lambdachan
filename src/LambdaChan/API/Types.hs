{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LambdaChan.API.Types (
  -- * Servant API
  LambdaChanAPI,
  lambdaChanAPI,

  -- * Request types
  CreateBoardRequest (..),
  CreateThreadRequest (..),
  CreatePostRequest (..),
  ImageUpload (..),
  LoginRequest (..),
  CreateUserRequest (..),
  ToggleRequest (..),

  -- * Response types
  BoardResponse (..),
  BoardCatalogResponse (..),
  ThreadSummary (..),
  ThreadResponse (..),
  ThreadDetailResponse (..),
  PostResponse (..),
  LoginResponse (..),
  UserResponse (..),

  -- * Converters
  boardToResponse,
  postToResponse,
) where

import Data.Aeson
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Database.Persist (Entity (..))
import Database.Persist.Sql (fromSqlKey)
import Servant

-- Import Schema qualified so 'Post' (DB entity) doesn't clash with
-- 'Servant.Post' (HTTP verb).
import qualified LambdaChan.Database.Schema as DB
import LambdaChan.Types (UserRole)

-- ---------------------------------------------------------------------------
-- Servant API type
-- ---------------------------------------------------------------------------

type LambdaChanAPI =
  -- Boards
  "boards" :> Get '[JSON] [BoardResponse]
    :<|> "boards"
      :> Header "Authorization" Text
      :> ReqBody '[JSON] CreateBoardRequest
      :> Post '[JSON] BoardResponse
    :<|> "boards"
      :> Capture "board" Text
      :> Get '[JSON] BoardCatalogResponse
    :<|> "boards"
      :> Capture "board" Text
      :> Header "Authorization" Text
      :> Delete '[JSON] NoContent
    -- Threads (anonymous creation)
    :<|> "boards"
      :> Capture "board" Text
      :> "threads"
      :> ReqBody '[JSON] CreateThreadRequest
      :> Post '[JSON] ThreadResponse
    :<|> "boards"
      :> Capture "board" Text
      :> "threads"
      :> Capture "threadId" Int64
      :> Get '[JSON] ThreadDetailResponse
    :<|> "boards"
      :> Capture "board" Text
      :> "threads"
      :> Capture "threadId" Int64
      :> Header "Authorization" Text
      :> Delete '[JSON] NoContent
    :<|> "boards"
      :> Capture "board" Text
      :> "threads"
      :> Capture "threadId" Int64
      :> "sticky"
      :> Header "Authorization" Text
      :> ReqBody '[JSON] ToggleRequest
      :> Patch '[JSON] NoContent
    :<|> "boards"
      :> Capture "board" Text
      :> "threads"
      :> Capture "threadId" Int64
      :> "lock"
      :> Header "Authorization" Text
      :> ReqBody '[JSON] ToggleRequest
      :> Patch '[JSON] NoContent
    -- Posts (anonymous creation)
    :<|> "boards"
      :> Capture "board" Text
      :> "threads"
      :> Capture "threadId" Int64
      :> "posts"
      :> ReqBody '[JSON] CreatePostRequest
      :> Post '[JSON] PostResponse
    :<|> "boards"
      :> Capture "board" Text
      :> "threads"
      :> Capture "threadId" Int64
      :> "posts"
      :> Capture "postId" Int64
      :> Header "Authorization" Text
      :> Delete '[JSON] NoContent
    -- Auth
    :<|> "auth" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
    :<|> "auth" :> "logout" :> Header "Authorization" Text :> Post '[JSON] NoContent
    -- Admin: user management
    :<|> "admin"
      :> "users"
      :> Header "Authorization" Text
      :> Get '[JSON] [UserResponse]
    :<|> "admin"
      :> "users"
      :> Header "Authorization" Text
      :> ReqBody '[JSON] CreateUserRequest
      :> Post '[JSON] UserResponse
    :<|> "admin"
      :> "users"
      :> Capture "userId" Int64
      :> Header "Authorization" Text
      :> Delete '[JSON] NoContent
    :<|> "admin"
      :> "users"
      :> Capture "userId" Int64
      :> "boards"
      :> Capture "boardName" Text
      :> Header "Authorization" Text
      :> Post '[JSON] NoContent
    :<|> "admin"
      :> "users"
      :> Capture "userId" Int64
      :> "boards"
      :> Capture "boardName" Text
      :> Header "Authorization" Text
      :> Delete '[JSON] NoContent

lambdaChanAPI :: Proxy LambdaChanAPI
lambdaChanAPI = Proxy

-- ---------------------------------------------------------------------------
-- Request types
-- ---------------------------------------------------------------------------

data CreateBoardRequest = CreateBoardRequest
  { cbrName :: Text
  , cbrTitle :: Text
  , cbrDescription :: Text
  }
  deriving (Show, Eq)

instance FromJSON CreateBoardRequest where
  parseJSON = withObject "CreateBoardRequest" $ \v ->
    CreateBoardRequest
      <$> v .: "name"
      <*> v .: "title"
      <*> v .: "description"

instance ToJSON CreateBoardRequest where
  toJSON r =
    object
      [ "name" .= cbrName r
      , "title" .= cbrTitle r
      , "description" .= cbrDescription r
      ]

{- | Image included in a post or thread creation request.
@data@ must be a base64-encoded string of the raw image bytes.
-}
data ImageUpload = ImageUpload
  { imgBase64 :: Text
  , imgFilename :: Text
  , imgMimeType :: Text
  }
  deriving (Show, Eq)

instance FromJSON ImageUpload where
  parseJSON = withObject "ImageUpload" $ \v ->
    ImageUpload
      <$> v .: "data"
      <*> v .: "filename"
      <*> v .: "mimeType"

instance ToJSON ImageUpload where
  toJSON i =
    object
      [ "data" .= imgBase64 i
      , "filename" .= imgFilename i
      , "mimeType" .= imgMimeType i
      ]

data CreateThreadRequest = CreateThreadRequest
  { ctrSubject :: Maybe Text
  , ctrContent :: Text
  , ctrAuthorName :: Text
  , ctrImage :: Maybe ImageUpload
  }
  deriving (Show, Eq)

instance FromJSON CreateThreadRequest where
  parseJSON = withObject "CreateThreadRequest" $ \v ->
    CreateThreadRequest
      <$> v .:? "subject"
      <*> v .: "content"
      <*> (v .:? "authorName" >>= maybe (pure "Anonymous") pure)
      <*> v .:? "image"

instance ToJSON CreateThreadRequest where
  toJSON r =
    object
      [ "subject" .= ctrSubject r
      , "content" .= ctrContent r
      , "authorName" .= ctrAuthorName r
      , "image" .= ctrImage r
      ]

data CreatePostRequest = CreatePostRequest
  { cprContent :: Text
  , cprAuthorName :: Text
  , cprImage :: Maybe ImageUpload
  }
  deriving (Show, Eq)

instance FromJSON CreatePostRequest where
  parseJSON = withObject "CreatePostRequest" $ \v ->
    CreatePostRequest
      <$> v .: "content"
      <*> (v .:? "authorName" >>= maybe (pure "Anonymous") pure)
      <*> v .:? "image"

instance ToJSON CreatePostRequest where
  toJSON r =
    object
      [ "content" .= cprContent r
      , "authorName" .= cprAuthorName r
      , "image" .= cprImage r
      ]

data LoginRequest = LoginRequest
  { lreqUsername :: Text
  , lreqPassword :: Text
  }
  deriving (Show, Eq)

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \v ->
    LoginRequest <$> v .: "username" <*> v .: "password"

instance ToJSON LoginRequest where
  toJSON r = object ["username" .= lreqUsername r, "password" .= lreqPassword r]

data CreateUserRequest = CreateUserRequest
  { curUsername :: Text
  , curPassword :: Text
  , curRole :: UserRole
  }
  deriving (Show, Eq)

instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \v ->
    CreateUserRequest
      <$> v .: "username"
      <*> v .: "password"
      <*> v .: "role"

instance ToJSON CreateUserRequest where
  toJSON r =
    object
      ["username" .= curUsername r, "password" .= curPassword r, "role" .= curRole r]

data ToggleRequest = ToggleRequest
  { togValue :: Bool
  }
  deriving (Show, Eq)

instance FromJSON ToggleRequest where
  parseJSON = withObject "ToggleRequest" $ \v -> ToggleRequest <$> v .: "value"

instance ToJSON ToggleRequest where
  toJSON r = object ["value" .= togValue r]

-- ---------------------------------------------------------------------------
-- Response types
-- ---------------------------------------------------------------------------

data BoardResponse = BoardResponse
  { brdId :: Int64
  , brdName :: Text
  , brdTitle :: Text
  , brdDescription :: Text
  , brdCreatedAt :: UTCTime
  }
  deriving (Show, Eq)

instance ToJSON BoardResponse where
  toJSON r =
    object
      [ "id" .= brdId r
      , "name" .= brdName r
      , "title" .= brdTitle r
      , "description" .= brdDescription r
      , "createdAt" .= brdCreatedAt r
      ]

instance FromJSON BoardResponse where
  parseJSON = withObject "BoardResponse" $ \v ->
    BoardResponse
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "title"
      <*> v .: "description"
      <*> v .: "createdAt"

-- | A post as returned in API responses. Image bytes are base64-encoded.
data PostResponse = PostResponse
  { prsId :: Int64
  , prsAuthorName :: Text
  , prsTripcode :: Maybe Text
  , prsContent :: Text
  , prsImageData :: Maybe Text
  , prsImageName :: Maybe Text
  , prsImageMimeType :: Maybe Text
  , prsCreatedAt :: UTCTime
  }
  deriving (Show, Eq)

instance ToJSON PostResponse where
  toJSON r =
    object
      [ "id" .= prsId r
      , "authorName" .= prsAuthorName r
      , "tripcode" .= prsTripcode r
      , "content" .= prsContent r
      , "imageData" .= prsImageData r
      , "imageName" .= prsImageName r
      , "imageMimeType" .= prsImageMimeType r
      , "createdAt" .= prsCreatedAt r
      ]

instance FromJSON PostResponse where
  parseJSON = withObject "PostResponse" $ \v ->
    PostResponse
      <$> v .: "id"
      <*> v .: "authorName"
      <*> v .:? "tripcode"
      <*> v .: "content"
      <*> v .:? "imageData"
      <*> v .:? "imageName"
      <*> v .:? "imageMimeType"
      <*> v .: "createdAt"

data ThreadSummary = ThreadSummary
  { tsmId :: Int64
  , tsmSubject :: Maybe Text
  , tsmBumpedAt :: UTCTime
  , tsmCreatedAt :: UTCTime
  , tsmIsLocked :: Bool
  , tsmIsSticky :: Bool
  , tsmPostCount :: Int
  , tsmOpPost :: PostResponse
  }
  deriving (Show, Eq)

instance ToJSON ThreadSummary where
  toJSON r =
    object
      [ "id" .= tsmId r
      , "subject" .= tsmSubject r
      , "bumpedAt" .= tsmBumpedAt r
      , "createdAt" .= tsmCreatedAt r
      , "isLocked" .= tsmIsLocked r
      , "isSticky" .= tsmIsSticky r
      , "postCount" .= tsmPostCount r
      , "opPost" .= tsmOpPost r
      ]

instance FromJSON ThreadSummary where
  parseJSON = withObject "ThreadSummary" $ \v ->
    ThreadSummary
      <$> v .: "id"
      <*> v .:? "subject"
      <*> v .: "bumpedAt"
      <*> v .: "createdAt"
      <*> v .: "isLocked"
      <*> v .: "isSticky"
      <*> v .: "postCount"
      <*> v .: "opPost"

data BoardCatalogResponse = BoardCatalogResponse
  { bcrBoard :: BoardResponse
  , bcrThreads :: [ThreadSummary]
  }
  deriving (Show, Eq)

instance ToJSON BoardCatalogResponse where
  toJSON r = object ["board" .= bcrBoard r, "threads" .= bcrThreads r]

instance FromJSON BoardCatalogResponse where
  parseJSON = withObject "BoardCatalogResponse" $ \v ->
    BoardCatalogResponse <$> v .: "board" <*> v .: "threads"

-- | Returned after successfully creating a new thread.
data ThreadResponse = ThreadResponse
  { thrId :: Int64
  , thrBoardName :: Text
  , thrSubject :: Maybe Text
  , thrCreatedAt :: UTCTime
  , thrOpPost :: PostResponse
  }
  deriving (Show, Eq)

instance ToJSON ThreadResponse where
  toJSON r =
    object
      [ "id" .= thrId r
      , "boardName" .= thrBoardName r
      , "subject" .= thrSubject r
      , "createdAt" .= thrCreatedAt r
      , "opPost" .= thrOpPost r
      ]

instance FromJSON ThreadResponse where
  parseJSON = withObject "ThreadResponse" $ \v ->
    ThreadResponse
      <$> v .: "id"
      <*> v .: "boardName"
      <*> v .:? "subject"
      <*> v .: "createdAt"
      <*> v .: "opPost"

-- | Full thread view: metadata + all posts.
data ThreadDetailResponse = ThreadDetailResponse
  { tdrSummary :: ThreadSummary
  , tdrPosts :: [PostResponse]
  }
  deriving (Show, Eq)

instance ToJSON ThreadDetailResponse where
  toJSON r = object ["thread" .= tdrSummary r, "posts" .= tdrPosts r]

instance FromJSON ThreadDetailResponse where
  parseJSON = withObject "ThreadDetailResponse" $ \v ->
    ThreadDetailResponse <$> v .: "thread" <*> v .: "posts"

data UserResponse = UserResponse
  { urId :: Int64
  , urUsername :: Text
  , urRole :: UserRole
  , urModBoards :: [Text]
  }
  deriving (Show, Eq)

instance ToJSON UserResponse where
  toJSON r =
    object
      [ "id" .= urId r
      , "username" .= urUsername r
      , "role" .= urRole r
      , "modBoards" .= urModBoards r
      ]

instance FromJSON UserResponse where
  parseJSON = withObject "UserResponse" $ \v ->
    UserResponse
      <$> v .: "id"
      <*> v .: "username"
      <*> v .: "role"
      <*> v .: "modBoards"

data LoginResponse = LoginResponse
  { lresToken :: Text
  , lresUser :: UserResponse
  }
  deriving (Show, Eq)

instance ToJSON LoginResponse where
  toJSON r = object ["token" .= lresToken r, "user" .= lresUser r]

instance FromJSON LoginResponse where
  parseJSON = withObject "LoginResponse" $ \v ->
    LoginResponse <$> v .: "token" <*> v .: "user"

-- ---------------------------------------------------------------------------
-- DB entity → response converters
-- ---------------------------------------------------------------------------

boardToResponse :: Entity DB.Board -> BoardResponse
boardToResponse (Entity k b) =
  BoardResponse
    { brdId = fromSqlKey k
    , brdName = DB.boardName b
    , brdTitle = DB.boardTitle b
    , brdDescription = DB.boardDescription b
    , brdCreatedAt = DB.boardCreatedAt b
    }

postToResponse :: Entity DB.Post -> PostResponse
postToResponse (Entity k p) =
  PostResponse
    { prsId = fromSqlKey k
    , prsAuthorName = DB.postAuthorName p
    , prsTripcode = DB.postTripcode p
    , prsContent = DB.postContent p
    , prsImageData = fmap (decodeUtf8 . B64.encode) (DB.postImageData p)
    , prsImageName = DB.postImageName p
    , prsImageMimeType = DB.postImageMimeType p
    , prsCreatedAt = DB.postCreatedAt p
    }
