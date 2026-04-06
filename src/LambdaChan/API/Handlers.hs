{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.API.Handlers (
  appServer,
) where

import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Database.Persist (Entity (..), entityKey, get)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Servant hiding (Post)

import LambdaChan.API.Types
import LambdaChan.Auth
import LambdaChan.Config
import LambdaChan.Database.Queries
import LambdaChan.Database.Schema
import LambdaChan.Types (UserRole (..))

-- | Maximum post count before a thread stops bumping.
bumpLimit :: Int
bumpLimit = 300

-- ---------------------------------------------------------------------------
-- Server wiring — order must match LambdaChanAPI type exactly
-- ---------------------------------------------------------------------------

appServer :: ServerT LambdaChanAPI App
appServer =
  listBoardsH
    :<|> createBoardH
    :<|> getBoardH
    :<|> deleteBoardH
    :<|> createThreadH
    :<|> getThreadH
    :<|> deleteThreadH
    :<|> setStickyH
    :<|> setLockedH
    :<|> createPostH
    :<|> deletePostH
    :<|> loginH
    :<|> logoutH
    :<|> adminListUsersH
    :<|> adminCreateUserH
    :<|> adminDeleteUserH
    :<|> adminAssignModH
    :<|> adminRemoveModH

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

getBoardOr404 :: Text -> App (Entity Board)
getBoardOr404 name = do
  mBoard <- runDB $ getBoardByName name
  case mBoard of
    Nothing -> throwNotFound $ "Board /" <> name <> "/ not found"
    Just b -> return b

decodeImage :: ImageUpload -> App (ByteString, Text, Text)
decodeImage img =
  case B64.decode (encodeUtf8 (imgBase64 img)) of
    Left err -> throwBadRequest $ "Invalid base64 image data: " <> T.pack err
    Right bs -> return (bs, imgFilename img, imgMimeType img)

buildThreadSummary :: Entity Thread -> App (Maybe ThreadSummary)
buildThreadSummary (Entity tid thread) = do
  mOp <- runDB $ getOpPost tid
  case mOp of
    Nothing -> return Nothing
    Just opEntity -> do
      cnt <- runDB $ getPostCount tid
      return $
        Just
          ThreadSummary
            { tsmId = fromSqlKey tid
            , tsmSubject = threadSubject thread
            , tsmBumpedAt = threadBumpedAt thread
            , tsmCreatedAt = threadCreatedAt thread
            , tsmIsLocked = threadIsLocked thread
            , tsmIsSticky = threadIsSticky thread
            , tsmPostCount = cnt
            , tsmOpPost = postToResponse opEntity
            }

-- Resolve a user's moderated board names for display in responses.
getModBoardNames :: UserId -> UserRole -> App [Text]
getModBoardNames uid role = case role of
  AdminRole -> return []
  ModeratorRole -> do
    boardIds <- runDB $ getUserModBoardIds uid
    boards <- runDB listBoards
    return
      [ boardName (entityVal b)
      | b@(Entity bid _) <- boards
      , fromSqlKey bid `elem` boardIds
      ]

extractBearerToken :: Text -> Text
extractBearerToken h = T.strip $ fromMaybe h (T.stripPrefix "Bearer " h)

-- ---------------------------------------------------------------------------
-- Board handlers
-- ---------------------------------------------------------------------------

listBoardsH :: App [BoardResponse]
listBoardsH = fmap (map boardToResponse) (runDB listBoards)

createBoardH :: Maybe Text -> CreateBoardRequest -> App BoardResponse
createBoardH auth req = do
  _admin <- requireAdmin auth
  mExisting <- runDB $ getBoardByName (cbrName req)
  when (isJust mExisting) $
    throwBadRequest $
      "Board /" <> cbrName req <> "/ already exists"
  now <- liftIO getCurrentTime
  let board =
        Board
          { boardName = cbrName req
          , boardTitle = cbrTitle req
          , boardDescription = cbrDescription req
          , boardCreatedAt = now
          }
  bid <- runDB $ createBoard board
  return $ boardToResponse (Entity bid board)

getBoardH :: Text -> App BoardCatalogResponse
getBoardH name = do
  boardEntity <- getBoardOr404 name
  threadEntities <- runDB $ getThreadsByBoard (entityKey boardEntity)
  summaries <- concatMaybes <$> forM threadEntities buildThreadSummary
  return
    BoardCatalogResponse
      { bcrBoard = boardToResponse boardEntity
      , bcrThreads = summaries
      }
 where
  concatMaybes = foldr (\x acc -> case x of Just v -> v : acc; Nothing -> acc) []

deleteBoardH :: Text -> Maybe Text -> App NoContent
deleteBoardH name auth = do
  _admin <- requireAdmin auth
  boardEntity <- getBoardOr404 name
  runDB $ hardDeleteBoard (entityKey boardEntity)
  return NoContent

-- ---------------------------------------------------------------------------
-- Thread handlers
-- ---------------------------------------------------------------------------

createThreadH :: Text -> CreateThreadRequest -> App ThreadResponse
createThreadH boardName' req = do
  boardEntity <- getBoardOr404 boardName'
  let bid = entityKey boardEntity
  salt <- asks (tripcodeSalt . appConfig)
  let (authorName, tripcode) = parseAuthorName salt (ctrAuthorName req)
  (imgBytes, imgName, imgMime) <- case ctrImage req of
    Nothing -> return (Nothing, Nothing, Nothing)
    Just img -> do
      (bs, fn, mt) <- decodeImage img
      return (Just bs, Just fn, Just mt)
  now <- liftIO getCurrentTime
  let thread =
        Thread
          { threadBoardId = bid
          , threadSubject = ctrSubject req
          , threadBumpedAt = now
          , threadCreatedAt = now
          , threadIsLocked = False
          , threadIsSticky = False
          , threadIsDeleted = False
          }
  tid <- runDB $ createThread thread
  let post =
        Post
          { postThreadId = tid
          , postBoardId = bid
          , postAuthorName = authorName
          , postTripcode = tripcode
          , postContent = ctrContent req
          , postImageData = imgBytes
          , postImageName = imgName
          , postImageMimeType = imgMime
          , postIsDeleted = False
          , postCreatedAt = now
          }
  pid <- runDB $ createPost post
  return
    ThreadResponse
      { thrId = fromSqlKey tid
      , thrBoardName = boardName'
      , thrSubject = ctrSubject req
      , thrCreatedAt = now
      , thrOpPost = postToResponse (Entity pid post)
      }

getThreadH :: Text -> Int64 -> App ThreadDetailResponse
getThreadH boardName' rawTid = do
  _board <- getBoardOr404 boardName'
  let tid = toSqlKey rawTid :: ThreadId
  mThread <- runDB $ getThread tid
  thread <- case mThread of
    Nothing -> throwNotFound "Thread not found"
    Just t -> return t
  when (threadIsDeleted thread) $ throwNotFound "Thread not found"
  posts <- runDB $ getPostsByThread tid
  cnt <- runDB $ getPostCount tid
  mOp <- runDB $ getOpPost tid
  opEnt <- case mOp of
    Nothing -> throwNotFound "Thread has no posts"
    Just e -> return e
  return
    ThreadDetailResponse
      { tdrSummary =
          ThreadSummary
            { tsmId = rawTid
            , tsmSubject = threadSubject thread
            , tsmBumpedAt = threadBumpedAt thread
            , tsmCreatedAt = threadCreatedAt thread
            , tsmIsLocked = threadIsLocked thread
            , tsmIsSticky = threadIsSticky thread
            , tsmPostCount = cnt
            , tsmOpPost = postToResponse opEnt
            }
      , tdrPosts = map postToResponse posts
      }

deleteThreadH :: Text -> Int64 -> Maybe Text -> App NoContent
deleteThreadH boardName' rawTid auth = do
  boardEntity <- getBoardOr404 boardName'
  _user <- requireModOrAdmin (fromSqlKey (entityKey boardEntity)) auth
  let tid = toSqlKey rawTid :: ThreadId
  mThread <- runDB $ getThread tid
  case mThread of
    Nothing -> throwNotFound "Thread not found"
    Just t -> when (threadIsDeleted t) $ throwNotFound "Thread not found"
  runDB $ softDeleteThread tid
  return NoContent

setStickyH :: Text -> Int64 -> Maybe Text -> ToggleRequest -> App NoContent
setStickyH boardName' rawTid auth req = do
  boardEntity <- getBoardOr404 boardName'
  _user <- requireModOrAdmin (fromSqlKey (entityKey boardEntity)) auth
  let tid = toSqlKey rawTid :: ThreadId
  mThread <- runDB $ getThread tid
  case mThread of
    Nothing -> throwNotFound "Thread not found"
    Just t -> when (threadIsDeleted t) $ throwNotFound "Thread not found"
  runDB $ setThreadSticky tid (togValue req)
  return NoContent

setLockedH :: Text -> Int64 -> Maybe Text -> ToggleRequest -> App NoContent
setLockedH boardName' rawTid auth req = do
  boardEntity <- getBoardOr404 boardName'
  _user <- requireModOrAdmin (fromSqlKey (entityKey boardEntity)) auth
  let tid = toSqlKey rawTid :: ThreadId
  mThread <- runDB $ getThread tid
  case mThread of
    Nothing -> throwNotFound "Thread not found"
    Just t -> when (threadIsDeleted t) $ throwNotFound "Thread not found"
  runDB $ setThreadLocked tid (togValue req)
  return NoContent

-- ---------------------------------------------------------------------------
-- Post handlers
-- ---------------------------------------------------------------------------

createPostH :: Text -> Int64 -> CreatePostRequest -> App PostResponse
createPostH boardName' rawTid req = do
  _board <- getBoardOr404 boardName'
  let tid = toSqlKey rawTid :: ThreadId
  mThread <- runDB $ getThread tid
  thread <- case mThread of
    Nothing -> throwNotFound "Thread not found"
    Just t -> return t
  when (threadIsDeleted thread) $ throwNotFound "Thread not found"
  when (threadIsLocked thread) $
    throwForbidden "This thread is locked and no longer accepts replies"
  salt <- asks (tripcodeSalt . appConfig)
  let (authorName, tripcode) = parseAuthorName salt (cprAuthorName req)
  (imgBytes, imgName, imgMime) <- case cprImage req of
    Nothing -> return (Nothing, Nothing, Nothing)
    Just img -> do
      (bs, fn, mt) <- decodeImage img
      return (Just bs, Just fn, Just mt)
  now <- liftIO getCurrentTime
  let post =
        Post
          { postThreadId = tid
          , postBoardId = threadBoardId thread
          , postAuthorName = authorName
          , postTripcode = tripcode
          , postContent = cprContent req
          , postImageData = imgBytes
          , postImageName = imgName
          , postImageMimeType = imgMime
          , postIsDeleted = False
          , postCreatedAt = now
          }
  pid <- runDB $ createPost post
  cnt <- runDB $ getPostCount tid
  when (cnt <= bumpLimit) $ runDB $ bumpThread tid now
  return $ postToResponse (Entity pid post)

deletePostH :: Text -> Int64 -> Int64 -> Maybe Text -> App NoContent
deletePostH boardName' rawTid rawPid auth = do
  boardEntity <- getBoardOr404 boardName'
  _user <- requireModOrAdmin (fromSqlKey (entityKey boardEntity)) auth
  let pid = toSqlKey rawPid :: PostId
  let tid = toSqlKey rawTid :: ThreadId
  mPost <- runDB $ getPost pid
  post <- case mPost of
    Nothing -> throwNotFound "Post not found"
    Just p -> return p
  when (postIsDeleted post) $ throwNotFound "Post not found"
  when (postThreadId post /= tid) $ throwNotFound "Post not in this thread"
  -- Deleting the OP deletes the whole thread (4chan behaviour).
  mOp <- runDB $ getOpPost tid
  case mOp of
    Just (Entity opId _) | opId == pid -> runDB $ softDeleteThread tid
    _ -> runDB $ softDeletePost pid
  return NoContent

-- ---------------------------------------------------------------------------
-- Auth handlers
-- ---------------------------------------------------------------------------

loginH :: LoginRequest -> App LoginResponse
loginH req = do
  mUserEntity <- runDB $ getUserByUsername (lreqUsername req)
  (Entity uid user) <- case mUserEntity of
    Nothing -> throwError err401{errBody = "{\"error\":\"Invalid credentials\"}"}
    Just e -> return e
  unless (verifyUserPassword (lreqPassword req) (userPasswordHash user)) $
    throwError err401{errBody = "{\"error\":\"Invalid credentials\"}"}
  tok <- liftIO generateSessionToken
  now <- liftIO getCurrentTime
  let session =
        Session
          { sessionUserId = uid
          , sessionToken = tok
          , sessionCreatedAt = now
          , sessionExpiresAt = addUTCTime sessionDuration now
          }
  _ <- runDB $ createSession session
  modNames <- getModBoardNames uid (userRole user)
  return
    LoginResponse
      { lresToken = tok
      , lresUser =
          UserResponse
            { urId = fromSqlKey uid
            , urUsername = userUsername user
            , urRole = userRole user
            , urModBoards = modNames
            }
      }

logoutH :: Maybe Text -> App NoContent
logoutH auth = do
  _user <- requireAuth auth
  case auth of
    Nothing -> return ()
    Just h -> runDB $ deleteSession (extractBearerToken h)
  return NoContent

-- ---------------------------------------------------------------------------
-- Admin handlers
-- ---------------------------------------------------------------------------

adminListUsersH :: Maybe Text -> App [UserResponse]
adminListUsersH auth = do
  _admin <- requireAdmin auth
  users <- runDB listUsers
  forM users $ \(Entity uid u) -> do
    names <- getModBoardNames uid (userRole u)
    return
      UserResponse
        { urId = fromSqlKey uid
        , urUsername = userUsername u
        , urRole = userRole u
        , urModBoards = names
        }

adminCreateUserH :: Maybe Text -> CreateUserRequest -> App UserResponse
adminCreateUserH auth req = do
  _admin <- requireAdmin auth
  mExisting <- runDB $ getUserByUsername (curUsername req)
  when (isJust mExisting) $
    throwBadRequest $
      "Username '" <> curUsername req <> "' is already taken"
  passHash <- liftIO $ hashUserPassword (curPassword req)
  now <- liftIO getCurrentTime
  let user =
        User
          { userUsername = curUsername req
          , userPasswordHash = passHash
          , userRole = curRole req
          , userCreatedAt = now
          }
  uid <- runDB $ createUser user
  return
    UserResponse
      { urId = fromSqlKey uid
      , urUsername = curUsername req
      , urRole = curRole req
      , urModBoards = []
      }

adminDeleteUserH :: Int64 -> Maybe Text -> App NoContent
adminDeleteUserH rawUid auth = do
  _admin <- requireAdmin auth
  runDB $ hardDeleteUser (toSqlKey rawUid)
  return NoContent

adminAssignModH :: Int64 -> Text -> Maybe Text -> App NoContent
adminAssignModH rawUid boardName' auth = do
  _admin <- requireAdmin auth
  let uid = toSqlKey rawUid :: UserId
  -- Verify the user exists.
  mUser <- runDB $ get uid
  case mUser of
    Nothing -> throwNotFound "User not found"
    Just _ -> return ()
  boardEntity <- getBoardOr404 boardName'
  runDB $ assignModToBoard uid (entityKey boardEntity)
  return NoContent

adminRemoveModH :: Int64 -> Text -> Maybe Text -> App NoContent
adminRemoveModH rawUid boardName' auth = do
  _admin <- requireAdmin auth
  let uid = toSqlKey rawUid :: UserId
  boardEntity <- getBoardOr404 boardName'
  runDB $ removeModFromBoard uid (entityKey boardEntity)
  return NoContent
