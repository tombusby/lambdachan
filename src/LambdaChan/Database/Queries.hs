module LambdaChan.Database.Queries
  ( -- * Boards
    listBoards
  , getBoardByName
  , createBoard
  , hardDeleteBoard
    -- * Threads
  , getThreadsByBoard
  , getThread
  , createThread
  , softDeleteThread
  , setThreadSticky
  , setThreadLocked
  , bumpThread
    -- * Posts
  , getPostsByThread
  , getPost
  , getOpPost
  , createPost
  , softDeletePost
  , getPostCount
    -- * Users
  , listUsers
  , getUserByUsername
  , createUser
  , hardDeleteUser
  , getUserModBoardIds
    -- * Moderator board assignments
  , assignModToBoard
  , removeModFromBoard
    -- * Sessions
  , createSession
  , deleteSession
  , getSessionUser
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, fromSqlKey)

import LambdaChan.Database.Schema
import LambdaChan.Types (AuthUser (..), UserRole (AdminRole))

-- ---------------------------------------------------------------------------
-- Boards
-- ---------------------------------------------------------------------------

listBoards :: SqlPersistT IO [Entity Board]
listBoards = selectList [] [Asc BoardName]

getBoardByName :: Text -> SqlPersistT IO (Maybe (Entity Board))
getBoardByName name = getBy (UniqueBoard name)

createBoard :: Board -> SqlPersistT IO BoardId
createBoard = insert

-- | Hard-delete a board and cascade to all threads, posts, and moderator
-- assignments.  Posts are deleted first to satisfy FK constraints.
hardDeleteBoard :: BoardId -> SqlPersistT IO ()
hardDeleteBoard bid = do
  deleteWhere [PostBoardId ==. bid]
  deleteWhere [ThreadBoardId ==. bid]
  deleteWhere [ModeratorBoardBoardId ==. bid]
  delete bid

-- ---------------------------------------------------------------------------
-- Threads
-- ---------------------------------------------------------------------------

-- | Threads for a board ordered stickies-first then by bump time descending.
getThreadsByBoard :: BoardId -> SqlPersistT IO [Entity Thread]
getThreadsByBoard bid =
  selectList [ThreadBoardId ==. bid, ThreadIsDeleted ==. False]
             [Desc ThreadIsSticky, Desc ThreadBumpedAt]

getThread :: ThreadId -> SqlPersistT IO (Maybe Thread)
getThread = get

createThread :: Thread -> SqlPersistT IO ThreadId
createThread = insert

softDeleteThread :: ThreadId -> SqlPersistT IO ()
softDeleteThread tid = update tid [ThreadIsDeleted =. True]

setThreadSticky :: ThreadId -> Bool -> SqlPersistT IO ()
setThreadSticky tid val = update tid [ThreadIsSticky =. val]

setThreadLocked :: ThreadId -> Bool -> SqlPersistT IO ()
setThreadLocked tid val = update tid [ThreadIsLocked =. val]

bumpThread :: ThreadId -> UTCTime -> SqlPersistT IO ()
bumpThread tid now = update tid [ThreadBumpedAt =. now]

-- ---------------------------------------------------------------------------
-- Posts
-- ---------------------------------------------------------------------------

-- | All non-deleted posts in a thread, oldest first.
getPostsByThread :: ThreadId -> SqlPersistT IO [Entity Post]
getPostsByThread tid =
  selectList [PostThreadId ==. tid, PostIsDeleted ==. False] [Asc PostCreatedAt]

getPost :: PostId -> SqlPersistT IO (Maybe Post)
getPost = get

-- | The first (OP) post in a thread.
getOpPost :: ThreadId -> SqlPersistT IO (Maybe (Entity Post))
getOpPost tid = do
  results <- selectList [PostThreadId ==. tid, PostIsDeleted ==. False]
                        [Asc PostCreatedAt, LimitTo 1]
  return $ case results of
    (p : _) -> Just p
    []      -> Nothing

createPost :: Post -> SqlPersistT IO PostId
createPost = insert

softDeletePost :: PostId -> SqlPersistT IO ()
softDeletePost pid = update pid [PostIsDeleted =. True]

-- | Count non-deleted posts in a thread.
getPostCount :: ThreadId -> SqlPersistT IO Int
getPostCount tid = count [PostThreadId ==. tid, PostIsDeleted ==. False]

-- ---------------------------------------------------------------------------
-- Users
-- ---------------------------------------------------------------------------

listUsers :: SqlPersistT IO [Entity User]
listUsers = selectList [] [Asc UserUsername]

getUserByUsername :: Text -> SqlPersistT IO (Maybe (Entity User))
getUserByUsername uname = getBy (UniqueUsername uname)

createUser :: User -> SqlPersistT IO UserId
createUser = insert

hardDeleteUser :: UserId -> SqlPersistT IO ()
hardDeleteUser uid = do
  deleteWhere [ModeratorBoardUserId ==. uid]
  deleteWhere [SessionUserId ==. uid]
  delete uid

-- | Int64 board IDs that a user is assigned to moderate.
getUserModBoardIds :: UserId -> SqlPersistT IO [Int64]
getUserModBoardIds uid = do
  assignments <- selectList [ModeratorBoardUserId ==. uid] []
  return $ map (fromSqlKey . moderatorBoardBoardId . entityVal) assignments

-- ---------------------------------------------------------------------------
-- Moderator board assignments
-- ---------------------------------------------------------------------------

assignModToBoard :: UserId -> BoardId -> SqlPersistT IO ()
assignModToBoard uid bid = do
  _ <- insertUnique (ModeratorBoard uid bid)
  return ()

removeModFromBoard :: UserId -> BoardId -> SqlPersistT IO ()
removeModFromBoard uid bid = deleteBy (UniqueModBoard uid bid)

-- ---------------------------------------------------------------------------
-- Sessions
-- ---------------------------------------------------------------------------

createSession :: Session -> SqlPersistT IO SessionId
createSession = insert

deleteSession :: Text -> SqlPersistT IO ()
deleteSession tok = deleteBy (UniqueToken tok)

-- | Look up a session by token; returns the associated AuthUser if the session
-- exists and has not expired.
getSessionUser :: Text -> UTCTime -> SqlPersistT IO (Maybe AuthUser)
getSessionUser tok now = do
  mSession <- getBy (UniqueToken tok)
  case mSession of
    Nothing -> return Nothing
    Just (Entity _ session)
      | sessionExpiresAt session <= now -> return Nothing
      | otherwise -> do
          mUser <- get (sessionUserId session)
          case mUser of
            Nothing   -> return Nothing
            Just user -> do
              boardIds <- getUserModBoardIds (sessionUserId session)
              -- Admins have global authority; we still record [] for mod-boards.
              let modBoards = case userRole user of
                    AdminRole -> []
                    _         -> boardIds
              return $ Just AuthUser
                { authUserId    = fromSqlKey (sessionUserId session)
                , authUsername  = userUsername user
                , authUserRole  = userRole user
                , authModBoards = modBoards
                }
