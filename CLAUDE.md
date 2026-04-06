# lambdachan — Claude context

This file gives Claude the context needed to work on lambdachan without a prior conversation. Read it fully before making changes.

---

## What this project is

A 4chan-style imageboard REST API. Key behavioural requirements:

- Anonymous posting (no account required to post or create threads)
- Tripcodes (`Name#pass` standard, `Name##pass` server-salted secure)
- Three user tiers: anonymous, moderator (per-board), admin (global)
- Images stored as BLOBs in the database (not on disk); returned base64-encoded in JSON
- 4chan-style thread mechanics: bump ordering, sticky, locked, bump limit (300 posts), OP deletion kills the thread
- DB backend switchable at runtime: SQLite (dev) or PostgreSQL (prod)

---

## Stack

| Layer | Library |
|---|---|
| HTTP framework | `servant` / `servant-server` / `warp` |
| Database ORM | `persistent` / `persistent-template` (schema + migrations) |
| DB backends | `persistent-sqlite` / `persistent-postgresql` |
| App monad | `ReaderT AppEnv Handler` (i.e. `ReaderT AppEnv (ExceptT ServerError IO)`) |
| Password hashing | `password` (bcrypt) |
| Session tokens | `uuid` (v4 random UUIDs stored in DB) |
| Tripcodes | `cryptohash-sha256` + `base64-bytestring` |
| Tests | `hspec` + `hspec-wai` + `QuickCheck` |
| GHC / LTS | 9.10.3 / LTS 24.36 |

---

## Module map

```
src/LambdaChan/
  Types.hs            UserRole (AdminRole|ModeratorRole), AuthUser, AppError
                      UserRole has ToJSON/FromJSON ("admin"/"moderator")
                      AuthUser carries Int64 IDs (not DB Key types) to avoid circular deps

  Config.hs           DatabaseBackend (SQLite FilePath | PostgreSQL Text)
                      AppConfig (dbBackend, serverPort, poolSize, tripcodeSalt)
                      AppEnv (dbPool :: ConnectionPool, appConfig :: AppConfig)
                      App = ReaderT AppEnv Handler
                      runDB :: SqlPersistT IO a -> App a   ← the key helper
                      throwNotFound / throwBadRequest / throwForbidden

  Database/
    Schema.hs         derivePersistField "UserRole"  ← stored as "AdminRole"/"ModeratorRole"
                      Entities: Board, User, ModeratorBoard, Thread, Post, Session
                      Generates: migrateAll, all EntityField constructors, all Unique constructors
                      NOTE: exports Post (DB entity) which clashes with Servant.Post (HTTP verb)
                            → always import this module qualified or hide Post

    Queries.hs        Pure SqlPersistT IO functions — no App monad here
                      Covers: listBoards, getBoardByName, createBoard, hardDeleteBoard,
                              getThreadsByBoard, getThread, createThread, softDeleteThread,
                              setThreadSticky, setThreadLocked, bumpThread,
                              getPostsByThread, getPost, getOpPost, createPost, softDeletePost,
                              getPostCount, listUsers, getUserByUsername, createUser, hardDeleteUser,
                              getUserModBoardIds, assignModToBoard, removeModFromBoard,
                              createSession, deleteSession, getSessionUser

  Auth.hs             computeTripcode :: ByteString -> Text  (! prefix, 10 base64 chars)
                      computeSecureTripcode :: Text -> ByteString -> Text  (!! prefix)
                      parseAuthorName :: Text -> Text -> (Text, Maybe Text)
                        first arg = server salt (from appConfig), second = raw author field
                      hashUserPassword :: Text -> IO Text
                      verifyUserPassword :: Text -> Text -> Bool
                      generateSessionToken :: IO Text
                      sessionDuration :: NominalDiffTime  (7 days)
                      requireAuth / requireAdmin / requireModOrAdmin :: Maybe Text -> App AuthUser
                        all take the raw Authorization header value (Maybe Text)

  API/
    Types.hs          LambdaChanAPI — the full 18-endpoint Servant type (see below)
                      lambdaChanAPI :: Proxy LambdaChanAPI
                      All request/response JSON types with manual ToJSON/FromJSON instances
                      boardToResponse :: Entity DB.Board -> BoardResponse
                      postToResponse  :: Entity DB.Post  -> PostResponse
                      IMPORTANT: imports Schema as `qualified ... as DB` to avoid Post clash

    Handlers.hs       appServer :: ServerT LambdaChanAPI App
                        — 18 handlers in exact order matching LambdaChanAPI
                      imports Servant with `hiding (Post)` to avoid clash with DB Post entity
                      bumpLimit = 300

  App.hs              mkApp :: AppEnv -> Application
                      initialisePool :: AppConfig -> IO ConnectionPool
                      runMigrations :: ConnectionPool -> IO ()
                      seedDatabase :: AppConfig -> ConnectionPool -> IO ()
                        — creates "admin"/"changeme" user if DB has no users; logs to stderr
                      runApp :: AppConfig -> IO ()

Lib.hs                re-exports runApp, defaultConfig, AppConfig, DatabaseBackend

app/Main.hs           reads DATABASE_URL / PORT / TRIPCODE_SALT env vars, calls runApp
```

---

## The API type (18 endpoints in order)

The handler order in `appServer` must match this type exactly or the Servant type system will reject it.

```
1.  GET    /boards                                          → [BoardResponse]
2.  POST   /boards                         [admin]          → BoardResponse
3.  GET    /boards/:board                                   → BoardCatalogResponse
4.  DELETE /boards/:board                  [admin]          → NoContent
5.  POST   /boards/:board/threads                           → ThreadResponse
6.  GET    /boards/:board/threads/:id                       → ThreadDetailResponse
7.  DELETE /boards/:board/threads/:id      [mod/admin]      → NoContent
8.  PATCH  /boards/:board/threads/:id/sticky [mod/admin]   → NoContent
9.  PATCH  /boards/:board/threads/:id/lock   [mod/admin]   → NoContent
10. POST   /boards/:board/threads/:id/posts                 → PostResponse
11. DELETE /boards/:board/threads/:id/posts/:pid [mod/admin] → NoContent
12. POST   /auth/login                                      → LoginResponse
13. POST   /auth/logout                    [bearer]         → NoContent
14. GET    /admin/users                    [admin]          → [UserResponse]
15. POST   /admin/users                    [admin]          → UserResponse
16. DELETE /admin/users/:id               [admin]          → NoContent
17. POST   /admin/users/:id/boards/:board [admin]          → NoContent
18. DELETE /admin/users/:id/boards/:board [admin]          → NoContent
```

Protected endpoints receive `Maybe Text` as the auth argument (the raw `Authorization` header). Call `requireAuth`, `requireAdmin`, or `requireModOrAdmin` to enforce access.

---

## Critical gotchas

### `Post` name clash
`LambdaChan.Database.Schema` exports a `Post` entity. `Servant` exports a `Post` HTTP verb type.
- In `API/Types.hs`: import Schema as `qualified LambdaChan.Database.Schema as DB`; use `DB.Post`, `DB.Board` etc. in converter functions.
- In `API/Handlers.hs`: `import Servant hiding (Post)`; import Schema unqualified.
- Do not import both `Servant` and `LambdaChan.Database.Schema` unqualified in the same module.

### `runDB` and `MonadUnliftIO`
`runSqlPool` requires `MonadUnliftIO`. `App = ReaderT AppEnv Handler` does NOT satisfy this because `Handler = ExceptT ServerError IO` lacks a `MonadUnliftIO` instance. The solution is to always run queries in `IO` and lift:
```haskell
runDB :: SqlPersistT IO a -> App a
runDB query = asks dbPool >>= liftIO . runSqlPool query
```
All query functions in `Queries.hs` have type `SqlPersistT IO a`.

### `errBody` is lazy `ByteString`
`ServerError.errBody :: Data.ByteString.Lazy.ByteString`. Use `Data.Aeson.encode` (which returns lazy BS) to build JSON error bodies, not `Data.Text.Encoding.encodeUtf8` (which returns strict BS).

### Persistent field naming
`persistent-template` generates field accessors as `<lowercaseEntityName><CapitalisedFieldName>`:
- `Board` → `boardName`, `boardTitle`, `boardCreatedAt`
- `Thread` → `threadBoardId`, `threadIsLocked`, `threadIsDeleted`
- `Post` → `postThreadId`, `postAuthorName`, `postImageData`
- `Session` → `sessionUserId`, `sessionToken`, `sessionExpiresAt`

`EntityField` constructors follow the same pattern with an uppercase first letter:
`BoardName`, `ThreadIsDeleted`, `PostBoardId`, etc.

### `fromSqlKey` / `toSqlKey`
Convert between `Key Record` (persistent) and `Int64` (used in JSON):
```haskell
import Database.Persist.Sql (fromSqlKey, toSqlKey)

fromSqlKey (entityKey boardEntity) :: Int64
toSqlKey (42 :: Int64) :: ThreadId
```

### `derivePersistField "UserRole"`
Stores `UserRole` as its `show` value: `"AdminRole"` or `"ModeratorRole"`. This is different from the JSON representation (`"admin"` / `"moderator"`). The `ToJSON`/`FromJSON` instances are defined manually in `Types.hs`.

---

## Testing approach

Tests use `hspec`. Each test gets a **fresh in-memory SQLite database** — no shared state, no cleanup.

```haskell
-- Pattern for DB tests
spec = around withTestPool $ do
  it "does something" $ \pool -> do
    result <- runSqlPool someQuery pool
    result `shouldBe` expected

-- Pattern for HTTP tests
spec = with withTestApp $ do
  it "returns 200" $
    get "/boards" `shouldRespondWith` 200
```

`withTestPool` (in `TestHelpers.hs`) creates a `:memory:` SQLite pool and runs migrations. `withTestApp` wraps that in a full WAI `Application`.

Run tests with `stack test`. Run a subset with `stack test --test-arguments "-m Auth"`.

---

## Adding new functionality

### New endpoint
1. Add to `LambdaChanAPI` type in `API/Types.hs`
2. Add corresponding handler to `appServer` in `API/Handlers.hs` — **position must match the type**
3. Implement the handler
4. Add tests

### New DB entity
1. Add to `[persistLowerCase|...|]` block in `Database/Schema.hs`
2. Add query functions in `Database/Queries.hs`
3. Migrations are automatic on startup

### New JSON type
Follow the existing manual `ToJSON`/`FromJSON` pattern (no Generic deriving used — avoids issues with field name prefixes and the `Post` clash).

---

## Running

```bash
stack build                   # build
stack test                    # test
stack exec lambdachan-exe     # run (SQLite, port 8080)

# PostgreSQL, custom port, custom salt
DATABASE_URL="host=localhost dbname=lambdachan user=app password=x" \
PORT=3000 \
TRIPCODE_SALT="$(openssl rand -hex 32)" \
stack exec lambdachan-exe
```
