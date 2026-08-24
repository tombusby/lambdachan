# lambdachan — Claude context

This file gives Claude the context needed to work on lambdachan without a prior conversation. Read it fully before making changes.

---

## What this project is

A 4chan-style imageboard REST API with an Elm SPA frontend. Key behavioural requirements:

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
| Frontend | Elm (`Browser.application`), Vite dev server |
| GHC / LTS | 9.10.3 / LTS 24.36 |

---

## Module map

Backend modules under `src/LambdaChan/`. Only the non-obvious contents are noted —
read the module for the full export list.

- **`Types.hs`** — `UserRole`, `AuthUser`, `AppError`. `AuthUser` carries `Int64` IDs
  rather than DB `Key` types, deliberately, to avoid a circular dependency on Schema.
- **`Config.hs`** — `DatabaseBackend`, `AppConfig`, `AppEnv`, and `App = ReaderT AppEnv Handler`.
  Home of `runDB` (see gotchas) and the `throwNotFound` / `throwBadRequest` / `throwForbidden` helpers.
- **`Database/Schema.hs`** — the `persistLowerCase` block. Entities: Board, User,
  ModeratorBoard, Thread, Post, Session. Generates `migrateAll` plus all `EntityField`
  and `Unique` constructors. **Exports `Post`, which clashes with `Servant.Post`** — see gotchas.
- **`Database/Queries.hs`** — pure `SqlPersistT IO` functions, no `App` monad. One
  function per operation; names are predictable (`getBoardByName`, `softDeletePost`, …).
- **`Auth.hs`** — tripcode computation (`computeTripcode` `!`-prefixed, 10 base64 chars;
  `computeSecureTripcode` `!!`-prefixed), password hashing, session tokens
  (`sessionDuration` = 7 days), and the `requireAuth` / `requireAdmin` / `requireModOrAdmin`
  guards. All three guards take the **raw `Authorization` header value** as `Maybe Text`.
  `parseAuthorName` takes the server salt first, the raw author field second.
- **`API/Types.hs`** — the `LambdaChanAPI` Servant type and all JSON types, with manual
  `ToJSON`/`FromJSON` instances (no Generic deriving — avoids field-prefix issues and the
  `Post` clash). Imports Schema qualified as `DB`.
- **`API/Handlers.hs`** — `appServer`. Imports `Servant hiding (Post)`. `bumpLimit = 300`.
- **`App.hs`** — `mkApp`, `initialisePool`, `runMigrations`, `seedDatabase` (creates
  `admin`/`changeme` if the DB has no users), `runApp`.
- **`app/Main.hs`** — reads `DATABASE_URL` / `PORT` / `TRIPCODE_SALT`, calls `runApp`.

### The API type

`LambdaChanAPI` in `API/Types.hs` is the contract; read it there rather than a copy
that will drift. The rule that matters:

> **Handler order in `appServer` must match the endpoint order in `LambdaChanAPI` exactly**,
> or the Servant type system rejects it — usually with an error that points at the wrong line.

Protected endpoints receive the raw `Authorization` header as `Maybe Text`; enforce access
by calling one of the `require*` guards.

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
Follow the existing manual `ToJSON`/`FromJSON` pattern.

### New frontend page
1. Create `frontend/src/Page/NewPage.elm` with `Model`, `Msg`, `init`, `update`, `view`
2. Add a constructor to `Route` in `Route.elm` and `routeParser`
3. Add `NewPageModel NewPage.Model` to `PageModel` in `Main.elm`
4. Add `NewPageMsg NewPage.Msg` to `PageMsg` in `Main.elm`
5. Handle the new route in `routeToPage` and the new msg in `updatePage`/`viewPage`

---

## Frontend (Elm SPA)

`frontend/src/` — a `Browser.application` SPA styled to mimic 4chan's classic aesthetic.
The site name is **λchan** everywhere. `Main.elm` holds the root Model/Msg/update/view,
`Route.elm` the routes, `Types.elm` the API-mirroring types and decoders, `Api.elm` every
HTTP call, and `Page/` and `View/` the page and component modules.

The non-obvious decisions:

- **API base URL**: all `Http.request` calls use an `/api` prefix (e.g. `/api/boards`)
- **Dev proxy**: Vite proxies `/api/*` → `http://localhost:8080` (stripping the prefix), so the Haskell server needs no changes during dev
- **Prod routing**: `mkApp` in `App.hs` splits requests at the WAI level — `"api" : rest` → Servant (prefix stripped); everything else → `wai-app-static` serving `frontend/dist/` with `index.html` fallback for SPA deep links
- **Session**: stored as JSON in `localStorage` via the `storeSession` port; passed as a `Maybe String` flag on startup
- **Image upload**: uses `elm/file` + `File.toUrl` to get a data URL, strips the `data:<mime>;base64,` prefix, sends `{ data, filename, mimeType }` in the JSON body
- **No CSS framework**: hand-written `style.css` — `#d6daf0` blue-grey for post boxes and headers, `#117743` green for author names

`STATIC_DIR` (default `frontend/dist`) sets where static assets are served from.

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

# Frontend
make frontend-install   # first time only
make frontend-build     # Elm → dist/
make dev                # Vite on :5173 (HMR) + Haskell on :8080
```
