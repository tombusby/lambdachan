# λchan

A 4chan-style imageboard built with [Servant](https://haskell-servant.github.io/) (Haskell) and [Elm](https://elm-lang.org/). Anonymous posting, tripcodes, per-board moderation, configurable database backends (SQLite for dev, PostgreSQL for production), and a classic 4chan-aesthetic frontend.

---

## Contents

- [Features](#features)
- [Quick start](#quick-start)
- [Configuration](#configuration)
- [Frontend](#frontend)
- [API reference](#api-reference)
- [Authentication](#authentication)
- [Tripcodes](#tripcodes)
- [User roles](#user-roles)
- [Database backends](#database-backends)
- [Development](#development)
- [Testing](#testing)
- [Project structure](#project-structure)

---

## Features

- **Anonymous posting** — no account required to create threads or reply, matching 4chan behaviour
- **Tripcodes** — `Name#password` (standard) and `Name##password` (secure, server-salted)
- **Three-tier auth** — anonymous users, per-board moderators, and global admins
- **Images in the database** — image bytes stored as BLOBs; returned base64-encoded in JSON
- **Thread mechanics** — bump ordering, sticky threads, locked threads, bump limit (300 posts)
- **Soft deletes** — moderated posts/threads are flagged `isDeleted`; deleting the OP removes the whole thread
- **Configurable DB** — SQLite (zero config) or PostgreSQL via connection string
- **Auto-seeded admin** — a default admin account is created on first run if no users exist
- **Elm SPA** — a 4chan-style frontend served from the same binary; no separate web server required

---

## Quick start

### Prerequisites

- [Stack](https://docs.haskellstack.org/) (installs GHC automatically)
- [Elm 0.19.1](https://guide.elm-lang.org/install/elm.html) — for building the frontend
- For PostgreSQL support: `libpq` system library (`brew install libpq` on macOS, `apt install libpq-dev` on Debian/Ubuntu)

### Build and run (SQLite, with frontend)

```bash
git clone <repo>
cd lambdachan
stack build
make elm-build          # compiles Elm → frontend/dist/
stack exec lambdachan-exe
```

Then open `http://localhost:8080` in a browser.

On first run with an empty database you will see:

```
==========================================
  No users found — creating default admin
  Username: admin
  Password: changeme
  CHANGE THIS PASSWORD IMMEDIATELY
==========================================
lambdachan listening on port 8080
```

Log in at `http://localhost:8080/login` and change the password immediately via the admin panel.

### Run with PostgreSQL

```bash
make elm-build
DATABASE_URL="host=localhost dbname=lambdachan user=postgres password=secret" \
  stack exec lambdachan-exe
```

---

## Configuration

All configuration is via environment variables. Defaults are shown.

| Variable | Default | Description |
|---|---|---|
| `DATABASE_URL` | `lambdachan.db` (SQLite) | SQLite file path **or** PostgreSQL connection string (must contain `postgres`) |
| `PORT` | `8080` | HTTP port to listen on |
| `TRIPCODE_SALT` | `changeme-in-production` | Server-side salt for `##` secure tripcodes — **set a random value in production** |
| `STATIC_DIR` | `frontend/dist` | Directory to serve compiled frontend assets from |

Example PostgreSQL connection string: `host=db.example.com port=5432 dbname=lambdachan user=app password=hunter2 sslmode=require`

---

## Frontend

The frontend is an Elm 0.19.1 SPA styled to mimic 4chan's classic aesthetic. It is served directly by the Haskell binary — no separate web server or reverse proxy needed.

### How routing works

All requests whose path begins with `/api/` are routed to the Servant REST API (with the `/api` prefix stripped). Everything else is served as static files from `frontend/dist/`, with `index.html` as the fallback for client-side routes.

```
http://localhost:8080/api/boards   → Servant handler
http://localhost:8080/b/g/42       → frontend/dist/index.html (Elm router takes over)
http://localhost:8080/             → frontend/dist/index.html
```

### Building the frontend

**Without npm (using `elm` directly):**

```bash
make elm-build      # compiles Elm, copies CSS and index.html to frontend/dist/
```

**With npm (using Vite):**

```bash
make frontend-install   # npm install (first time only)
make frontend-build     # vite build → frontend/dist/
```

### Development workflow

For active frontend development, Vite provides hot module replacement:

```bash
# Terminal 1 — Haskell API server
stack exec lambdachan-exe

# Terminal 2 — Vite dev server with HMR
cd frontend && npm run dev
```

Then visit `http://localhost:5173`. Vite proxies all `/api/*` requests to the Haskell server on port 8080. Changes to Elm source files trigger an instant in-browser reload.

Or with the Makefile shortcut:

```bash
make dev   # starts both concurrently
```

---

## API reference

All endpoints are mounted under `/api/`. They return and accept `application/json`. Images are passed as base64-encoded strings inside JSON bodies. Protected endpoints require an `Authorization: Bearer <token>` header obtained from `POST /api/auth/login`.

### Boards

| Method | Path | Auth | Description |
|---|---|---|---|
| `GET` | `/api/boards` | None | List all boards |
| `POST` | `/api/boards` | Admin | Create a board |
| `GET` | `/api/boards/:board` | None | Board catalog (thread list with OP previews) |
| `DELETE` | `/api/boards/:board` | Admin | Delete a board and all its content |

**Create board request:**
```json
{ "name": "g", "title": "Technology", "description": "Technology discussion" }
```

**Board response:**
```json
{ "id": 1, "name": "g", "title": "Technology", "description": "Technology discussion", "createdAt": "2026-04-06T12:00:00Z" }
```

**Catalog response:**
```json
{
  "board": { "id": 1, "name": "g", ... },
  "threads": [
    {
      "id": 42,
      "subject": "Optional subject",
      "bumpedAt": "2026-04-06T13:00:00Z",
      "createdAt": "2026-04-06T12:00:00Z",
      "isLocked": false,
      "isSticky": false,
      "postCount": 7,
      "opPost": { ... }
    }
  ]
}
```

Threads are ordered: sticky threads first, then by `bumpedAt` descending (most recently active first).

---

### Threads

| Method | Path | Auth | Description |
|---|---|---|---|
| `POST` | `/api/boards/:board/threads` | None | Create a new thread (OP post) |
| `GET` | `/api/boards/:board/threads/:id` | None | Get a thread and all its replies |
| `DELETE` | `/api/boards/:board/threads/:id` | Mod/Admin | Soft-delete a thread |
| `PATCH` | `/api/boards/:board/threads/:id/sticky` | Mod/Admin | Set sticky status |
| `PATCH` | `/api/boards/:board/threads/:id/lock` | Mod/Admin | Set locked status |

**Create thread request:**
```json
{
  "subject": "What's your favourite distro?",
  "content": "I'll start: Arch btw",
  "authorName": "Anonymous",
  "image": {
    "data": "<base64-encoded image bytes>",
    "filename": "screenshot.png",
    "mimeType": "image/png"
  }
}
```

`subject`, `authorName`, and `image` are all optional. `authorName` defaults to `"Anonymous"`. See [Tripcodes](#tripcodes) for how to include a tripcode in `authorName`.

**Thread detail response:**
```json
{
  "thread": { "id": 42, "subject": "...", "postCount": 7, ... },
  "posts": [
    { "id": 1, "authorName": "Anonymous", "tripcode": null, "content": "...", "imageData": "<base64>", "imageName": "screenshot.png", "imageMimeType": "image/png", "createdAt": "..." },
    { "id": 2, "authorName": "Anonymous", "tripcode": "!AbCdEfGhIj", "content": "...", "imageData": null, ... }
  ]
}
```

**Toggle request** (for sticky/lock):
```json
{ "value": true }
```

---

### Posts

| Method | Path | Auth | Description |
|---|---|---|---|
| `POST` | `/api/boards/:board/threads/:id/posts` | None | Reply to a thread |
| `DELETE` | `/api/boards/:board/threads/:id/posts/:pid` | Mod/Admin | Soft-delete a post |

**Create post request:**
```json
{
  "content": "Gentoo is the answer",
  "authorName": "Anonymous",
  "image": null
}
```

Deleting the OP post (post ID matches the first post in the thread) will soft-delete the entire thread, matching 4chan behaviour. Replies to threads past the bump limit (300 posts) do not bump the thread.

---

### Auth

| Method | Path | Auth | Description |
|---|---|---|---|
| `POST` | `/api/auth/login` | None | Log in; returns a session token |
| `POST` | `/api/auth/logout` | Bearer | Invalidate the current session token |

**Login request:**
```json
{ "username": "admin", "password": "changeme" }
```

**Login response:**
```json
{
  "token": "550e8400-e29b-41d4-a716-446655440000",
  "user": { "id": 1, "username": "admin", "role": "admin", "modBoards": [] }
}
```

Pass the token in the `Authorization` header for all protected endpoints:
```
Authorization: Bearer 550e8400-e29b-41d4-a716-446655440000
```

Sessions expire after 7 days.

---

### Admin: user management

All routes require an admin session token.

| Method | Path | Description |
|---|---|---|
| `GET` | `/api/admin/users` | List all moderator/admin accounts |
| `POST` | `/api/admin/users` | Create a moderator or admin account |
| `DELETE` | `/api/admin/users/:id` | Delete a user account |
| `POST` | `/api/admin/users/:id/boards/:board` | Grant a moderator access to a board |
| `DELETE` | `/api/admin/users/:id/boards/:board` | Revoke a moderator's access to a board |

**Create user request:**
```json
{ "username": "alice", "password": "s3cret", "role": "moderator" }
```

Valid roles: `"admin"`, `"moderator"`.

**User response:**
```json
{ "id": 2, "username": "alice", "role": "moderator", "modBoards": ["g", "sci"] }
```

---

## Authentication

λchan uses **session tokens** (UUID v4). On login, a token is returned and stored in the database with a 7-day expiry. Include it as a Bearer token on protected requests.

Anonymous users post without any token. The `authorName` field in post/thread creation requests is free text — anyone can type any name (with or without a tripcode).

---

## Tripcodes

Tripcodes let users prove a consistent identity without creating an account, exactly as on 4chan.

In the `authorName` field, append `#password` or `##password`:

| Input | Output name | Tripcode | Algorithm |
|---|---|---|---|
| `Anonymous` | `Anonymous` | none | — |
| `Anonymous#hunter2` | `Anonymous` | `!AbCdEfGhIj` | SHA256(password) |
| `Anonymous##hunter2` | `Anonymous` | `!!KlMnOpQrSt` | SHA256(password + server salt) |

**Standard tripcode** (`#`): deterministic; the same password always produces the same tripcode across all λchan instances.

**Secure tripcode** (`##`): includes a server-side salt (`TRIPCODE_SALT`), so it cannot be computed or spoofed by users who do not know the salt. Set a long random value for `TRIPCODE_SALT` in production.

Computed tripcodes appear in the `tripcode` field of post responses.

---

## User roles

| Role | Capabilities |
|---|---|
| **Anonymous** | Create threads, reply to threads (no account needed) |
| **Moderator** | Everything anonymous can do + delete posts/threads, sticky/lock threads — on boards they are assigned to only |
| **Admin** | Everything moderators can do on all boards + create/delete boards, create/delete/assign user accounts |

Moderators are assigned to boards individually via `POST /api/admin/users/:id/boards/:board`.

---

## Database backends

The same compiled binary supports both backends via the `DATABASE_URL` environment variable.

### SQLite (default)

Zero setup. State is stored in a single file. Suitable for development and staging.

```bash
# Use default file (lambdachan.db in the working directory)
stack exec lambdachan-exe

# Specify a different path
DATABASE_URL=/var/lib/lambdachan/data.db stack exec lambdachan-exe
```

### PostgreSQL

Requires `libpq` on the host. Set `DATABASE_URL` to a libpq connection string containing the word `postgres`:

```bash
DATABASE_URL="postgresql://user:pass@host:5432/lambdachan" stack exec lambdachan-exe
```

Schema migrations are applied automatically on startup with `runMigration migrateAll`.

---

## Development

### Backend (Haskell)

```bash
# Build
stack build

# Build and watch for changes (requires entr or similar)
find src -name '*.hs' | entr stack build

# Run with auto-reload via ghcid
stack exec ghcid -- --command="stack ghci lambdachan:lib"

# Check types without a full build
stack build --fast
```

#### Adding a new endpoint

1. Add the endpoint type to `LambdaChanAPI` in `src/LambdaChan/API/Types.hs`
2. Add the handler function to `appServer` in `src/LambdaChan/API/Handlers.hs` — **order must match the type exactly**
3. Implement the handler function
4. Add tests in `test/LambdaChan/`

#### Adding a new database entity

1. Add the entity to the `[persistLowerCase|...|]` block in `src/LambdaChan/Database/Schema.hs`
2. Add query functions in `src/LambdaChan/Database/Queries.hs`
3. Migrations run automatically on startup — no manual migration files needed for development

### Frontend (Elm)

```bash
# Type-check without producing output
cd frontend && elm make src/Main.elm --output=/dev/null

# Build to frontend/dist/ (no npm)
make elm-build

# Install npm deps and build with Vite (enables HMR in dev)
make frontend-install
make frontend-build

# Full dev workflow (Haskell on :8080, Vite+HMR on :5173)
make dev
```

#### Adding a new page

1. Create `frontend/src/Page/NewPage.elm` with `Model`, `Msg`, `init`, `update`, `view`
2. Add a constructor to `Route` in `frontend/src/Route.elm` and extend `routeParser`
3. Add `NewPageModel NewPage.Model` to `PageModel` and `NewPageMsg NewPage.Msg` to `PageMsg` in `Main.elm`
4. Handle the new route in `routeToPage` and the new message in `updatePage`/`viewPage`

---

## Testing

### Backend

```bash
# Run all tests
stack test

# Run tests with verbose output
stack test --test-arguments "--format=progress"

# Run a specific spec file
stack test --test-arguments "-m Auth"
```

The test suite has three components:

| Spec | Type | What it tests |
|---|---|---|
| `AuthSpec` | Unit | Tripcode computation, password hashing, `parseAuthorName` |
| `DatabaseSpec` | Integration | CRUD, soft deletes, bump ordering, session expiry, mod board assignment — against an in-memory SQLite DB |
| `APISpec` | HTTP integration | Full HTTP round-trips via `hspec-wai` — auth enforcement, 404 handling, content negotiation |

Each test runs against a fresh in-memory SQLite database (`:memory:`), so tests are fully isolated with no cleanup required. The test app uses `mkApiApp` (pure Servant, no static file layer) so test paths do not need the `/api` prefix.

### Frontend smoke test

Build and run the full stack, then verify in a browser:

```bash
make elm-run   # elm-build + stack exec lambdachan-exe
# open http://localhost:8080
```

Things to check:

- Board list loads at `/`
- Navigating to `/b/:board` shows the catalog
- A new thread can be posted anonymously
- Login at `/login` stores the session and shows the logout button
- After logging in as admin, mod controls (delete, sticky, lock) appear on threads and posts
- The admin panel at `/admin` lists users and allows creating/deleting accounts and assigning moderators to boards

### Frontend type-check

```bash
cd frontend && elm make src/Main.elm --output=/dev/null
```

This runs the Elm compiler across all 14 modules without writing any output. Elm's type system catches most logic errors at compile time, so a clean build here provides strong confidence in the frontend's correctness.

---

## Project structure

```
lambdachan/
├── Makefile                             — Common tasks (build, test, elm-build, dev)
├── app/
│   └── Main.hs                          — Entry point; reads env vars, calls runApp
├── src/
│   ├── Lib.hs                           — Public re-exports
│   └── LambdaChan/
│       ├── Types.hs                     — Core types: UserRole, AuthUser, AppError
│       ├── Config.hs                    — AppConfig, AppEnv, App monad (ReaderT AppEnv Handler)
│       ├── Auth.hs                      — Tripcodes, bcrypt, session tokens, auth guards
│       ├── App.hs                       — mkApp (with static routing), mkApiApp (Servant only), runApp
│       ├── Database/
│       │   ├── Schema.hs                — Persistent entity definitions + migrateAll
│       │   └── Queries.hs               — All database operations
│       └── API/
│           ├── Types.hs                 — Servant API type (18 endpoints) + JSON types
│           └── Handlers.hs              — Request handler implementations
├── test/
│   ├── Spec.hs                          — Test runner
│   └── LambdaChan/
│       ├── TestHelpers.hs               — Shared test utilities (test pool, app, user fixtures)
│       ├── AuthSpec.hs                  — Auth unit tests
│       ├── DatabaseSpec.hs              — Database integration tests
│       └── APISpec.hs                   — HTTP integration tests
├── frontend/
│   ├── elm.json                         — Elm dependencies
│   ├── package.json                     — npm dependencies (Vite + vite-plugin-elm)
│   ├── vite.config.js                   — Vite config; proxies /api/* to localhost:8080 in dev
│   ├── index.html                       — SPA shell for Vite dev/build
│   ├── dist-index.html                  — SPA shell for elm-make builds (no ES modules)
│   └── src/
│       ├── Main.elm                     — Browser.application root
│       ├── Route.elm                    — URL parser (5 routes)
│       ├── Types.elm                    — Shared Elm types + JSON decoders/encoders
│       ├── Api.elm                      — All HTTP calls, JSON decoders, error helpers
│       ├── Session.elm                  — localStorage ports + session helpers
│       ├── Page/
│       │   ├── BoardList.elm            — Board directory
│       │   ├── Catalog.elm              — Thread list + new-thread form
│       │   ├── Thread.elm               — Thread view + reply form + mod controls
│       │   ├── Login.elm                — Login form
│       │   └── Admin.elm                — User and moderator management
│       ├── View/
│       │   ├── Post.elm                 — Post renderer (full and compact)
│       │   ├── PostForm.elm             — Shared post/thread form with image upload
│       │   ├── Image.elm                — base64 image thumbnail rendering
│       │   ├── Nav.elm                  — Top navigation bar
│       │   └── Modal.elm                — Delete confirmation modal
│       └── style.css                    — 4chan-inspired stylesheet
├── package.yaml                         — hpack config (dependencies, GHC options)
└── stack.yaml                           — Stack resolver (LTS 24.36, GHC 9.10.3)
```
