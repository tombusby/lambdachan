# lambdachan

A 4chan-style imageboard REST API built with [Servant](https://haskell-servant.github.io/) and [Persistent](https://www.yesodweb.com/book/persistent). Anonymous posting, tripcodes, per-board moderation, and configurable database backends (SQLite for dev, PostgreSQL for production).

---

## Contents

- [Features](#features)
- [Quick start](#quick-start)
- [Configuration](#configuration)
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

---

## Quick start

### Prerequisites

- [Stack](https://docs.haskellstack.org/) (installs GHC automatically)
- For PostgreSQL support: `libpq` system library (`brew install libpq` on macOS, `apt install libpq-dev` on Debian/Ubuntu)

### Build and run (SQLite)

```bash
git clone <repo>
cd lambdachan
stack build
stack exec lambdachan-exe
```

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

Change the default password immediately by deleting the seeded user via the admin API and creating a new one.

### Run with PostgreSQL

```bash
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

Example PostgreSQL connection string: `host=db.example.com port=5432 dbname=lambdachan user=app password=hunter2 sslmode=require`

---

## API reference

All endpoints return and accept `application/json`. Images are passed as base64-encoded strings inside JSON bodies. Protected endpoints require an `Authorization: Bearer <token>` header obtained from `POST /auth/login`.

### Boards

| Method | Path | Auth | Description |
|---|---|---|---|
| `GET` | `/boards` | None | List all boards |
| `POST` | `/boards` | Admin | Create a board |
| `GET` | `/boards/:board` | None | Board catalog (thread list with OP previews) |
| `DELETE` | `/boards/:board` | Admin | Delete a board and all its content |

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
| `POST` | `/boards/:board/threads` | None | Create a new thread (OP post) |
| `GET` | `/boards/:board/threads/:id` | None | Get a thread and all its replies |
| `DELETE` | `/boards/:board/threads/:id` | Mod/Admin | Soft-delete a thread |
| `PATCH` | `/boards/:board/threads/:id/sticky` | Mod/Admin | Set sticky status |
| `PATCH` | `/boards/:board/threads/:id/lock` | Mod/Admin | Set locked status |

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
| `POST` | `/boards/:board/threads/:id/posts` | None | Reply to a thread |
| `DELETE` | `/boards/:board/threads/:id/posts/:pid` | Mod/Admin | Soft-delete a post |

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
| `POST` | `/auth/login` | None | Log in; returns a session token |
| `POST` | `/auth/logout` | Bearer | Invalidate the current session token |

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
| `GET` | `/admin/users` | List all moderator/admin accounts |
| `POST` | `/admin/users` | Create a moderator or admin account |
| `DELETE` | `/admin/users/:id` | Delete a user account |
| `POST` | `/admin/users/:id/boards/:board` | Grant a moderator access to a board |
| `DELETE` | `/admin/users/:id/boards/:board` | Revoke a moderator's access to a board |

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

lambdachan uses **session tokens** (UUID v4). On login, a token is returned and stored in the database with a 7-day expiry. Include it as a Bearer token on protected requests.

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

**Standard tripcode** (`#`): deterministic; the same password always produces the same tripcode across all lambdachan instances.

**Secure tripcode** (`##`): includes a server-side salt (`TRIPCODE_SALT`), so it cannot be computed or spoofed by users who do not know the salt. Set a long random value for `TRIPCODE_SALT` in production.

Computed tripcodes appear in the `tripcode` field of post responses.

---

## User roles

| Role | Capabilities |
|---|---|
| **Anonymous** | Create threads, reply to threads (no account needed) |
| **Moderator** | Everything anonymous can do + delete posts/threads, sticky/lock threads — on boards they are assigned to only |
| **Admin** | Everything moderators can do on all boards + create/delete boards, create/delete/assign user accounts |

Moderators are assigned to boards individually via `POST /admin/users/:id/boards/:board`.

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

```bash
# Build
stack build

# Build and watch for changes (requires entr or similar)
find src -name '*.hs' | entr stack build

# Run with auto-reload via ghcid
stack exec ghcid -- --command="stack ghci lambdachan:lib"

# Launch the server in dev mode
stack exec lambdachan-exe

# Check types without a full build
stack build --fast
```

### Adding a new endpoint

1. Add the endpoint type to `LambdaChanAPI` in `src/LambdaChan/API/Types.hs`
2. Add the handler function to `appServer` in `src/LambdaChan/API/Handlers.hs` — **order must match the type exactly**
3. Implement the handler function
4. Add tests in `test/LambdaChan/`

### Adding a new database entity

1. Add the entity to the `[persistLowerCase|...|]` block in `src/LambdaChan/Database/Schema.hs`
2. Add query functions in `src/LambdaChan/Database/Queries.hs`
3. Migrations run automatically on startup — no manual migration files needed for development

---

## Testing

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

Each test runs against a fresh in-memory SQLite database (`:memory:`), so tests are fully isolated with no cleanup required.

---

## Project structure

```
lambdachan/
├── app/
│   └── Main.hs                      — Entry point; reads env vars, calls runApp
├── src/
│   ├── Lib.hs                       — Public re-exports
│   └── LambdaChan/
│       ├── Types.hs                 — Core types: UserRole, AuthUser, AppError
│       ├── Config.hs                — AppConfig, AppEnv, App monad (ReaderT AppEnv Handler)
│       ├── Auth.hs                  — Tripcodes, bcrypt, session tokens, auth guards
│       ├── App.hs                   — mkApp, runApp, pool init, default admin seed
│       ├── Database/
│       │   ├── Schema.hs            — Persistent entity definitions + migrateAll
│       │   └── Queries.hs           — All database operations
│       └── API/
│           ├── Types.hs             — Servant API type (18 endpoints) + JSON types
│           └── Handlers.hs          — Request handler implementations
├── test/
│   ├── Spec.hs                      — Test runner
│   └── LambdaChan/
│       ├── TestHelpers.hs           — Shared test utilities (test pool, app, user fixtures)
│       ├── AuthSpec.hs              — Auth unit tests
│       ├── DatabaseSpec.hs          — Database integration tests
│       └── APISpec.hs               — HTTP integration tests
├── package.yaml                     — hpack config (dependencies, GHC options)
└── stack.yaml                       — Stack resolver (LTS 24.36, GHC 9.10.3)
```
