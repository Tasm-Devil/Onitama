# Onitama - Project Context

Multiplayer web implementation of the abstract board game Onitama.

## Tech Stack

- **Frontend**: Elm 0.19.1 (TEA pattern)
- **Backend**: Haskell with Servant framework
- **Auth**: OIDC via Traefik + Authelia (headers: `Remote-User`, `Remote-Name`)
- **Real-time**: Server-Sent Events (SSE)
- **Persistence**: In-memory by default; optional JSON file via `--database`
- **Concurrency**: STM (Software Transactional Memory)

## Build Commands

```bash
make all            # Full setup + build (debug mode)
make build          # Build both client (debug) and server
make release        # Build optimized production version
make server-start   # Build and run server on port 8080
make clean          # Remove build artifacts
```

**Note:** `make test` exists but the test suite is commented out (no active tests).

## Project Structure

```
client/src/           # Elm frontend
  Main.elm            # Application entry, routing, SSE subscriptions
  Api.elm             # HTTP client, SSE event decoders
  Lobby.elm           # Game lobby UI (stateful with card set picker)
  Ports.elm           # JS interop (SSE, sound)
  Game/
    Game.elm          # Board rendering, move execution, game log view
    Card.elm          # Card definitions and movement patterns
    Figure.elm        # Piece (King/Pawn) definitions
    Cell.elm          # Board cell rendering

server/src/           # Haskell backend
  Types.hs            # Shared domain types (Game, Color, OidcUserId, AuthUser, errors, requests)
  Api.hs              # Servant API route definitions
  App.hs              # Request handlers, SSE streaming
  Subscribers.hs      # SSE subscription management (STM-based)
  Database.hs         # JSON persistence (games only, no player DB)
  Onitama.hs          # Onitama game logic (move validation, win detection)
  Minimax.hs          # AI opponent (negamax with alpha-beta pruning)
  Options.hs          # CLI and YAML config parsing (incl. --dev mode)

assets/               # Static files served to browser
  index.html
  elm.js
  style.css
  sse.js              # SSE EventSource wrapper
  sound.js            # Move sound effect notifications
  mp3/                # Move sound effect audio files
```

## API Endpoints

Base URL: `http://localhost:8080/1/onitama`

### REST Endpoints

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/me` | Get authenticated user info |
| GET | `/games` | List all games |
| POST | `/games` | Create new game (unified: multiplayer + AI) |
| GET | `/games/{id}` | Get game state |
| POST | `/games/{id}/players` | Join game |
| POST | `/games/{id}/moves` | Submit move |
| POST | `/games/{id}/concede` | Concede game |

### SSE Endpoints

| Path | Events |
|------|--------|
| `/games/stream` | `lobbyChanged` (client refetches summaries) |
| `/games/{id}/stream` | `move`, `concede`, `playerJoined` |

Authenticated endpoints use `X-Forwarded-User` and `X-Forwarded-Preferred-Username` headers (set by oauth2-proxy, or dev middleware).

## Authentication

- **Production**: GitHub OAuth via oauth2-proxy. NPM routes to oauth2-proxy, which handles login and proxies to onitama with auth headers.
- **Headers**: `X-Forwarded-User` (user ID) and `X-Forwarded-Preferred-Username` (display name), standard oauth2-proxy headers.
- **Development**: `--dev` flag enables middleware that injects default headers when missing. Use `--dev-user NAME` to customize. `make server-start` enables dev mode automatically.
- **Identity types**: `OidcUserId` (newtype over Text), `AuthUser { authUserId, authUserName }`.
- **No localStorage/tokens**: Old `X-Session-Token` + localStorage system fully removed.
- **AI player**: Uses reserved `OidcUserId "__ai__"`.
- **Player names**: Cached in Game record (`player_white_name`, `player_black_name`), not in a separate player DB.

## Architecture

### Server-Side Move Validation

The server validates all moves against Onitama rules before accepting them:
- `Onitama.hs` is the single source of truth for card definitions (16 base + 16 Sensei's Path expansion), plus move parsing and game state replay
- Card set selection (`BaseOnly` or `WithExpansion`) is chosen at game creation time; move validation always knows all cards
- Moves are validated by replaying the full history then checking the new move
- Win detection (king capture / temple reached) sets the `winner` field on the server
- Invalid moves return `MEInvalidMove`, completed games return `MEGameOver`

### Real-time Updates (SSE)

- **Lobby stream**: Invalidate+refetch pattern — server sends `lobbyChanged` on connect, on changes, and on game cleanup; client refetches game summaries via GET
- **Game stream**: Granular events — `move` (with optional `winner`), `concede`, and `playerJoined` broadcast directly to players
- STM-based subscriber management with automatic cleanup on disconnect
- EventSource auto-reconnects on connection loss

### Frontend (Elm)

- TEA architecture with record Model `{ key, currentUser, page }` + Page union type
- Page states: `Loading Route | LobbyPage Lobby.Model | AwaitingGame (Maybe GameId) String | GamePage GameId Game LogEntries`
- `currentUser : Maybe AuthUser` — fetched from `/me` on init, identity from OIDC provider
- Lobby.elm is stateful: `Browsing | PickingCardSet GameType CardSet` with card set picker inline
- Only two URL patterns: `/` (lobby) and `/{gameId}` (game). No `/newgame` routes.
- "New Game" / "Play vs AI" are button clicks in lobby, not navigation
- SSE subscriptions replace polling for real-time updates
- Moves applied exclusively via game SSE stream (HTTP response only for error handling)
- Board perspective rotated for Black player
- Sound notification plays on opponent's moves via `playSound` port

### Backend (Haskell)

- Type-safe API with Servant
- `Types.hs` holds all shared domain types; `Api.hs` is purely route definitions
- `Database.hs` depends on `Types.hs` (not `Api.hs`) — clean layering
- TVar/STM for concurrent game state and SSE subscribers
- `DBData` contains only games (no player table)
- `gameToGameWithNames` and `gameToSummary` are pure functions (names stored in Game record)
- Auto-saves to `gamedb.json` (configurable interval)
- Configurable via CLI or YAML config file

### Key Patterns

**SSE Broadcasting:**
- `SubscriberStore` holds `TVar` maps of `TQueue` per client
- Handlers broadcast events after successful operations
- WAI `responseStream` for SSE responses

**Game Log:**
- `LogEntry` type: `MoveEntry GameMove | SystemEntry String`
- System messages (join, game start) built on join and via `playerJoined` SSE
- Append-only list (head = most recent), rendered with `column-reverse` CSS

## Server Configuration

```bash
server --port 3000              # Custom port
server --verbose                # HTTP logging
server --database /data/db.json # Enable persistence to JSON file
server --config server.yaml     # Load YAML config
server --no-cleanup             # Disable game cleanup
server --dev                    # Dev mode (inject auth headers)
server --dev-user alice         # Dev mode with custom user name
```

## Testing SSE

```bash
# Watch lobby events
curl -N localhost:8080/1/onitama/games/stream

# Watch game events
curl -N localhost:8080/1/onitama/games/1/stream
```

## Known Limitations

- No chat feature

## Docker

```bash
make docker        # Build release + Docker image + save tar
make docker-start  # Build + run on http://localhost:8080
```

Deployment uses oauth2-proxy for GitHub OAuth + NPM as reverse proxy. See `.env.example` for required secrets.
