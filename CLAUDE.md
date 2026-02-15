# Onitama - Project Context

Multiplayer web implementation of the abstract board game Onitama.

## Tech Stack

- **Frontend**: Elm 0.19.1 (TEA pattern)
- **Backend**: Haskell with Servant framework
- **Real-time**: Server-Sent Events (SSE)
- **Persistence**: JSON file (`gamedb.json`)
- **Concurrency**: STM (Software Transactional Memory)

## Build Commands

```bash
make all            # Full setup + build (debug mode)
make build          # Build both client (debug) and server
make release        # Build optimized production version
make server-start   # Build and run server on port 8080
make test           # Run all tests
make clean          # Remove build artifacts
```

## Project Structure

```
client/src/           # Elm frontend
  Main.elm            # Application entry, routing, SSE subscriptions
  Api.elm             # HTTP client, SSE event decoders
  Lobby.elm           # Game lobby UI (pure view, no Msg type)
  EnterName.elm       # Name entry / game join flow
  Ports.elm           # JS interop (localStorage, SSE, sound)
  Game/
    Game.elm          # Board rendering, move execution, game log view
    Card.elm          # Card definitions and movement patterns
    Figure.elm        # Piece (King/Pawn) definitions
    Cell.elm          # Board cell rendering

server/src/           # Haskell backend
  Api.hs              # Servant API route definitions
  App.hs              # Request handlers, SSE streaming
  Subscribers.hs      # SSE subscription management (STM-based)
  Game.hs             # Game data types (pure data, no logic)
  Database.hs         # JSON persistence and session management
  Onitama.hs          # Onitama game logic (move validation, win detection)
  Options.hs          # CLI and YAML config parsing

assets/               # Static files served to browser
  index.html
  elm.js
  style.css
  localStorage.js     # Player identity persistence
  sse.js              # SSE EventSource wrapper
  sound.js            # Move sound effect notifications
  mp3/                # Move sound effect audio files
```

## API Endpoints

Base URL: `http://localhost:8080/1/onitama`

### REST Endpoints

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/games` | List all games |
| POST | `/games` | Create new game |
| GET | `/games/{id}` | Get game state |
| POST | `/games/{id}/players` | Join game |
| POST | `/games/{id}/moves` | Submit move |
| POST | `/games/{id}/concede` | Concede game |
| GET | `/newgame` | Serve index.html (client creates game) |

### SSE Endpoints

| Path | Events |
|------|--------|
| `/games/stream` | `lobbyChanged` (client refetches summaries) |
| `/games/{id}/stream` | `move`, `concede`, `playerJoined` |

All authenticated endpoints use `X-Session-Token` header.

## Architecture

### Server-Side Move Validation

The server validates all moves against Onitama rules before accepting them:
- `Onitama.hs` is the single source of truth for card definitions (names, moves, starting player), plus move parsing and game state replay
- Moves are validated by replaying the full history then checking the new move
- Win detection (king capture / temple reached) sets the `winner` field on the server
- Invalid moves return `MEInvalidMove`, completed games return `MEGameOver`

### Real-time Updates (SSE)

- **Lobby stream**: Invalidate+refetch pattern — server sends `lobbyChanged` on connect, on changes, and on game cleanup; client refetches game summaries via GET
- **Game stream**: Granular events — `move` (with optional `winner`), `concede`, and `playerJoined` broadcast directly to players
- STM-based subscriber management with automatic cleanup on disconnect
- EventSource auto-reconnects on connection loss

### Frontend (Elm)

- TEA architecture with record Model + Page type: `Redirect | LobbyPage | EnterNamePage | PlayingPage`
- EnterName module handles name entry/join flow with its own Msg/update/view
- SSE subscriptions replace polling for real-time updates
- Moves applied exclusively via game SSE stream (HTTP response only for error handling)
- Player identities stored in localStorage with bidirectional port sync
- Board perspective rotated for Black player
- Sound notification plays on opponent's moves via `playSound` port

### Backend (Haskell)

- Type-safe API with Servant
- TVar/STM for concurrent game state and SSE subscribers
- UUID-based session tokens per player
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
server --database /data/db.json # Custom DB path
server --config server.yaml     # Load YAML config
server --no-cleanup             # Disable game cleanup
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
- No expansion cards (Sensei's Path)

## Docker

```bash
make docker        # Build release + Docker image + save tar
make docker-start  # Build + run on http://localhost:8080
```
