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
  Lobby.elm           # Game lobby UI
  Ports.elm           # JS interop (localStorage, SSE)
  Game/
    Game.elm          # Core game logic and board rendering
    Card.elm          # Card definitions and movement patterns
    Figure.elm        # Piece (King/Pawn) definitions
    Cell.elm          # Board cell rendering

server/src/           # Haskell backend
  Api.hs              # Servant API route definitions
  App.hs              # Request handlers, SSE streaming
  Subscribers.hs      # SSE subscription management (STM-based)
  Game.hs             # Game data types
  Database.hs         # JSON persistence and session management
  Options.hs          # CLI and YAML config parsing

assets/               # Static files served to browser
  index.html
  elm.js
  style.css
  localStorage.js     # Player identity persistence
  sse.js              # SSE EventSource wrapper
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

### SSE Endpoints

| Path | Events |
|------|--------|
| `/games/stream` | `gameCreated`, `playerJoined`, `gameStarted`, `gameEnded` |
| `/games/{id}/stream` | `move`, `concede` |

All authenticated endpoints use `X-Session-Token` header.

## Architecture

### Game-Agnostic Server

The server knows **nothing about Onitama rules**. It's a pure "move broker":
- Stores game state (moves as opaque strings)
- Manages sessions and authentication
- Broadcasts events via SSE
- Persists data to JSON

All game logic (move validation, win detection, card rules) lives in the Elm client.

### Real-time Updates (SSE)

- **Lobby stream**: Broadcasts when games are created, players join, games start/end
- **Game stream**: Broadcasts moves and concede events to players in a game
- STM-based subscriber management with automatic cleanup on disconnect
- EventSource auto-reconnects on connection loss

### Frontend (Elm)

- TEA architecture with page states: `Redirect | Lobby | EnterName | Playing`
- SSE subscriptions replace polling for real-time updates
- Player identities stored in localStorage with bidirectional port sync
- Board perspective rotated for Black player

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

**Move History:**
- Append-only list: `[(GameMove, UTCTime)]` (head = most recent)
- Client applies moves locally when received via SSE

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

- No server-side move validation (by design)
- No chat feature
- No expansion cards (Sensei's Path)

## Docker

```bash
make release
docker build -t onitama:latest .
docker run -p 8080:8080 onitama:latest
```
