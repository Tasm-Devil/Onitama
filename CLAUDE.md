# Onitama - Project Context

Multiplayer web implementation of the abstract board game Onitama.

## Tech Stack

- **Frontend**: Elm 0.19.1 (TEA pattern)
- **Backend**: Haskell with Servant framework
- **Persistence**: JSON file (`gamedb.json`)
- **Concurrency**: STM (Software Transactional Memory)

## Build Commands

```bash
make all            # Full setup + build (debug mode)
make build          # Build both client (debug) and server
make release        # Build optimized production version
make client-build   # Build Elm frontend in DEBUG mode (with time-travel debugger)
make client-release # Build Elm frontend in RELEASE mode (optimized, ~40% smaller)
make server-build   # Build Haskell backend only
make server-start   # Build and run server on port 8080
make test           # Run all tests
make clean          # Remove build artifacts
```

**Debug vs Release:**
- **Debug** (`make client-build`): Includes Elm's time-travel debugger, larger file size (~375KB)
- **Release** (`make client-release`): Optimized for production, no debugger, smaller (~225KB)

## Project Structure

```
client/src/           # Elm frontend
  Main.elm            # Application entry, routing, page states
  Api.elm             # HTTP client for server communication
  Lobby.elm           # Game lobby UI
  Ports.elm           # JS interop (localStorage)
  Game/
    Game.elm          # Core game logic and board rendering
    Card.elm          # Card definitions and movement patterns
    Figure.elm        # Piece (King/Pawn) definitions
    Cell.elm          # Board cell rendering

server/src/           # Haskell backend
  Api.hs              # Servant API route definitions
  App.hs              # Request handlers
  Game.hs             # Game data types
  Database.hs         # JSON persistence and session management
  Options.hs          # CLI and YAML config parsing

assets/               # Static files served to browser
  index.html
  favicon.svg
  elm.js
  style.css
  localStorage.js     # Bidirectional player identity persistence
```

## API Endpoints

Base URL: `http://localhost:8080/1/onitama`

**RESTful Design (Phase 1 Complete - 2026-01-18)**

| Method | Path | Purpose | Returns |
|--------|------|---------|---------|
| GET | `/games` | List all games | `[GameSummary]` |
| POST | `/games` | Create new game | `GameId` |
| GET | `/games/{id}` | Get game state | `GameWithNames` or 404 |
| POST | `/games/{id}/players` | Join game (body: name, header: token?) | `Either JoinError JoinGameResponse` |
| POST | `/games/{id}/moves` | Submit move (body: move, header: token) | `Either MoveError GameMove` |
| POST | `/games/{id}/concede` | Concede game (header: token) | `Either ConcedeError Color` |

**Error Types:**
- **JoinError**: `JEGameNotFound`, `JEGameFull`, `JEInvalidToken`, `JENameTaken`, `JEInvalidName`
- **MoveError**: `MEInvalidToken`, `MENotYourTurn`, `MEGameNotFound`, `MEGameOver`, `MEInvalidMove`
- **ConcedeError**: `CEInvalidToken`, `CEGameNotFound`, `CEAlreadyEnded`

All authenticated endpoints use `X-Session-Token` header.

## Architecture Philosophy

**Onitama-Specific Server with Game-Agnostic Potential**: The server is currently designed for Onitama (API paths include `/onitama/`), but the core architecture **could be** game-agnostic with minimal changes. It acts as a pure "move broker" or "state store" with no knowledge of game rules:

**What the server does:**
- Stores game state (moves/history as opaque strings)
- Manages sessions and player authentication
- Routes messages between players
- Persists data to JSON

**What the server does NOT do:**
- Validate moves (client-side only)
- Check win conditions (client-side only)
- Understand card or piece rules (client-side only)
- Enforce turn order beyond basic "who moves next" based on card color*

*Turn order is determined by the common card's color stamp, which the server looks up from a hardcoded list.

**All game logic lives in clients**:
- Move validation happens client-side only
- Win condition detection is client-side
- Card/piece rules are client-side
- Board state reconstruction from move history

**Benefits**:
- Server infrastructure is reusable for any turn-based game
- Game logic updates don't require server redeployment
- Clean separation of concerns

## Architecture Notes

### Frontend (Elm)
- TEA architecture with page states: `Redirect | Lobby | EnterName | Playing`
- Player identities stored in localStorage: `[{playerName, token}]`
  - Bidirectional sync via ports
  - Provides autocomplete for returning players
- Board perspective rotated for Black player
- Polls server for updates (no WebSocket)

### Backend (Haskell)
- Type-safe API with Servant
- TVar/STM for concurrent game state
- UUID-based session tokens per player
- Auto-saves to `gamedb.json` (configurable interval, default: 1 minute)
- **No magic numbers**: All timeouts, intervals, and ports configurable via CLI or config file
- **Clean database JSON**: Runtime state (next IDs, hasChanged flag, cleanup config) separated from persisted data (games and players only)

### Key Patterns

**Database Structure:**
- `DBData` (persisted): Contains only `dbGames` and `dbPlayers` (pure data)
- `DB` (runtime): Wraps `TVar DBData` with runtime state (next IDs, dirty flag, config)
- Next IDs calculated on startup from existing data using `calculateNextId`

**Move History:**
- Append-only list: `[(GameMove, UTCTime)]` (head = most recent, FP style)
- Server history compared with local history to detect opponent moves

**localStorage Persistence:**
- Player identities (name + token pairs) stored in browser localStorage
- Bidirectional port communication:
  1. Elm → `savePlayer` port → JavaScript updates localStorage
  2. JavaScript → `loadPlayers` subscription → Elm receives updated list
- Enables autocomplete and seamless token reuse

**Cleanup Logic:**
- Automatic cleanup based on game status (configurable):
  - **WaitingForPlayers**: 2 hours (default)
  - **InProgress**: 24 hours (default)
  - **Completed**: 0 = never (default, keeps history for playback)
  - Timeout <= 0 means never clean up (disabled)
- Background cleanup thread runs every 10 minutes (default)

## Server Configuration

**Command-Line Options:**
```bash
# Basic Options
-v, --verbose                 Enable verbose HTTP logging
-p, --port PORT               Server port (default: 8080)
--host HOST                   Bind address (default: "0.0.0.0")
-d, --database FILE           Database file path (default: "gamedb.json")
--reset-db                    Start with fresh empty database

# Advanced Options
--save-interval MINS          Database save interval in minutes (default: 1)
--cleanup-waiting HOURS       Hours before cleaning waiting games (default: 2.0)
--cleanup-active HOURS        Hours before cleaning active games (default: 24.0)
--cleanup-completed HOURS     Hours before cleaning completed games (default: 0 = never)
--cleanup-interval MINS       Minutes between cleanup checks (default: 10)
--no-cleanup                  Disable automatic game cleanup

# Config & Info
-c, --config FILE             Config file path (must be explicitly specified)
--version                     Show version information
-h, --help                    Show help message
```

**Configuration File:**
- YAML format with precedence: **defaults < config file < CLI arguments**
- Example: `onitama-server.example.yaml`
- Must be explicitly specified with `--config` flag

## Testing

- **Haskell**: hspec framework in `server/test/` (currently minimal)
- **Elm**: No tests yet (can add with elm-test)

## TODO

### Active TODOs

**1. Add Footer Component** (MEDIUM priority)
- Create `client/src/Footer.elm` with project info, GitHub link
- Import in Lobby.elm and Main.elm error pages
- Match minimal aesthetic

**2. Improve CSS and Visual Design** (LOW priority)
- Mobile responsive layout
- Card hover effects
- Piece movement animations
- Better color scheme
- Consider Tailwind CSS or pure CSS custom properties

**3. Other Roadmap Items**
- See README.md for full roadmap

### Completed

- ✅ Timestamps for games and moves (automatic cleanup)
- ✅ Command-line options & YAML config file support
- ✅ Clean database JSON (runtime state separated from persisted data)
- ✅ Per-player token system (global player identity)
- ✅ RESTful API design with proper error handling

## Known Limitations

- No server-side move validation (cheating possible by design)
- No win detection on server (client-only)
- HTTP polling only (WebSocket planned)
- No chat feature
- No expansion cards (Sensei's Path)

## Docker

```bash
make release  # Build optimized version for production
docker build -t onitama:latest .
docker run -p 8080:8080 onitama:latest
```

**Important:** Always run `make release` before building the Docker image to ensure the optimized frontend is included.
