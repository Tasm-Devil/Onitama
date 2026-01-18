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

assets/               # Static files served to browser
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

**Future: Referee Client** (optional): For competitive play, a separate "Referee" client can be implemented that:
- Gets notified by the server when moves are posted
- Validates moves against game rules
- Flags invalid moves or disputed games
- This keeps the server single-purpose while enabling optional validation

**Path to Game-Agnostic**:
If you want to support multiple games:
1. Change API paths from `/1/onitama/games/` to `/1/games/{gameType}/` or just `/1/games/`
2. Remove hardcoded `cardStartPlayer` logic (move to client or make it generic)
3. Done! The core server architecture already treats moves as opaque strings

**Benefits of current approach**:
- Server infrastructure is nearly reusable for any turn-based game
- Game logic updates don't require server redeployment
- Referee can be optional (casual vs competitive modes)
- Clean separation of concerns
- Easy to add more games later if needed

## Architecture Notes

### Frontend (Elm)
- TEA architecture with discriminated union for page states: `Redirect | Lobby | EnterName | Playing`
- Player identities stored in localStorage as list: `[{playerName, token}]`
  - Bidirectional sync: JavaScript immediately notifies Elm when players are saved
  - Prevents data loss during state transitions
  - Provides autocomplete for returning players
- Board perspective rotated for Black player (see `transformGameMove`)
- Polls server for updates (no WebSocket)

### Client Flow (Page State Transitions)                                                                                                                                                          

**1. Init → Redirect**
- App starts in `Redirect` state with empty storedPlayers
- Immediately fetches game summaries from server
- Subscribes to localStorage to load stored player identities
             
**2. Redirect → Lobby or EnterName**
- When summaries arrive and storedPlayers load:
  - If URL is "/" (or invalid): → `Lobby` with game list
  - If URL is "/{gameId}" and game exists: → `EnterName` for that game
  - If URL is "/{gameId}" but game doesn't exist: → `Lobby` and redirect to "/"
                                                                                                                                                                                                  
**3. Lobby**
- User can:
  - Create new game → navigates to "/{newGameId}" → `EnterName`
  - Click existing game → navigates to "/{gameId}" → `EnterName`
- Polls for lobby updates every 3 seconds
                                                                                                                                                                                                  
**4. EnterName**
- Three substates:
  - `Entering name`: Shows input form with autocomplete from storedPlayers 
  - `Joining name`: Waiting for server join response
  - `JoinError name error`: Server rejected join, shows error with retry
- On submit:
  - Looks up token from storedPlayers by matching playerName
  - Sends join request with name and optional token to server
- On success: → `Playing`
                                                                                                                                                                                                  
**5. Playing**
- Receives game state and token from server
- Saves new PlayerIdentity to localStorage (JS immediately syncs back to Elm)
- Renders game board from player's perspective (Black rotated 180°)
- Polls server for game state updates every 2 seconds (stops when GameOver)
- User makes move:
  - Game state updates locally to `MoveDone`
  - Move sent to server
  - On confirmation: game state updated with move
- Auto-concedes if client detects player has lost
- URL navigation away from game: → `Redirect` to re-evaluate
                                                                                                                                                                                                  
**Key Patterns:**
- `storedPlayers: List PlayerIdentity` is threaded through all states for autocomplete/token lookup
- **localStorage synchronization**: When Elm saves a player via port, JavaScript updates localStorage then immediately sends the fresh list back to Elm via subscription, keeping all model states in sync
- Move history comparison: Server history vs local history to detect opponent moves
- Perspective transformation: Black player's moves rotated before sending, opponent moves rotated on receive
         

### Backend (Haskell)
- Type-safe API with Servant
- TVar/STM for concurrent game state
- UUID-based session tokens per player per game
- Auto-saves to `gamedb.json` (configurable interval, default: 30 seconds)
- **No magic numbers**: All timeouts, intervals, and ports are configurable via CLI or config file

### Important Patterns

**Move History:**
- Append-only list (head = most recent, functional style)
- Server history compared with local history to detect opponent moves

**Cards:**
- 16 standard Onitama cards with hardcoded movement patterns

**localStorage Persistence:**
- Player identities (name + token pairs) stored in browser localStorage
- Bidirectional port communication pattern:
  1. Elm → `savePlayer` port → JavaScript updates localStorage
  2. JavaScript → `loadPlayers` subscription → Elm receives updated list
  3. This immediate sync prevents data loss during page state transitions
- Implemented in `assets/localStorage.js` and `client/src/Ports.elm`
- Enables autocomplete for returning players and seamless token reuse

## Testing

- **Haskell**: hspec framework in `server/test/` (currently minimal)
- **Elm**: No tests yet (can add with elm-test)

## TODO
- See README.md

### 1. ✅ COMPLETED: Timestamps for Game Lifecycle Management
**Status: COMPLETED** (2026-01-18)

**Implementation Details**:
- Added `createdAt` and `lastActivity` fields to `Game` type
- Changed history to `[(GameMove, UTCTime)]` for per-move timestamps
- Timestamps stored in ISO8601 format in `gamedb.json`
- Automatic cleanup based on game status:
  - **WaitingForPlayers**: 2 hours
  - **InProgress**: 24 hours
  - **Completed**: 72 hours
- Background cleanup thread runs every 10 minutes
- Lobby displays relative time ("5 min ago", "2 hr ago") for created/lastActivity
- `lastActivity` updates on: game creation, player joining, move submission
- No update on polling (passive viewing doesn't extend game lifetime)

### 2. Add Footer Component
**Priority: MEDIUM** (polish)

**Frontend (Elm)**:
- Create `client/src/Footer.elm` with reusable footer view
- Include:
  - "Made with Elm and Haskell ❤️" (user preference on emoji)
  - GitHub link: `https://github.com/Tasm-Devil/Onitama`
  - Game rules link (optional)
  - Version/commit hash (from build-time env var?)
- Import and display in:
  - `Lobby.elm` (always visible)
  - `Game/Game.elm` (optional, may clutter during play)
  - `Main.elm` on error/redirect pages

**Styling**:
- Fixed bottom or bottom-of-page?
- Match current minimal aesthetic

### 3. Improve CSS and Visual Design
**Priority: LOW** (can be iterative)

**Considerations**:
- Keep it lightweight (no heavy frameworks unless justified)
- Mobile responsive (media queries)
- Better board/card aesthetics:
  - Card hover effects
  - Piece movement animations
  - Board grid improvements
- Consider:
  - Tailwind CSS (utility-first, tree-shakeable)
  - Pure CSS custom properties (simple, no build step)
  - Current approach works, just needs refinement

**Subtasks** (can be broken down later):
- Responsive layout for mobile
- Hover states and transitions
- Better color scheme
- Accessible focus states

### 4. ✅ COMPLETED: Command-Line Options to Server
**Status: COMPLETED - All Phases** (2026-01-18)

**Implementation Details**:
- Added `optparse-applicative` and `yaml` dependencies
- Created `server/src/Options.hs` with full option parsing and YAML config file support
- Updated `server/app/Main.hs` to parse options, handle version flag, and bind to specified host
- Refactored `Database.hs` to accept configurable save interval and cleanup configuration
- Modified `DB` type to include file path (not just TVar)
- Updated `App.hs` with `appWithConfig` function and cleanup options converter
- Background threads now use configurable intervals instead of hardcoded values
- **Eliminated all magic numbers**: Single source of truth in `Options.hs` for all defaults
- **Removed dead code**: Deleted unused `app :: IO Application` function from App.hs
- **Removed duplicate defaults**: Consolidated cleanup config defaults into Options.hs only

**All Implemented Options**:
```bash
# Basic Options (Phase 1 & 2)
-v, --verbose                 Enable verbose HTTP logging (logStdoutDev)
-p, --port PORT               Server port (default: 8080)
--host HOST                   Bind address (default: "0.0.0.0")
-d, --database FILE           Database file path (default: "gamedb.json")
--reset-db                    Start with fresh empty database

# Advanced Options (Phase 3 & 4)
--save-interval SECS          Database save interval in seconds (default: 30)
--cleanup-waiting HOURS       Hours before cleaning waiting games (default: 2.0)
--cleanup-active HOURS        Hours before cleaning active games (default: 24.0)
--cleanup-completed HOURS     Hours before cleaning completed games (default: 72.0)
--cleanup-interval MINS       Minutes between cleanup checks (default: 10)
--no-cleanup                  Disable automatic game cleanup

# Config & Info
-c, --config FILE             Config file path (default: ./onitama-server.yaml)
--version                     Show version information (0.1.0.0)
-h, --help                    Show help message
```

**Configuration File Support**:
- YAML format with option precedence: **defaults < config file < CLI arguments**
- Example config file at `onitama-server.example.yaml`
- CLI flags always override config file values
- Config file is optional (uses `./onitama-server.yaml` by default if exists)

**Usage Examples**:
```bash
# Default settings
./server

# Development mode with verbose logging
./server -v -p 3000

# Production with custom database and host binding
./server --host 127.0.0.1 --database /var/lib/onitama/games.json

# Testing with fresh database and custom cleanup
./server -v --reset-db --cleanup-waiting 1.0 --cleanup-interval 5

# Disable cleanup for testing
./server --no-cleanup --save-interval 60

# Use config file
./server --config /etc/onitama/server.yaml

# Show version
./server --version
```

## Known Limitations

- No server-side move validation (cheating possible by design - see Architecture Philosophy)
- No checkmate/win detection on server (client-only)
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
