# Onitama - Project Context

Multiplayer web implementation of the abstract board game Onitama.

## Tech Stack

- **Frontend**: Elm 0.19.1 (TEA pattern)
- **Backend**: Haskell with Servant framework
- **Persistence**: JSON file (`gamedb.json`)
- **Concurrency**: STM (Software Transactional Memory)

## Build Commands

```bash
make all           # Full setup + build
make server-start  # Build and run server on port 8080
make client-build  # Build Elm frontend only
make server-build  # Build Haskell backend only
make test          # Run all tests
make clean         # Remove build artifacts
```

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
```

## API Endpoints

Base URL: `http://localhost:8080/1/onitama`

| Method | Path | Purpose |
|--------|------|---------|
| POST | `/new` | Create new game |
| GET | `/summary` | List all games |
| PUT | `/` | Join game (query: table, name, token?) |
| GET | `/` | Get game state (query: table) |
| POST | `/` | Submit move (query: table, token) |

## Architecture Philosophy

**Game-Agnostic Server**: The server is intentionally designed to be game-agnostic. It acts as a pure "move broker" or "state store" with no knowledge of game rules:
- Stores game state (moves/history)
- Manages sessions and players
- Notifies clients of updates

**All game logic lives in clients**:
- Move validation happens client-side only
- Win condition detection is client-side
- Card/piece rules are client-side

**Future: Referee Client**: For competitive play, a separate "Referee" client can be implemented that:
- Gets notified by the server when moves are posted
- Validates moves against game rules
- Flags invalid moves or disputed games
- This keeps the server single-purpose while enabling optional validation

**Benefits of this approach**:
- Server infrastructure is reusable for any turn-based game
- Game logic updates don't require server redeployment
- Referee can be optional (casual vs competitive modes)
- Clean separation of concerns

## Architecture Notes

### Frontend (Elm)
- TEA architecture with discriminated union for page states: `Redirect | Lobby | EnterName | Playing | Rejoining`
- Session stored in localStorage (gameId, playerName, token)
- Board perspective rotated for Black player (see `transformGameMove`)
- Polls server for updates (no WebSocket)

### Backend (Haskell)
- Type-safe API with Servant
- TVar/STM for concurrent game state
- UUID-based session tokens per player per game
- Auto-saves to `gamedb.json` every 30 seconds

### Important Patterns
- Move history is append-only (head = most recent, functional style)
- Cards: 16 standard Onitama cards with hardcoded movement patterns

## Testing

- **Haskell**: hspec framework in `server/test/` (currently minimal)
- **Elm**: No tests yet (can add with elm-test)

## TODO

### 1. Add Timestamps for Game Lifecycle Management
**Priority: HIGH** (required for production)

**Backend (Haskell)**:
- Add `createdAt :: UTCTime` field to `Game` type in `server/src/Game.hs`
- Add `lastActivity :: UTCTime` field to `Game` type
- Add `timestamp :: UTCTime` field to each move in history (make history `[(GameMove, UTCTime)]`)
- Update `lastActivity` on every move submission
- Add periodic cleanup job to delete abandoned games (e.g., `lastActivity > 24 hours`)
- Ensure timestamps are serialized in ISO8601 format in `gamedb.json`

**Frontend (Elm)**:
- Update `Game` decoder in `client/src/Api.elm` to parse timestamps
- Display "Last activity: X minutes ago" in lobby for each game
- Add visual indicator for stale games (e.g., grayed out if > 1 hour inactive)

**Database Schema Change**:
```json
{
  "dbGames": {
    "1": {
      "createdAt": "2026-01-13T10:30:00Z",
      "lastActivity": "2026-01-13T10:35:00Z",
      "cards": [...],
      "history": [
        {"move": "w:c1c3:tiger", "timestamp": "2026-01-13T10:35:00Z", "playerId": "1"}
      ],
      ...
    }
  }
}
```

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

### 4. Migrate to Per-Player Token System
**Priority: HIGH** (architectural change, do before adding more features)

**Current**: Token per game per player `[{gameId, playerName, token}]`
**Target**: Token per player globally `[{playerId, playerName, token}]`

**Backend (Haskell)**:
- Create new `Player` type in `server/src/Database.hs`:
  ```haskell
  data Player = Player
    { playerId   :: PlayerId    -- Integer
    , playerName :: Text
    , playerToken :: SessionToken -- UUID
    , createdAt  :: UTCTime
    }
  ```
- Add `dbPlayers :: Map PlayerId Player` to database
- Add `dbNextPlayerId :: Int` counter
- Change `Game` to reference `PlayerId` instead of player names:
  ```haskell
  data Game = Game
    { gameCards        :: [Card]
    , gameHistory      :: [(GameMove, UTCTime, PlayerId)]
    , gamePlayerWhite  :: Maybe PlayerId
    , gamePlayerBlack  :: Maybe PlayerId
    , gameWinner       :: Maybe Winner
    , gameCreatedAt    :: UTCTime
    , gameLastActivity :: UTCTime
    }
  ```
- Update API endpoints to work with player IDs:
  - `/new` returns `PlayerId` on first join
  - Join endpoint creates/reuses player record
  - Token validates against `dbPlayers`, not per-game

**Frontend (Elm)**:
- Update localStorage structure:
  ```javascript
  // Old: [{ gameId: 1, playerName: "Alice", token: "..." }]
  // New: { playerId: "1", playerName: "Alice", token: "..." }
  ```
- Update `Ports.elm` to store single player identity
- Update API client to send player ID with requests
- Handle name changes (allow player to update display name?)

**Database Migration**:
- Write migration script to convert existing `gamedb.json`
- Extract unique players from existing games
- Generate player IDs and assign to games
- Preserve existing tokens if possible (or invalidate and require re-login)

**Database Schema (Final)**:
```json
{
  "dbGames": {
    "1": {
      "createdAt": "2026-01-13T10:30:00Z",
      "lastActivity": "2026-01-13T10:35:00Z",
      "cards": ["Boar", "Elephant", "Crane", "Ox", "Tiger"],
      "history": [
        {"move": "w:c1c3:tiger", "timestamp": "2026-01-13T10:35:00Z", "playerId": "1"}
      ],
      "player_black": null,
      "player_white": "1",
      "winner": null
    }
  },
  "dbPlayers": {
    "1": {
      "name": "Alice",
      "token": "da7db216-61d9-46ec-b1ce-d931aab6b111",
      "createdAt": "2026-01-10T08:00:00Z"
    }
  },
  "dbNextGameId": 2,
  "dbNextPlayerId": 2,
  "dbHasChanged": true
}
```

**Benefits**:
- Consistent player identity across games
- Easier to add future features (stats, match history, ELO rating)
- Natural fit for eventual relational DB migration
- No global name collisions (player ID is unique)

**Tradeoffs**:
- More complex migration from current system
- Player names become mutable (need UI to change them?)
- Slightly more DB lookups (player ID -> name for display)

### 5. Add Command-Line Options to Server
**Priority: LOW** (quality of life improvement)

**Backend (Haskell)**:
- Add command-line argument parsing (use `optparse-applicative` package)
- Support options like:
  - `-v, --verbose`: Enable verbose request logging (logStdoutDev)
  - `-p, --port PORT`: Specify custom port (default 8080)
  - `-d, --database FILE`: Specify custom database file path
  - `--log-file FILE`: Log to file instead of stdout
- Update `server/app/Main.hs` to conditionally enable logStdoutDev based on verbose flag
- Currently, verbose logging must be manually uncommented in source code

**Example Usage**:
```bash
./server -v -p 3000  # Run on port 3000 with verbose logging
./server --database /data/games.json  # Custom database location
```

**Note**: The logStdoutDev import is already available in Main.hs with instructions on how to enable it manually.

## Known Limitations

- No server-side move validation (cheating possible by design - see Architecture Philosophy)
- No checkmate/win detection on server (client-only)
- HTTP polling only (WebSocket planned)
- No chat feature
- No expansion cards (Sensei's Path)

## Docker

```bash
make all
docker build -t onitama:latest .
docker run -p 8080:8080 onitama:latest
```
