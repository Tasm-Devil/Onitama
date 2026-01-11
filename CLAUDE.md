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

## Known Limitations / TODOs

- No server-side move validation (cheating possible)
- No checkmate/win detection on server
- HTTP polling only (WebSocket planned)
- No chat feature
- No expansion cards (Sensei's Path)

## Docker

```bash
make all
docker build -t onitama:latest .
docker run -p 8080:8080 onitama:latest
```
