# Onitama

A multiplayer web implementation of the elegant abstract strategy game [Onitama](https://www.arcanewonders.com/product/onitama/), built with **Elm** and **Haskell**.

```
       Black's Temple
              v
    +---+---+---+---+---+
    | p | p | K | p | p |   Black
    +---+---+---+---+---+
    |   |   |   |   |   |
    +---+---+---+---+---+
    |   |   |   |   |   |
    +---+---+---+---+---+
    |   |   |   |   |   |
    +---+---+---+---+---+
    | P | P | K | P | P |   White
    +---+---+---+---+---+
              ^
       White's Temple
```

Two players face off with 5 pieces each (1 King + 4 Pawns). Each turn, play a movement card to move one piece, then swap that card with the neutral card. Capture the opponent's King or move your King to their Temple to win.

> **Play the latest version**: [https://onitama.deneaux.de/](https://onitama.deneaux.de/) (only for testing)
> **Play the old serverless version**: [tasm-devil.github.io/Onitama](https://tasm-devil.github.io/Onitama/) (no server required)

---

## Quick Start

```bash
# Prerequisites: stack, elm, elm-test

# Build everything
make all

# Start the server (includes hot-reload for client)
make server-start

# Open http://localhost:8080 in your browser
```

---

## Architecture

```
+------------------+          +------------------+
|                  |   HTTP   |                  |
|   Elm Client     | <------> |  Haskell Server  |
|                  |          |                  |
|  - Game rules    |          |  - State store   |
|  - UI/UX         |          |  - Sessions      |
|  - Win detection |          |  - Persistence   |
+------------------+          +------------------+
```

### Design Philosophy: Game-Agnostic Server

The server intentionally knows **nothing about Onitama rules**. It's a pure "move broker":

| Server Does | Server Does NOT |
|------------|-----------------|
| Store game state | Validate moves |
| Manage sessions | Check win conditions |
| Persist to JSON | Know card rules |
| Route messages | Enforce turn order* |

*Turn order is based on the common card's color stamp, which the server looks up.

**Why?** This separation means:
- Server code is reusable for any turn-based game
- Game logic updates don't require server redeployment
- A future "Referee" client could optionally validate moves for competitive play

### Tech Stack

| Layer | Technology | Pattern |
|-------|------------|---------|
| Frontend | Elm 0.19.1 | [TEA](https://guide.elm-lang.org/architecture/) (The Elm Architecture) |
| Backend | Haskell + Servant | Type-safe REST API |
| Persistence | JSON file | Auto-save every 30s via STM |

---

## Project Structure

```
client/src/
  Main.elm              # Routing, page states, game setup
  Api.elm               # HTTP client
  Lobby.elm             # Game browser UI
  Ports.elm             # localStorage interop
  Game/
    Game.elm            # Core game logic, board rendering
    Card.elm            # Card definitions + movement patterns
    Figure.elm          # Piece types (King/Pawn)
    Cell.elm            # Board cell rendering

server/src/
  Api.hs                # Servant route definitions
  App.hs                # Request handlers
  Game.hs               # Game data types + turn logic
  Database.hs           # JSON persistence, sessions

assets/                 # Static files served to browser
```

---

## API Reference

Base URL: `http://localhost:8080`

| Method | Endpoint | Query Params | Body | Description |
|--------|----------|--------------|------|-------------|
| `POST` | `/1/onitama/new` | - | - | Create new game, returns `GameId` |
| `GET` | `/1/onitama/summary` | - | - | List all games as `[GameSummary]` |
| `PUT` | `/1/onitama` | `table`, `name` | - | Join game (or rejoin with token header) |
| `GET` | `/1/onitama` | `table` | - | Get game state |
| `POST` | `/1/onitama` | `table` | `GameMove` | Submit a move (requires token header) |
| `POST` | `/1/onitama/concede` | `table` | - | Concede the game (requires token header) |
| `GET` | `/:gameId` | - | - | Serve game HTML page |

**Authentication**: Routes that modify game state require the `X-Session-Token` header.

### Data Types

```haskell
GameId       = Int
SessionToken = Text (UUID)
Color        = "White" | "Black"
GameStatus   = "WaitingForPlayers" | "InProgress" | "Completed"

-- GameMove is a human-readable string: "<color>:<from><to>:<card>"
-- Examples: "w:c1c3:tiger", "b:c5c4:crane"
-- Positions use chess notation: columns a-e, rows 1-5
GameMove = String

GameSummary = {
  summaryId:        GameId,
  summaryPlayer1:   String,
  summaryPlayer2:   String,
  summaryMoveCount: Int,
  summaryStatus:    GameStatus
}

JoinGameResponse = {
  responseGame:  Game,
  responseToken: SessionToken
}
```

### Example Session

```bash
# Create a new game
curl -X POST localhost:8080/1/onitama/new
# Returns: 1

# Alice joins as White
curl -X PUT "localhost:8080/1/onitama?table=1&name=Alice"
# Returns: {"responseGame": {...}, "responseToken": "abc-123..."}

# Bob joins as Black
curl -X PUT "localhost:8080/1/onitama?table=1&name=Bob"
# Returns: {"responseGame": {...}, "responseToken": "def-456..."}

# Alice makes a move (format: "color:from+to:card")
curl -X POST "localhost:8080/1/onitama?table=1" \
  -H "X-Session-Token: abc-123..." \
  -H "Content-Type: application/json" \
  -d '"w:c1c3:tiger"'

# Bob fetches the updated game state
curl "localhost:8080/1/onitama?table=1"
```

### Game State JSON

```json
{
  "cards": ["Tiger", "Crab", "Monkey", "Crane", "Dragon"],
  "history": ["b:c5c4:crane", "w:c1c3:tiger"],
  "player_white": "Alice",
  "player_black": "Bob",
  "winner": null
}
```

- **Cards order**: `[White1, White2, Black1, Black2, Common]`
- **History order**: Most recent move first (head of list = last move, FP style)

The common card (5th) determines who moves first based on its color stamp.

---

## Build Commands

| Command | Description |
|---------|-------------|
| `make all` | Full setup + build |
| `make server-start` | Build and run (port 8080), hot-reloads client |
| `make client-build` | Build Elm frontend only |
| `make server-build` | Build Haskell backend only |
| `make test` | Run all tests |
| `make clean` | Remove build artifacts |

### Docker

```bash
make all
docker build -t onitama:latest .
docker run -p 8080:8080 onitama:latest
```

---

## Roadmap

- [x] Sequential game IDs (instead of UUIDs)
- [x] Clean API structure `/VERSION/GAME?table=ID`
- [x] Session-based authentication per game
- [x] Persist sessions in localStorage
- [x] Win detection (capture King / reach Temple)
- [x] Common card determines starting player
- [x] Simplify move format to `w:c1c3:tiger`
- [x] Move token to HTTP header (`X-Session-Token`)
- [ ] Server sends random seed, client generates cards
- [ ] Global auth system (email + 6-digit code)
- [ ] WebSocket for real-time updates
- [ ] In-game chat
- [ ] Sensei's Path expansion cards

### Expansion Cards

- [Sensei's Path](https://www.gadgetsville.store/wp-content/uploads/2017/12/16096-c.jpg) - 16 additional cards
- [Promo Cards](https://www.arcanewonders.com/product/onitama-promo-cards/) - Special edition cards

---

## Acknowledgments

- Game design: Shimpei Sato, published by Arcane Wonders
- Inspiration: [Lanny/Onitama](https://github.com/Lanny/Onitama)
- Elm HTTP guide: [elmprogramming.com](https://elmprogramming.com/decoding-json-part-1.html)
- Servant + Elm integration: [example-servant-elm](https://github.com/haskell-servant/example-servant-elm)

---

*This is a learning project exploring functional programming on both frontend (Elm) and backend (Haskell). Contributions welcome!*
