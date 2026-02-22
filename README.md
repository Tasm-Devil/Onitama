# Onitama

A multiplayer web implementation of [Onitama](https://www.arcanewonders.com/product/onitama/), built with **Elm** and **Haskell**.

```
      a   b   c   d   e
    +---+---+---+---+---+
  5 | p | p | K | p | p |  Black
    +---+---+---+---+---+
  4 |   |   |   |   |   |
    +---+---+---+---+---+
  3 |   |   |   |   |   |
    +---+---+---+---+---+
  2 |   |   |   |   |   |
    +---+---+---+---+---+
  1 | P | P | K | P | P |  White
    +---+---+---+---+---+
```

> **Play now**: [onitama.deneaux.de](https://onitama.deneaux.de/)

---

## Quick Start

```bash
make all          # Build everything
make server-start # Run on http://localhost:8080
```

---

## Architecture

```
+------------------+    SSE     +-------------------+
|   Elm Client     | <--------- |  Haskell Server   |
|                  |    HTTP    |                   |
|  - UI/UX         | ---------> |  - Move validation|
|  - Move display  |            |  - Win detection  |
|                  |            |  - Sessions/SSE   |
+------------------+            +-------------------+
```

| Layer | Technology |
|-------|------------|
| Frontend | Elm 0.19.1 (TEA) |
| Backend | Haskell + Servant |
| Real-time | Server-Sent Events |
| Persistence | In-memory (STM); optional JSON file via `--database` |

The server validates all moves against Onitama rules and handles win detection. The client renders the board and applies moves received via SSE.

---

## API

Base: `http://localhost:8080/1/onitama`

### REST

| Method | Endpoint | Body | Headers | Returns |
|--------|----------|------|---------|---------|
| POST | `/games` | `{newGamePlayerName, newGameVsAI, newGameCardSet}` | `X-Session-Token?` | `Either JoinError NewGameResponse` |
| GET | `/games` | - | - | `[GameSummary]` |
| GET | `/games/{id}` | - | - | `GameWithNames` |
| POST | `/games/{id}/players` | `{joinPlayerName}` | `X-Session-Token?` | `Either JoinError JoinGameResponse` |
| POST | `/games/{id}/moves` | `MoveNotation` | `X-Session-Token` | `Either MoveError MoveNotation` |
| POST | `/games/{id}/concede` | - | `X-Session-Token` | `Either ConcedeError Color` |
| GET | `/{gameId}` | - | - | Serves `index.html` (client-side routing) |

### SSE Streams

| Endpoint | Events |
|----------|--------|
| `/games/stream` | `lobbyChanged` (client refetches summaries) |
| `/games/{id}/stream` | `move`, `concede`, `playerJoined` |

```bash
# Test SSE
curl -N localhost:8080/1/onitama/games/stream    # Lobby events
curl -N localhost:8080/1/onitama/games/1/stream  # Game 1 events
```

---

## Configuration

```bash
server --port 3000           # Custom port
server --verbose             # HTTP logging
server --config server.yaml  # YAML config
server --no-cleanup          # Disable auto-cleanup
```

See `onitama-server.example.yaml` for all options.

---

## Docker

```bash
make docker        # Build release + Docker image + save tar
make docker-start  # Build + run on http://localhost:8080
```

---

## Why Haskell + Elm?

**Haskell** is a natural fit for a board game server: algebraic data types model game state precisely, pattern matching expresses game rules clearly, and pure functions keep logic easy to test and reason about. STM (Software Transactional Memory) makes concurrent multiplayer state safe without manual locking, and Servant provides type-safe API routing with minimal boilerplate.

**Elm** brings the same philosophy to the frontend: a strong type system, immutable data, and The Elm Architecture (TEA) make UI state predictable. If it compiles, it runs — no runtime exceptions.

Together they enable fearless refactoring: the compilers catch structural errors, so changes compose like building blocks rather than a house of cards.

---

## Roadmap

- [x] SSE real-time updates
- [x] Per-player token system
- [x] Automatic game cleanup
- [x] CLI & YAML config
- [X] Server-side move validation
- [x] Dark mode (prefers-color-scheme) and CSS custom properties
- [X] Spectator mode
- [X] AI opponent (minmax 5-ply)
- [x] Sensei's Path expansion cards (optional at game creation)
- [ ] Move playback showing GameState
- [ ] Choose different AI depths
- [ ] Multi-language support (i18n via browser language)
- [ ] Traefik + Authelia for auth

---

## Acknowledgments

- Game design: Shimpei Sato (Arcane Wonders)
- Inspiration: [Lanny/Onitama](https://github.com/Lanny/Onitama)
