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

## Testing

```bash
make test         # Server test suite
make demo-pass    # Demo game in which you must pass, http://localhost:8080/1
make ai-match     # Let two AI search depths play against each other
```

**`make test`** runs the hspec suite in `server/test/Spec.hs`. It covers move notation, rule enforcement and win detection, positions without a legal move, the minimax search and an end-to-end HTTP game against the AI.

**`make demo-pass`** starts the server with a prepared game from `server/test/fixtures/must-pass.json`. You play White as the dev user, and none of your pieces can move, so the board offers your two cards to pass with. The server works on a temporary copy, so every run starts from the same position. Stop your normal server first, because both use port 8080.

**`make ai-match`** pits two minimax depths against each other and prints wins and search times per move. Games are reproducible through their seed.

```bash
make ai-match ARGS="--white-depth 3 --black-depth 5 --games 20 --cards mixed"
stack exec -- ai-match --help
```

AI log lines on the server start with `[AI] game <id>:`, for example `[AI] game 1: plays b:c2c4:kirin`.

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
| Auth | GitHub OAuth via oauth2-proxy |
| Real-time | Server-Sent Events |
| Persistence | In-memory (STM); optional JSON file via `--database` |

The server validates all moves against Onitama rules and handles win detection. The client renders the board and applies moves received via SSE. Authentication is handled by oauth2-proxy, which sets `X-Forwarded-User` and `X-Forwarded-Preferred-Username` headers.

---

## API

Base: `http://localhost:8080/1/onitama`

### REST

| Method | Endpoint | Body | Returns |
|--------|----------|------|---------|
| GET | `/me` | - | `AuthUser` |
| POST | `/games` | `{newGameVsAI, newGameCardSet}` | `Either JoinError NewGameResponse` |
| GET | `/games` | - | `[GameSummary]` |
| GET | `/games/{id}` | - | `GameWithNames` |
| POST | `/games/{id}/players` | - | `Either JoinError JoinGameResponse` |
| POST | `/games/{id}/moves` | `MoveNotation` | `Either MoveError MoveNotation` |
| POST | `/games/{id}/concede` | - | `Either ConcedeError Color` |
| GET | `/{gameId}` | - | Serves `index.html` (client-side routing) |

Authenticated endpoints use `X-Forwarded-User` and `X-Forwarded-Preferred-Username` headers (set by oauth2-proxy, or dev middleware).

`MoveNotation` is `<color>:<from><to>:<card>`, for example `w:c1c3:tiger`. A player without any legal move must still exchange a card, written as `<color>:pass:<card>`, for example `b:pass:fox`. The server rejects a pass while a normal move exists.

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
server --dev                 # Dev mode (inject auth headers)
server --dev-user alice      # Dev mode with custom user name
```

See `onitama-server.example.yaml` for all options.

### Dev Mode (Two-Player Testing)

`make server-start` runs the server with `--dev`, which injects auth headers automatically when they're missing. Every browser request is authenticated as the default user (`"dev"`).

To test two-player games locally, use a header-injection browser extension like [ModHeader](https://modheader.com/):

1. **Tab 1** — browse normally (no extension rules needed). You'll be user `"dev"`.
2. **Tab 2** — use ModHeader to set:
   - `X-Forwarded-User: player2`
   - `X-Forwarded-Preferred-Username: Player 2`

The server identifies players by the `X-Forwarded-User` header value, so the two tabs are seen as different users. No incognito window needed.

---

## Docker

```bash
make docker        # Build client + Docker image (server is compiled inside) + save tar
make docker-start  # Build + run on http://localhost:8080
```

The server binary is compiled inside the image (multi-stage `Dockerfile`), so it does not depend on the glibc version of the build machine. The first build compiles all Haskell dependencies and takes a while; later builds reuse the cached dependency layer.

Deployment uses oauth2-proxy for GitHub OAuth + Nginx Proxy Manager as reverse proxy. See `.env.example` for required secrets and `docker-compose.yml` for the full stack.

---

## Why Haskell + Elm?

**Haskell** is a natural fit for a board game server: algebraic data types model game state precisely, pattern matching expresses game rules clearly, and pure functions keep logic easy to test and reason about. STM (Software Transactional Memory) makes concurrent multiplayer state safe without manual locking, and Servant provides type-safe API routing with minimal boilerplate.

**Elm** brings the same philosophy to the frontend: a strong type system, immutable data, and The Elm Architecture (TEA) make UI state predictable. If it compiles, it runs — no runtime exceptions.

Together they enable fearless refactoring: the compilers catch structural errors, so changes compose like building blocks rather than a house of cards.

---

## Roadmap

- [x] SSE real-time updates
- [x] Automatic game cleanup
- [x] CLI & YAML config
- [x] Server-side move validation
- [x] Dark mode (prefers-color-scheme) and CSS custom properties
- [x] Spectator mode
- [x] AI opponent (minmax 5-ply)
- [x] Sensei's Path expansion cards (optional at game creation)
- [x] GitHub OAuth via oauth2-proxy
- [x] App header with user avatar, game status, and logout
- [x] Drag-to-move support (desktop & mobile via pointer events)
- [x] Passing when no legal move exists (official rule, also for the AI)
- [x] Server test suite, AI-vs-AI match tool and pass demo
- [ ] Make a howto play
- [ ] Move playback showing GameState
- [ ] Choose different AI depths
- [ ] Multi-language support (i18n via browser language)

---

## Acknowledgments

- Game design: Shimpei Sato (Arcane Wonders)
- Inspiration: [Lanny/Onitama](https://github.com/Lanny/Onitama)
