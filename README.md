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
+------------------+    SSE     +------------------+
|   Elm Client     | <--------- |  Haskell Server  |
|                  |    HTTP    |                  |
|  - Game rules    | ---------> |  - State store   |
|  - UI/UX         |            |  - Sessions      |
|  - Win detection |            |  - SSE broadcast |
+------------------+            +------------------+
```

| Layer | Technology |
|-------|------------|
| Frontend | Elm 0.19.1 (TEA) |
| Backend | Haskell + Servant |
| Real-time | Server-Sent Events |
| Persistence | JSON file (STM) |

The server is **game-agnostic** - it stores moves as opaque strings. All game logic lives in the client.

---

## API

Base: `http://localhost:8080/1/onitama`

### REST

| Method | Endpoint | Body | Headers | Returns |
|--------|----------|------|---------|---------|
| POST | `/games` | - | - | `GameId` |
| GET | `/games` | - | - | `[GameSummary]` |
| GET | `/games/{id}` | - | - | `GameWithNames` |
| POST | `/games/{id}/players` | `{joinPlayerName}` | `X-Session-Token?` | `Either JoinError JoinGameResponse` |
| POST | `/games/{id}/moves` | `GameMove` | `X-Session-Token` | `Either MoveError GameMove` |
| POST | `/games/{id}/concede` | - | `X-Session-Token` | `Either ConcedeError Color` |
| GET | `/newgame` | - | - | Serves `index.html` (creates game via client) |

### SSE Streams

| Endpoint | Events |
|----------|--------|
| `/games/stream` | `lobbyChanged` (client refetches summaries) |
| `/games/{id}/stream` | `move`, `concede` |

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
make release
docker build -t onitama .
docker run -p 8080:8080 onitama
docker save onitama:latest > onitama.tar
```

---

## Roadmap

- [x] SSE real-time updates
- [x] Per-player token system
- [x] Automatic game cleanup
- [x] CLI & YAML config
- [x] Game list in lobby: newest first
- [x] Fix "just now" timestamp not updating
- [X] Mobile responsive table width
- [x] Simplify lobby SSE to invalidate+refetch pattern
- [ ] Server-side move validation (break game-agnostic design)
- [ ] CSS improvements (responsive, animations)
- [ ] Docker: volumes for DB/config, versioning
- [ ] Spectator mode
- [ ] Move playback
- [ ] Sensei's Path expansion cards
- [ ] Traefik + Authelia for auth
- [ ] AI opponent

---

## Acknowledgments

- Game design: Shimpei Sato (Arcane Wonders)
- Inspiration: [Lanny/Onitama](https://github.com/Lanny/Onitama)
