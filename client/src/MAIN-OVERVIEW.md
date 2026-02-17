# Main.elm Overview

## Model

```
Model
  key            : Nav.Key          -- browser navigation
  storedPlayers  : List PlayerSession   -- from localStorage (name + token pairs)
  page           : Page
```

## Pages (State Machine)

```
Redirect Url                              -- transient: waiting for data to decide which page
LobbyPage Lobby.Model                     -- game list
EnterNamePage GameCreation EnterName.Model -- name entry before join/create
GamePage GameId (Maybe PlayerSession) Game (List LogEntry)  -- playing, spectating, or reviewing
```

`GameCreation` tells `EnterNamePage` what to do on submit:
- `JoinExisting GameId` -- join an existing game
- `CreateNew`           -- create multiplayer game
- `CreateNewVsAI`       -- create AI game

## Messages

| Msg | Source | Handler |
|-----|--------|---------|
| `ChangedUrl` | Browser | `handleUrlChange` |
| `ClickedLink` | Browser | `handleClickedLink` |
| `GotGameMsg` | Game board UI | `handleGameMsg` |
| `GotEnterNameMsg` | Name entry UI | `handleEnterNameMsg` |
| `StoredPlayersLoaded` | localStorage port | inline (just stores) |
| `GotServerMsg` | HTTP responses | `handleServerMsg` (dispatches below) |
| `LobbyEventReceived` | Lobby SSE port | `handleLobbyEvent` |
| `GameEventReceived` | Game SSE port | `handleGameEvent` |
| `Tick` | 1-min timer | updates `currentTime` on LobbyPage |

### Server sub-messages (`Api.Msg`, wrapped in `GotServerMsg`)

| Api.Msg | Handler |
|---------|---------|
| `ReceivedGameSummariesFromServer` | `handleGameSummaries` |
| `ReceivedJoinGameResponse` | `handleJoinResponse` |
| `ReceivedNewGameResponse` | `handleNewGameResponse` |
| `ReceivedPostCreatedFromServer` | `handleMoveConfirmation` |
| `ReceivedConcedeResponse` | ignored (applied via SSE) |
| `ReceivedGameFromServer` | `handleSpectateGame` |

## Navigation Flows

### App startup
```
init
  page = Redirect url
  cmd  = fetch game summaries
           |
           v
handleGameSummaries (Redirect url)
  parse url.path:
    "/"           --> LobbyPage + open lobby SSE
    "/newgame"    --> EnterNamePage CreateNew
    "/newgame-ai" --> EnterNamePage CreateNewVsAI
    "/{id}" exists & shouldJoin --> EnterNamePage (JoinExisting id)
    "/{id}" exists & spectate  --> fetch game --> handleSpectateGame --> GamePage (no session)
    "/{id}" not found          --> LobbyPage (redirect to /)
```

### Create new game (multiplayer)
```
LobbyPage -- click "New Game" --> Nav.pushUrl "/newgame"
  |
  v
handleUrlChange (LobbyPage, "newgame")
  page = EnterNamePage CreateNew
  cmd  = closeLobbyStream
  |
  v  (user types name, submits)
handleEnterNameMsg (RequestJoin)
  --> handleRequestGame (CreateNew)
  cmd = Api.createGame name False token
  |
  v
handleNewGameResponse (Ok)
  --> joinGameSuccess
  page = GamePage id (Just session) game log
  cmd  = save player, open game SSE, replaceUrl "/{id}"
```

### Create AI game
```
LobbyPage -- click "Play vs AI" --> Nav.pushUrl "/newgame-ai"
  |
  v
handleUrlChange (LobbyPage, "newgame-ai")
  page = EnterNamePage CreateNewVsAI
  cmd  = closeLobbyStream
  |
  v  (user types name, submits)
handleEnterNameMsg (RequestJoin)
  --> handleRequestGame (CreateNewVsAI)
  cmd = Api.createGame name True token
  |
  v
handleNewGameResponse (Ok)
  --> joinGameSuccess
  page = GamePage id (Just session) game log
  cmd  = save player, open game SSE, replaceUrl "/{id}"
```

### Join existing game
```
LobbyPage -- click game row --> Nav.pushUrl "/{id}"
  |
  v
handleUrlChange (LobbyPage, "{id}")
  shouldJoinGame? (returning player OR WaitingForPlayers, AND NOT Completed)
    yes --> page = EnterNamePage (JoinExisting id), closeLobbyStream
    no  --> page = Redirect url, fetch game for spectating
  |
  v  (if joining: user types name, submits)
handleEnterNameMsg (RequestJoin)
  --> handleRequestGame (JoinExisting id)
  cmd = Api.joinGame id name token
  |
  v
handleJoinResponse (Ok)
  --> joinGameSuccess
  page = GamePage id (Just session) game log
```

### Spectate / Review
```
handleUrlChange (LobbyPage, "{id}")  -- shouldJoinGame = false
  page = Redirect url
  cmd  = closeLobbyStream, fetch game
  |
  v
handleSpectateGame (Ok, Redirect)
  page = GamePage id Nothing (spectating=True) log
  cmd  = openGameStream
```

## In-Game Event Handling

### Game SSE events (`handleGameEvent`)
- **MoveEvent**: parse move string, apply to game, set winner if present, play sound
- **ConcedeEvent**: set GameOver state
- **PlayerJoinedEvent**: add log entries, transition from WaitingForOpponent to Thinking

### Move submission
```
GotGameMsg (board interaction)
  --> Game.update produces MoveDone
  --> Api.postNewGameMove
  |
  v
handleMoveConfirmation
  Ok  --> no-op (move applied via SSE)
  Err --> revert game state to Thinking
```

### Lobby SSE events (`handleLobbyEvent`)
- Any event --> refetch game summaries

## Subscriptions (by page)

| Page | Subscriptions |
|------|---------------|
| All pages | `Ports.loadPlayers` (localStorage sync) |
| LobbyPage | `Ports.lobbyEventReceived` + 1-min `Tick` |
| GamePage (not GameOver) | `Ports.gameEventReceived` |
| EnterNamePage, Redirect | none (besides localStorage) |

## Key Helper Functions

| Function | Purpose |
|----------|---------|
| `shouldJoinGame` | Decide join vs spectate: true if (returning player OR waiting) AND NOT completed |
| `buildGame` | Construct Game from ServerGame + player name, replay move history |
| `buildSpectatorGame` | Like buildGame but from White's perspective, with `spectating = True` |
| `joinGameSuccess` | Shared transition to GamePage after any successful join/create |
| `parseWinnerColor` | Parse "White"/"Black" string to Color |

## Side Effects Summary

| Effect | When |
|--------|------|
| `Ports.openLobbyStream` | Entering LobbyPage |
| `Ports.closeLobbyStream` | Leaving LobbyPage |
| `Ports.openGameStream` | Entering GamePage |
| `Ports.closeGameStream` | Leaving GamePage |
| `Ports.savePlayer` | After successful join/create |
| `Ports.playSound` | On receiving move via SSE |
| `Nav.replaceUrl` | After join/create (update URL to /{id}) |
| `Nav.pushUrl` | Link clicks in lobby |
