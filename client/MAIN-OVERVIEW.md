# Main.elm Overview

## Model

```elm
type alias Model =
    { key : Key                              -- Browser navigation key
    , storedPlayers : List PlayerSession      -- All sessions from localStorage
    , currentPlayer : Maybe PlayerSession     -- Tab-level identity (set once, persists until refresh)
    , page : Page                             -- Current page state
    }

type alias PlayerSession = { name : String, token : String }
```

## Page State Machine

```elm
type Page
    = Loading Route                                    -- Initial load / navigation in flight
    | LobbyPage Lobby.Model                            -- Game list + create UI
    | EnterNamePage PendingAction EnterName.Model       -- Name prompt (shown once per tab)
    | AwaitingGame PendingAction String                 -- Spinner while API responds
    | GamePage GameId Game (List Game.LogEntry)         -- Active/completed game

type Route = LobbyRoute | GameRoute GameId

type PendingAction
    = PendingCreate Bool Lobby.CardSet   -- vsAI, cardSet
    | PendingJoin GameId
```

### Transitions

```
    Browser load
        |
        v
    Loading route ──── summaries arrive ──┬── LobbyRoute ─────────> LobbyPage
                                          |
                                          └── GameRoute id ────────> resolveGameNavigation
                                                                        |
                                          ┌── JoinDirectly ────────> AwaitingGame
                                          |── NeedIdentity ────────> EnterNamePage
                                          └── SpectateGame ────────> Loading (fetch game)

    LobbyPage
        |── "New Game" / "VS AI" ──> PickingCardSet (internal to Lobby.Model)
        |── PickingCardSet + Confirm ──┬── currentPlayer set ──────> AwaitingGame
        |                              └── no identity ────────────> EnterNamePage
        |── click game row ────────────> resolveGameNavigation (same as above)
        └── lobby SSE ─────────────────> refetch summaries (stays on LobbyPage)

    EnterNamePage
        └── submit name ──────────────> set currentPlayer ──> AwaitingGame

    AwaitingGame
        |── create success ────────────> GamePage (pushUrl /{id})
        |── join success ──────────────> GamePage
        └── error ─────────────────────> EnterNamePage (with error msg)

    GamePage
        |── game SSE (move/concede/playerJoined) ──> update game state
        |── user move ─────────────────> POST /moves (response = error handling only)
        |── user concede ──────────────> POST /concede
        └── navigate away ─────────────> Loading route (closes game stream)
```

## URLs

Only two URL patterns:
- `/` -- lobby
- `/{gameId}` -- game

No `/newgame` routes. Game creation is a button click in the lobby.

## Navigation Resolution

`resolveGameNavigation : Maybe PlayerSession -> List GameSummary -> GameId -> NavigationAction`

| Game Status      | currentPlayer        | Result          |
|------------------|----------------------|-----------------|
| Completed        | any                  | SpectateGame    |
| WaitingForPlayers| Just session         | JoinDirectly    |
| WaitingForPlayers| Nothing              | NeedIdentity    |
| InProgress       | Just (matches player)| JoinDirectly    |
| InProgress       | Just (no match)      | SpectateGame    |
| InProgress       | Nothing              | SpectateGame    |
| Unknown (no summary) | any              | NeedIdentity    |

## Handler Responsibilities

| Handler                | Trigger                     | Key Actions                                      |
|------------------------|-----------------------------|--------------------------------------------------|
| `handleUrlChange`      | Browser URL change          | Route to Loading / handleGameNavigation           |
| `handleGameNavigation` | Game link clicked from lobby| resolveGameNavigation, fire join/spectate          |
| `handleLobbyMsg`       | Lobby.Msg                   | Intercept ConfirmCreate, delegate rest to Lobby    |
| `handleLobbyCreate`    | ConfirmCreate               | Check identity, fire create or show EnterName      |
| `handleEnterNameMsg`   | EnterName.Msg               | Intercept RequestJoin, set identity, fire API       |
| `handleGameMsg`        | Game.Msg                    | Update game, fire moves/concede with session token |
| `handleServerMsg`      | Api.Msg                     | Dispatch to specific response handlers             |
| `handleGameSummaries`  | GET /games response         | Resolve Loading route, update LobbyPage            |
| `handleJoinResponse`   | POST /players response      | joinGameSuccess or show error                      |
| `handleNewGameResponse`| POST /games response        | joinGameSuccess or show error                      |
| `handleSpectateGame`   | GET /games/{id} response    | Build spectator game, open game stream             |
| `handleGameEvent`      | Game SSE event              | Apply move/concede/playerJoined to game state      |

## SSE Subscriptions

| Page      | Streams                                  |
|-----------|------------------------------------------|
| LobbyPage | lobbyEventReceived + Tick (60s for relative times) |
| GamePage  | gameEventReceived (unless GameOver)      |
| Others    | none                                     |

Stream lifecycle:
- Lobby stream opened by `toLobby`, closed when leaving lobby (create, join, spectate)
- Game stream opened by `joinGameSuccess` / `handleSpectateGame`, closed on navigate away

## Browser History

- `PendingCreate` (game creation): `Nav.pushUrl` so back button returns to lobby
- `PendingJoin` (clicking game link): no URL change needed (already navigated via link)
- Spectate: no extra URL change (already navigated via link)

## Error Recovery

- Join/create errors from `AwaitingGame` return to `EnterNamePage` with error message
- Move errors reset game state from `MoveDone` back to `Thinking`
- HTTP errors on initial load fall back to empty lobby
