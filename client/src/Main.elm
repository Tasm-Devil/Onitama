module Main exposing (main)

import Api exposing (Msg(..), ServerGame)
import Browser
import Browser.Navigation as Nav exposing (Key)
import Game.Figure exposing (Color(..))
import Game.Game as Game exposing (Game, GameMove, GameState(..), Msg(..))
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Lobby exposing (GameId, Model, Msg(..), Status(..))
import Ports
import Url exposing (Url)



-- TYPES


type alias PlayerName =
    String



-- MODEL


type alias SessionToken =
    String


type alias StoredSession =
    { gameId : GameId
    , playerName : String
    , token : SessionToken
    }


type Model
    = Redirect Url Key (List StoredSession)
    | Lobby Lobby.Model (List StoredSession)
    | EnterName PlayerName GameId Key (Maybe StoredSession) (List StoredSession)
    | Playing PlayerName GameId SessionToken Game (List Game.GameMove) Key (List StoredSession)
    | Rejoining GameId String SessionToken Key (List StoredSession) -- Rejoining state to track which game



-- MAIN


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( Redirect url key [], Cmd.map GotServerMsg Api.getGameSummariesFromServer )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.loadSession SessionLoadedFromStorage


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Onitama"
        [ case model of
            Redirect url _ _ ->
                Html.div [ HtmlA.class "lobby" ]
                    [ Html.h1 []
                        [ Html.text url.path ]
                    ]

            Lobby m _ ->
                Lobby.view m
                    |> Html.map GotLobbyMsg

            EnterName player _ _ _ _ ->
                Html.div [ HtmlA.class "landing-screen" ]
                    --, HtmlA.style "display" "none" ]
                    [ Html.form [ HtmlA.id "name-form" ]
                        [ Html.h1 []
                            [ Html.text "Onitama" ]
                        , Html.small [] [ Html.text "Enter your name..." ]
                        , Html.div [ HtmlA.class "name-line" ]
                            [ Html.input [ HtmlA.id "name", HtmlA.placeholder "Enter your name", HtmlA.value <| player, onInput TypingName ] []
                            , Html.input [ HtmlA.type_ "button", HtmlA.value "Join", onClick RequestGameFromServer ] []
                            ]
                        ]
                    ]

            Playing _ _ _ game history _ _ ->
                Html.div [ HtmlA.class "game-container", HtmlA.style "display" "flex" ]
                    ((game
                        |> Game.view
                        |> List.map (Html.map GotGameMsg)
                     )
                        ++ [ Html.div []
                                [ Html.input [ HtmlA.type_ "button", HtmlA.value "Update", onClick RequestGameFromServer ] []
                                , viewHistory history
                                ]
                           ]
                    )

            Rejoining _ playerName _ _ _ ->
                Html.div [ HtmlA.class "landing-screen" ]
                    [ Html.h2 [] [ Html.text "Rejoining..." ]
                    , Html.p [] [ Html.text ("Rejoining as " ++ playerName) ]
                    ]
        ]



{-

   viewHistoryOrError : Model -> Html Msg
   viewHistoryOrError model =
       --    case model.errorMessage of
       --        Just message ->
       --            viewError message
       --        Nothing ->
       case model of
           Playing _ _ _ history _ ->
               viewHistory history

           _ ->
               Html.text "Das sollten Sie nicht sehen"

      viewError : String -> Html Msg
      viewError errorMessage =
          let
              errorHeading =
                  "Couldn't fetch data at this time."
          in
          Html.div []
              [ Html.h3 [] [ Html.text errorHeading ]
              , Html.text ("Error: " ++ errorMessage)
              ]
-}


viewHistory : List GameMove -> Html Msg
viewHistory history =
    Html.div [ HtmlA.class "game-log" ]
        [ Html.ul [ HtmlA.id "log-lines" ]
            (List.map viewGameMove <| List.reverse history)
        , Html.input [ HtmlA.id "chat-box", HtmlA.type_ "text" ] []
        ]


viewGameMove : GameMove -> Html Msg
viewGameMove gameMove =
    let
        ( from_x, from_y ) =
            ( 1 + Tuple.first gameMove.from, 1 + Tuple.second gameMove.from )

        ( move_x, move_y ) =
            gameMove.move

        ( to_x, to_y ) =
            ( from_x + move_x, from_y + move_y )

        ( from_x_char, to_x_char ) =
            ( Char.fromCode (96 + from_x), Char.fromCode (96 + to_x) )

        from =
            String.fromChar from_x_char ++ String.fromInt from_y

        to =
            String.fromChar to_x_char ++ String.fromInt to_y
    in
    Html.li [ HtmlA.class "log-message" ]
        [ Html.text (Game.Figure.colorToString gameMove.color ++ " moved from " ++ from ++ " to " ++ to ++ " by playing the " ++ gameMove.card.name ++ " card.") ]



-- HELPERS
-- Add or update a session in the session list


updateSessionStorage : StoredSession -> List StoredSession -> List StoredSession
updateSessionStorage newSession sessions =
    let
        -- Remove any existing session for this game (by gameId and token)
        -- This prevents duplicates even if playerName changes
        filtered =
            List.filter
                (\s -> not (s.gameId == newSession.gameId && s.token == newSession.token))
                sessions

        -- Also remove any session with same gameId but empty playerName
        cleanedUp =
            List.filter
                (\s -> not (s.gameId == newSession.gameId && String.isEmpty s.playerName))
                filtered
    in
    newSession :: cleanedUp



-- Find a session for a specific game and player


findSession : GameId -> String -> List StoredSession -> Maybe StoredSession
findSession gameId playerName sessions =
    List.filter (\s -> s.gameId == gameId && s.playerName == playerName) sessions
        |> List.head



-- Encode a session for localStorage


encodeSession : StoredSession -> Encode.Value
encodeSession session =
    Encode.object
        [ ( "gameId", Encode.int session.gameId )
        , ( "playerName", Encode.string session.playerName )
        , ( "token", Encode.string session.token )
        ]



-- Decode sessions from localStorage


decodeSessions : Encode.Value -> List StoredSession
decodeSessions value =
    let
        sessionDecoder =
            Decode.map3 StoredSession
                (Decode.field "gameId" Decode.int)
                (Decode.field "playerName" Decode.string)
                (Decode.field "token" Decode.string)

        result =
            Decode.decodeValue (Decode.list sessionDecoder) value
    in
    case result of
        Ok sessions ->
            sessions

        Err _ ->
            []



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Game.Msg
    | GotLobbyMsg Lobby.Msg
    | TypingName String
    | RequestGameFromServer
    | SessionLoadedFromStorage Encode.Value
    | GotServerMsg Api.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, Lobby lobby sessions ) ->
            let
                gameidStr =
                    String.dropLeft 1 url.path
            in
            if String.isEmpty gameidStr then
                -- Stay in lobby if URL is just "/"
                ( model, Cmd.none )

            else
                case String.toInt gameidStr of
                    Just gameid ->
                        -- Check if we have a stored session for this game
                        case List.filter (\s -> s.gameId == gameid) sessions |> List.head of
                            Just session ->
                                -- We have a session! Auto-join
                                let
                                    _ =
                                        Debug.log "Found stored session, auto-joining" ( gameid, session.playerName, session.token )
                                in
                                ( Rejoining gameid session.playerName session.token lobby.key sessions
                                , Cmd.map GotServerMsg <| Api.joinGame gameid session.playerName (Just session.token)
                                )

                            Nothing ->
                                -- No session, go to name entry
                                ( EnterName "" gameid lobby.key Nothing sessions, Cmd.none )

                    Nothing ->
                        -- Invalid game ID, stay in lobby
                        ( model, Cmd.none )

        ( ChangedUrl url, EnterName name currentGameId key storedSession sessions ) ->
            let
                newGameIdStr =
                    String.dropLeft 1 url.path

                newGameId =
                    String.toInt newGameIdStr
            in
            if newGameId == Just currentGameId then
                -- Stay in EnterName state if the URL matches the current game
                ( EnterName name currentGameId key storedSession sessions, Cmd.none )

            else
                -- Different game, redirect and fetch summaries
                ( Redirect url key sessions, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        ( ChangedUrl url, Playing _ _ _ _ _ key sessions ) ->
            ( Redirect url key sessions, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        ( ClickedLink urlRequest, Lobby lobby sessions ) ->
            -- user clicked on a Join link in the table
            case urlRequest of
                Browser.Internal url ->
                    ( Lobby lobby sessions, Nav.pushUrl lobby.key <| Url.toString url )

                _ ->
                    ( model, Cmd.none )

        ( GotGameMsg gamemsg, Playing name gameid token game history_ key sessions ) ->
            -- game signals a new gamemove to this update function by MoveDone
            -- let the server know by invoking postNewGameMove
            let
                game_after =
                    Game.update gamemsg game
            in
            ( Playing name gameid token game_after history_ key sessions
            , case game_after.state of
                MoveDone gameMove ->
                    transformGameMove gameMove
                        |> Api.postNewGameMove gameid token
                        |> Cmd.map GotServerMsg

                _ ->
                    Cmd.none
            )

        ( GotLobbyMsg RequestNewGameFromServer, Lobby _ _ ) ->
            -- User clicked on 'New Game' button
            ( model, Cmd.map GotServerMsg Api.getGameIdFromServer )

        ( TypingName newname, EnterName _ gameid key storedSession sessions ) ->
            ( EnterName newname gameid key storedSession sessions, Cmd.none )

        ( RequestGameFromServer, EnterName name gameid _ _ sessions ) ->
            -- User completed entering name and clicked the join button
            let
                -- Look for a session for this game and player
                maybeStoredSession =
                    findSession gameid name sessions

                maybeToken =
                    Maybe.map .token maybeStoredSession

                _ =
                    Debug.log "Joining with token" ( name, gameid, maybeToken )
            in
            ( model, Cmd.map GotServerMsg <| Api.joinGame gameid name maybeToken )

        ( RequestGameFromServer, Playing _ gameid _ _ _ _ _ ) ->
            -- fetch latest gamemove from server
            ( model, Cmd.map GotServerMsg <| Api.getGameFromServer gameid )

        ( GotServerMsg (ReceivedGameSummariesFromServer (Ok summaries)), Redirect url key sessions ) ->
            -- all the game summaries successfully fetched from server
            let
                gameidStr =
                    String.dropLeft 1 url.path

                maybeGameId =
                    String.toInt gameidStr

                gamesids =
                    List.map .summaryId summaries
            in
            case maybeGameId of
                Just gameid ->
                    if List.member gameid gamesids then
                        -- enter a game directly by URL
                        ( EnterName "" gameid key Nothing sessions, Cmd.none )

                    else
                        -- The URL points to an nonexisting gameid
                        ( Lobby { status = Home summaries, key = key } sessions, Nav.pushUrl key <| "/" )

                Nothing ->
                    if String.isEmpty gameidStr then
                        -- the default way to enter the lobby
                        ( Lobby { status = Home summaries, key = key } sessions, Cmd.none )

                    else
                        -- Invalid game ID format
                        ( Lobby { status = Home summaries, key = key } sessions, Nav.pushUrl key <| "/" )

        ( GotServerMsg (ReceivedGameSummariesFromServer (Err _)), Redirect _ key sessions ) ->
            -- If fetching summaries fails, show an empty lobby
            ( Lobby { status = Home [], key = key } sessions, Cmd.none )

        ( GotServerMsg (ReceivedGameIdFromServer (Ok gameId)), Lobby lobby sessions ) ->
            -- getGameIdFromServer was succesfull. Created a new game on the server.
            -- Refresh the game list from server to get updated summaries
            ( Lobby lobby sessions, Nav.pushUrl lobby.key <| "/" ++ String.fromInt gameId )

        ( GotServerMsg (ReceivedJoinGameResponse (Ok joinResponse)), EnterName name gameid key _ sessions ) ->
            -- joinGame was succesfull. Now create a new game in Browser
            let
                servergame =
                    joinResponse.responseGame

                token =
                    joinResponse.responseToken

                _ =
                    Debug.log "Successfully joined game with token" ( name, gameid, token )

                finalgame =
                    buildGame name servergame

                -- Create stored session for this player
                newStoredSession =
                    { gameId = gameid
                    , playerName = name
                    , token = token
                    }

                -- Add to session storage (only if playerName is not empty)
                updatedSessions =
                    if String.isEmpty name then
                        sessions

                    else
                        updateSessionStorage newStoredSession sessions

                saveCmd =
                    if String.isEmpty name then
                        Cmd.none

                    else
                        Ports.saveSession (encodeSession newStoredSession)
            in
            ( Playing name gameid token finalgame servergame.history key updatedSessions
            , saveCmd
            )

        ( GotServerMsg (ReceivedJoinGameResponse (Ok joinResponse)), Rejoining gameId playerName _ key sessions ) ->
            -- Auto-rejoin succeeded from Rejoining state
            let
                servergame =
                    joinResponse.responseGame

                token =
                    joinResponse.responseToken

                _ =
                    Debug.log "Auto-rejoin successful" ( playerName, gameId, token )

                finalgame =
                    buildGame playerName servergame
            in
            ( Playing playerName gameId token finalgame servergame.history key sessions, Cmd.none )

        ( GotServerMsg (ReceivedJoinGameResponse (Err httpError)), Rejoining gameId playerName _ key sessions ) ->
            -- Auto-rejoin failed, show EnterName
            let
                _ =
                    Debug.log "Auto-rejoin failed, showing name entry" httpError
            in
            ( EnterName playerName gameId key Nothing sessions, Cmd.none )

        ( GotServerMsg (ReceivedJoinGameResponse (Ok joinResponse)), Redirect url key sessions ) ->
            -- Auto-rejoin succeeded from Redirect state
            let
                servergame =
                    joinResponse.responseGame

                token =
                    joinResponse.responseToken

                gameid =
                    String.dropLeft 1 url.path
                        |> String.toInt
                        |> Maybe.withDefault 0

                -- Figure out the player name from the game state
                playerName =
                    -- Find which player we are by checking sessions
                    List.filter (\s -> s.gameId == gameid && s.token == token) sessions
                        |> List.head
                        |> Maybe.map .playerName
                        |> Maybe.withDefault "Unknown"

                _ =
                    Debug.log "Auto-rejoin successful" ( playerName, gameid, token )

                finalgame =
                    buildGame playerName servergame
            in
            ( Playing playerName gameid token finalgame servergame.history key sessions, Cmd.none )

        ( GotServerMsg (ReceivedJoinGameResponse (Err httpError)), Redirect url key sessions ) ->
            -- Auto-rejoin failed from Redirect state, show EnterName
            let
                gameidStr =
                    String.dropLeft 1 url.path

                gameId =
                    String.toInt gameidStr |> Maybe.withDefault 0

                _ =
                    Debug.log "Auto-rejoin failed, showing name entry" httpError
            in
            ( EnterName "" gameId key Nothing sessions, Cmd.none )

        ( GotServerMsg (ReceivedJoinGameResponse (Err httpError)), EnterName name gameid key storedSession sessions ) ->
            -- joinGame failed
            let
                _ =
                    Debug.log "Failed to join game" httpError
            in
            -- Stay in EnterName state so user can try again
            ( EnterName name gameid key storedSession sessions, Cmd.none )

        ( GotServerMsg (ReceivedGameFromServer (Ok servergame)), Playing name gameid token game _ key sessions ) ->
            -- getGameFromServer was succesfull. Append the last gamemove from opponent to the history.
            List.head servergame.history
                |> Maybe.map
                    (\gameMove ->
                        ( Playing name gameid token (game |> Game.update (NewGameMove <| transformGameMove gameMove)) servergame.history key sessions
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ( GotServerMsg (ReceivedPostCreatedFromServer (Ok gameMove)), Playing name gameid token game history_ key sessions ) ->
            ( Playing name gameid token (game |> Game.update (NewGameMove <| transformGameMove gameMove)) (gameMove :: history_) key sessions
            , Cmd.none
            )

        ( SessionLoadedFromStorage value, Redirect url key _ ) ->
            -- Sessions loaded from localStorage on startup
            let
                loadedSessions =
                    decodeSessions value

                gameidStr =
                    String.dropLeft 1 url.path

                maybeGameId =
                    String.toInt gameidStr
            in
            case maybeGameId of
                Just gameId ->
                    -- Check if we have a session for this game
                    case List.filter (\s -> s.gameId == gameId) loadedSessions |> List.head of
                        Just session ->
                            -- We have a session! Auto-rejoin
                            let
                                _ =
                                    Debug.log "Auto-rejoining with stored session" session.playerName
                            in
                            ( Redirect url key loadedSessions
                            , Cmd.map GotServerMsg <| Api.joinGame gameId session.playerName (Just session.token)
                            )

                        Nothing ->
                            -- No session for this game, proceed normally
                            ( Redirect url key loadedSessions, Cmd.none )

                Nothing ->
                    -- Not navigating to a game, just load summaries
                    ( Redirect url key loadedSessions, Cmd.none )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


transformGameMove : Game.GameMove -> Game.GameMove
transformGameMove g =
    case g.color of
        Black ->
            Game.rotateGameMove g

        White ->
            g


buildGame : String -> ServerGame -> Game
buildGame name servergame =
    let
        newgame =
            Game.setupNewGame servergame.cards
                (if name == servergame.player_black then
                    Black

                 else
                    White
                )
                White

        -- ToDo: White is not always the first player!
    in
    List.foldr (\gameMove -> Game.update (NewGameMove <| transformGameMove gameMove)) newgame servergame.history



{-
   buildErrorMessage : Http.Error -> String
   buildErrorMessage httpError =
       case httpError of
           Http.BadUrl message ->
               message

           Http.Timeout ->
               "Server is taking too long to respond. Please try again later."

           Http.NetworkError ->
               "Unable to reach server."

           Http.BadStatus statusCode ->
               "Request failed with status code: " ++ String.fromInt statusCode

           Http.BadBody message ->
               message
-}
