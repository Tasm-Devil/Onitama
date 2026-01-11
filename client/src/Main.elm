module Main exposing (main)

import Api exposing (Msg(..), ServerGame)
import Browser
import Browser.Navigation as Nav exposing (Key)
import Game.Figure exposing (Color(..))
import Game.Game as Game exposing (Game, GameMove, GameState(..), Msg(..))
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Lobby exposing (GameId, Model, Msg(..), Status(..))
import Ports
import Url exposing (Url)



-- TYPES AND MODEL


type alias PlayerName =
    String


type alias SessionToken =
    String


type alias StoredSession =
    { gameId : GameId
    , playerName : String
    , token : SessionToken
    }


type Model
    = Redirect Key Url (List StoredSession)
    | Lobby Lobby.Model (List StoredSession)
    | EnterName Key GameId PlayerName (Maybe StoredSession) (List StoredSession)
    | Playing Key GameId PlayerName SessionToken Game (List Game.GameMove) (List StoredSession)
    | Rejoining Key GameId PlayerName SessionToken (List StoredSession)



-- MAIN


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( Redirect key url [], Cmd.map GotServerMsg Api.getGameSummariesFromServer )


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
            Redirect _ url _ ->
                Html.div [ HtmlA.class "lobby" ]
                    [ Html.h1 []
                        [ Html.text url.path ]
                    ]

            Lobby m _ ->
                Lobby.view m
                    |> Html.map GotLobbyMsg

            EnterName _ _ player _ _ ->
                Html.div [ HtmlA.class "landing-screen" ]
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

            Playing _ _ _ _ game history _ ->
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

            Rejoining _ _ playerName _ _ ->
                Html.div [ HtmlA.class "landing-screen" ]
                    [ Html.h2 [] [ Html.text "Rejoining..." ]
                    , Html.p [] [ Html.text ("Rejoining as " ++ playerName) ]
                    ]
        ]


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


-- Check if we lost and should concede
checkAndConcede : Game -> GameId -> SessionToken -> Cmd Msg
checkAndConcede game gameid token =
    case game.state of
        GameOver winner ->
            if winner /= game.myColor then
                -- We lost, send concede to server
                Cmd.map GotServerMsg <| Api.concede gameid token

            else
                -- We won, opponent should concede
                Cmd.none

        _ ->
            Cmd.none


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
    case msg of
        ChangedUrl url ->
            handleUrlChange url model

        ClickedLink urlRequest ->
            handleClickedLink urlRequest model

        GotGameMsg gamemsg ->
            handleGameMsg gamemsg model

        GotLobbyMsg lobbymsg ->
            handleLobbyMsg lobbymsg model

        TypingName newname ->
            handleTypingName newname model

        RequestGameFromServer ->
            handleRequestGame model

        SessionLoadedFromStorage value ->
            handleSessionLoaded value model

        GotServerMsg servermsg ->
            handleServerMsg servermsg model



-- URL CHANGE HANDLERS


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    let
        gameidStr =
            String.dropLeft 1 url.path
    in
    case model of
        Lobby lobby sessions ->
            if String.isEmpty gameidStr then
                ( model, Cmd.none )

            else
                case String.toInt gameidStr of
                    Just gameid ->
                        case List.filter (\s -> s.gameId == gameid) sessions |> List.head of
                            Just session ->
                                ( Rejoining lobby.key gameid session.playerName session.token sessions
                                , Cmd.map GotServerMsg <| Api.joinGame gameid session.playerName (Just session.token)
                                )

                            Nothing ->
                                ( EnterName lobby.key gameid "" Nothing sessions, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        EnterName key currentGameId name storedSession sessions ->
            if String.toInt gameidStr == Just currentGameId then
                ( EnterName key currentGameId name storedSession sessions, Cmd.none )

            else
                ( Redirect key url sessions, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        Playing key _ _ _ _ _ sessions ->
            ( Redirect key url sessions, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        _ ->
            ( model, Cmd.none )



-- LINK CLICK HANDLERS


handleClickedLink : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
handleClickedLink urlRequest model =
    case model of
        Lobby lobby sessions ->
            case urlRequest of
                Browser.Internal url ->
                    ( Lobby lobby sessions, Nav.pushUrl lobby.key <| Url.toString url )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- GAME MESSAGE HANDLERS


handleGameMsg : Game.Msg -> Model -> ( Model, Cmd Msg )
handleGameMsg gamemsg model =
    case model of
        Playing key gameid name token game history_ sessions ->
            let
                game_after =
                    Game.update gamemsg game

                cmd =
                    case game_after.state of
                        MoveDone gameMove ->
                            transformGameMove gameMove
                                |> Api.postNewGameMove gameid token
                                |> Cmd.map GotServerMsg

                        _ ->
                            Cmd.none
            in
            ( Playing key gameid name token game_after history_ sessions, cmd )

        _ ->
            ( model, Cmd.none )



-- LOBBY MESSAGE HANDLERS


handleLobbyMsg : Lobby.Msg -> Model -> ( Model, Cmd Msg )
handleLobbyMsg lobbymsg model =
    case ( lobbymsg, model ) of
        ( RequestNewGameFromServer, Lobby _ _ ) ->
            ( model, Cmd.map GotServerMsg Api.getGameIdFromServer )

        _ ->
            ( model, Cmd.none )



-- USER INPUT HANDLERS


handleTypingName : String -> Model -> ( Model, Cmd Msg )
handleTypingName newname model =
    case model of
        EnterName key gameid _ storedSession sessions ->
            ( EnterName key gameid newname storedSession sessions, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleRequestGame : Model -> ( Model, Cmd Msg )
handleRequestGame model =
    case model of
        EnterName _ gameid name _ sessions ->
            let
                maybeStoredSession =
                    findSession gameid name sessions

                maybeToken =
                    Maybe.map .token maybeStoredSession
            in
            ( model, Cmd.map GotServerMsg <| Api.joinGame gameid name maybeToken )

        Playing _ gameid _ _ _ _ _ ->
            ( model, Cmd.map GotServerMsg <| Api.getGameFromServer gameid )

        _ ->
            ( model, Cmd.none )



-- SESSION HANDLERS


handleSessionLoaded : Encode.Value -> Model -> ( Model, Cmd Msg )
handleSessionLoaded value model =
    case model of
        Redirect key url _ ->
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
                    case List.filter (\s -> s.gameId == gameId) loadedSessions |> List.head of
                        Just session ->
                            ( Rejoining key gameId session.playerName session.token loadedSessions
                            , Cmd.map GotServerMsg <| Api.joinGame gameId session.playerName (Just session.token)
                            )

                        Nothing ->
                            ( Redirect key url loadedSessions, Cmd.none )

                Nothing ->
                    ( Redirect key url loadedSessions, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SERVER MESSAGE HANDLERS


handleServerMsg : Api.Msg -> Model -> ( Model, Cmd Msg )
handleServerMsg servermsg model =
    case servermsg of
        ReceivedGameSummariesFromServer result ->
            handleGameSummaries result model

        ReceivedGameIdFromServer result ->
            handleNewGameId result model

        ReceivedJoinGameResponse result ->
            handleJoinResponse result model

        ReceivedGameFromServer result ->
            handleGameUpdate result model

        ReceivedPostCreatedFromServer result ->
            handleMoveConfirmation result model

        ReceivedConcedeResponse _ ->
            ( model, Cmd.none )


handleGameSummaries : Result Http.Error (List Lobby.GameSummary) -> Model -> ( Model, Cmd Msg )
handleGameSummaries result model =
    case ( result, model ) of
        ( Ok summaries, Redirect key url sessions ) ->
            let
                gameidStr =
                    String.dropLeft 1 url.path

                maybeGameId =
                    String.toInt gameidStr

                gameIds =
                    List.map .summaryId summaries
            in
            case maybeGameId of
                Just gameid ->
                    if List.member gameid gameIds then
                        ( EnterName key gameid "" Nothing sessions, Cmd.none )

                    else
                        ( Lobby { status = Home summaries, key = key } sessions, Nav.pushUrl key "/" )

                Nothing ->
                    if String.isEmpty gameidStr then
                        ( Lobby { status = Home summaries, key = key } sessions, Cmd.none )

                    else
                        ( Lobby { status = Home summaries, key = key } sessions, Nav.pushUrl key "/" )

        ( Err _, Redirect key _ sessions ) ->
            ( Lobby { status = Home [], key = key } sessions, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleNewGameId : Result Http.Error GameId -> Model -> ( Model, Cmd Msg )
handleNewGameId result model =
    case ( result, model ) of
        ( Ok gameId, Lobby lobby sessions ) ->
            ( Lobby lobby sessions, Nav.pushUrl lobby.key <| "/" ++ String.fromInt gameId )

        _ ->
            ( model, Cmd.none )


handleJoinResponse : Result Http.Error Api.JoinGameResponse -> Model -> ( Model, Cmd Msg )
handleJoinResponse result model =
    case ( result, model ) of
        ( Ok joinResponse, EnterName key gameid name _ sessions ) ->
            joinGameSuccess key gameid name joinResponse sessions True

        ( Err _, EnterName key gameid name storedSession sessions ) ->
            ( EnterName key gameid name storedSession sessions, Cmd.none )

        ( Ok joinResponse, Rejoining key gameId playerName _ sessions ) ->
            joinGameSuccess key gameId playerName joinResponse sessions False

        ( Err _, Rejoining key gameId playerName _ sessions ) ->
            ( EnterName key gameId playerName Nothing sessions, Cmd.none )

        ( Ok joinResponse, Redirect key url sessions ) ->
            let
                gameid =
                    String.dropLeft 1 url.path
                        |> String.toInt
                        |> Maybe.withDefault 0

                token =
                    joinResponse.responseToken

                playerName =
                    List.filter (\s -> s.gameId == gameid && s.token == token) sessions
                        |> List.head
                        |> Maybe.map .playerName
                        |> Maybe.withDefault "Unknown"
            in
            joinGameSuccess key gameid playerName joinResponse sessions False

        ( Err _, Redirect key url sessions ) ->
            let
                gameId =
                    String.dropLeft 1 url.path
                        |> String.toInt
                        |> Maybe.withDefault 0
            in
            ( EnterName key gameId "" Nothing sessions, Cmd.none )

        _ ->
            ( model, Cmd.none )


joinGameSuccess : Key -> GameId -> String -> Api.JoinGameResponse -> List StoredSession -> Bool -> ( Model, Cmd Msg )
joinGameSuccess key gameid name joinResponse sessions shouldSaveSession =
    let
        servergame =
            joinResponse.responseGame

        token =
            joinResponse.responseToken

        finalgame =
            buildGame name servergame

        newStoredSession =
            { gameId = gameid
            , playerName = name
            , token = token
            }

        updatedSessions =
            if shouldSaveSession && not (String.isEmpty name) then
                updateSessionStorage newStoredSession sessions

            else
                sessions

        saveCmd =
            if shouldSaveSession && not (String.isEmpty name) then
                Ports.saveSession (encodeSession newStoredSession)

            else
                Cmd.none

        concedeCmd =
            checkAndConcede finalgame gameid token
    in
    ( Playing key gameid name token finalgame servergame.history updatedSessions
    , Cmd.batch [ saveCmd, concedeCmd ]
    )


handleGameUpdate : Result Http.Error Api.ServerGame -> Model -> ( Model, Cmd Msg )
handleGameUpdate result model =
    case ( result, model ) of
        ( Ok servergame, Playing key gameid name token game _ sessions ) ->
            case game.state of
                GameOver _ ->
                    ( Playing key gameid name token game servergame.history sessions, Cmd.none )

                _ ->
                    List.head servergame.history
                        |> Maybe.map
                            (\gameMove ->
                                let
                                    updatedGame =
                                        game |> Game.update (NewGameMove <| transformGameMove gameMove)

                                    concedeCmd =
                                        checkAndConcede updatedGame gameid token
                                in
                                ( Playing key gameid name token updatedGame servergame.history sessions
                                , concedeCmd
                                )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleMoveConfirmation : Result Http.Error Game.GameMove -> Model -> ( Model, Cmd Msg )
handleMoveConfirmation result model =
    case ( result, model ) of
        ( Ok gameMove, Playing key gameid name token game history_ sessions ) ->
            let
                updatedGame =
                    game |> Game.update (NewGameMove <| transformGameMove gameMove)

                concedeCmd =
                    checkAndConcede updatedGame gameid token
            in
            ( Playing key gameid name token updatedGame (gameMove :: history_) sessions
            , concedeCmd
            )

        _ ->
            ( model, Cmd.none )
