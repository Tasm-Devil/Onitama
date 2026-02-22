module Main exposing (main)

import Api exposing (GameEvent(..), GameId, Msg(..), ServerGame)
import Browser
import Browser.Navigation as Nav exposing (Key)
import EnterName
import Game.Card exposing (dummyCard)
import Game.Figure exposing (Color(..))
import Game.Game as Game exposing (Game, GameState(..), Msg(..), transformGameMove)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Lobby
import Ports
import Task
import Time
import Url exposing (Url)



-- TYPES


type alias PlayerName =
    String


type alias PlayerToken =
    String


type alias PlayerSession =
    { name : PlayerName
    , token : PlayerToken
    }


type alias Model =
    { key : Key
    , storedPlayers : List PlayerSession
    , currentPlayer : Maybe PlayerSession
    , page : Page
    }


type PendingAction
    = PendingCreate Bool Lobby.CardSet
    | PendingJoin GameId


type Route
    = LobbyRoute
    | GameRoute GameId


type Page
    = Loading Route
    | LobbyPage Lobby.Model
    | EnterNamePage PendingAction EnterName.Model
    | AwaitingGame PendingAction String
    | GamePage GameId Game (List Game.LogEntry)


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Game.Msg
    | GotEnterNameMsg EnterName.Msg
    | GotLobbyMsg Lobby.Msg
    | StoredPlayersLoaded Encode.Value
    | GotServerMsg Api.Msg
    | LobbyEventReceived Encode.Value
    | GameEventReceived Encode.Value
    | LobbyTick Time.Posix


type NavigationAction
    = JoinDirectly PlayerSession
    | NeedIdentity
    | SpectateGame



-- MAIN


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


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, storedPlayers = [], currentPlayer = Nothing, page = Loading (routeFromUrl url) }
    , Cmd.map GotServerMsg Api.getGameSummariesFromServer
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        playersSub =
            Ports.loadPlayers StoredPlayersLoaded

        sseSub =
            case model.page of
                GamePage _ game _ ->
                    case game.state of
                        GameOver _ ->
                            Sub.none

                        _ ->
                            Ports.gameEventReceived GameEventReceived

                LobbyPage _ ->
                    Ports.lobbyEventReceived LobbyEventReceived

                _ ->
                    Sub.none
    in
    Sub.batch [ playersSub, sseSub ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            handleUrlChange url model

        ClickedLink urlRequest ->
            handleClickedLink urlRequest model

        GotGameMsg gamemsg ->
            handleGameMsg gamemsg model

        GotEnterNameMsg enterNameMsg ->
            handleEnterNameMsg enterNameMsg model

        GotLobbyMsg lobbyMsg ->
            handleLobbyMsg lobbyMsg model

        StoredPlayersLoaded value ->
            ( { model | storedPlayers = decodePlayers value }, Cmd.none )

        GotServerMsg servermsg ->
            handleServerMsg servermsg model

        LobbyEventReceived value ->
            handleLobbyEvent value model

        GameEventReceived value ->
            handleGameEvent value model

        LobbyTick currentTime ->
            case model.page of
                LobbyPage lobbyModel ->
                    ( { model | page = LobbyPage (Lobby.updateTime currentTime lobbyModel) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- NAVIGATION HANDLERS


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    let
        route =
            routeFromUrl url
    in
    case model.page of
        LobbyPage lobbyModel ->
            case route of
                GameRoute gameid ->
                    handleGameNavigation model lobbyModel gameid

                LobbyRoute ->
                    ( model, Cmd.none )

        EnterNamePage (PendingJoin currentGameId) _ ->
            if route == GameRoute currentGameId then
                ( model, Cmd.none )

            else
                ( { model | page = Loading route }, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        EnterNamePage _ _ ->
            ( { model | page = Loading route }, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        GamePage currentGameId _ _ ->
            if route == GameRoute currentGameId then
                ( model, Cmd.none )

            else
                ( { model | page = Loading route }
                , Cmd.batch
                    [ Ports.closeGameStream ()
                    , Cmd.map GotServerMsg Api.getGameSummariesFromServer
                    ]
                )

        AwaitingGame _ _ ->
            ( { model | page = Loading route }, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        Loading _ ->
            ( { model | page = Loading route }, Cmd.map GotServerMsg Api.getGameSummariesFromServer )


handleClickedLink : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
handleClickedLink urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        _ ->
            ( model, Cmd.none )


handleGameNavigation : Model -> Lobby.Model -> GameId -> ( Model, Cmd Msg )
handleGameNavigation model lobbyModel gameid =
    case resolveGameNavigation model.currentPlayer (Lobby.getGames lobbyModel) gameid of
        JoinDirectly session ->
            ( { model | currentPlayer = Just session, page = AwaitingGame (PendingJoin gameid) session.name }
            , Cmd.batch
                [ Ports.closeLobbyStream ()
                , Cmd.map GotServerMsg (Api.joinGame gameid session.name (Just session.token))
                ]
            )

        NeedIdentity ->
            ( { model | page = EnterNamePage (PendingJoin gameid) (EnterName.Entering "") }
            , Ports.closeLobbyStream ()
            )

        SpectateGame ->
            ( { model | page = Loading (GameRoute gameid) }
            , Cmd.batch [ Ports.closeLobbyStream (), Cmd.map GotServerMsg (Api.getGameFromServer gameid) ]
            )


resolveGameNavigation : Maybe PlayerSession -> List Lobby.GameSummary -> GameId -> NavigationAction
resolveGameNavigation currentPlayer summaries gameid =
    let
        summary =
            List.filter (\s -> s.summaryId == gameid) summaries |> List.head

        status =
            Maybe.map .summaryStatus summary

        matchesGamePlayer session =
            case summary of
                Just s ->
                    session.name == s.summaryPlayer1 || session.name == s.summaryPlayer2

                Nothing ->
                    False
    in
    case status of
        Just Lobby.Completed ->
            SpectateGame

        Just Lobby.WaitingForPlayers ->
            case currentPlayer of
                Just session ->
                    JoinDirectly session

                Nothing ->
                    NeedIdentity

        Just Lobby.InProgress ->
            case currentPlayer of
                Just session ->
                    if matchesGamePlayer session then
                        JoinDirectly session

                    else
                        SpectateGame

                Nothing ->
                    SpectateGame

        Nothing ->
            NeedIdentity


resolveGameRoute : Model -> List Lobby.GameSummary -> GameId -> ( Model, Cmd Msg )
resolveGameRoute model summaries gameid =
    case resolveGameNavigation model.currentPlayer summaries gameid of
        JoinDirectly session ->
            ( { model | currentPlayer = Just session, page = AwaitingGame (PendingJoin gameid) session.name }
            , Cmd.map GotServerMsg (Api.joinGame gameid session.name (Just session.token))
            )

        NeedIdentity ->
            ( { model | page = EnterNamePage (PendingJoin gameid) (EnterName.Entering "") }
            , Cmd.none
            )

        SpectateGame ->
            ( model, Cmd.map GotServerMsg (Api.getGameFromServer gameid) )



-- LOBBY HANDLERS


handleLobbyEvent : Encode.Value -> Model -> ( Model, Cmd Msg )
handleLobbyEvent value model =
    let
        eventType =
            Decode.decodeValue (Decode.field "event" Decode.string) value
                |> Result.withDefault ""
    in
    case ( eventType, model.page ) of
        ( "lobbyChanged", LobbyPage _ ) ->
            ( model, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        ( "tick", LobbyPage _ ) ->
            ( model, Task.perform LobbyTick Time.now )

        _ ->
            ( model, Cmd.none )


handleLobbyMsg : Lobby.Msg -> Model -> ( Model, Cmd Msg )
handleLobbyMsg msg model =
    case model.page of
        LobbyPage lobbyModel ->
            case msg of
                Lobby.ConfirmCreate ->
                    handleLobbyCreate model lobbyModel

                Lobby.ClickPlay gameid ->
                    ( { model | page = EnterNamePage (PendingJoin gameid) (EnterName.Entering "") }
                    , Cmd.batch
                        [ Ports.closeLobbyStream ()
                        , Nav.pushUrl model.key ("/" ++ String.fromInt gameid)
                        ]
                    )

                _ ->
                    ( { model | page = LobbyPage (Lobby.update msg lobbyModel) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleLobbyCreate : Model -> Lobby.Model -> ( Model, Cmd Msg )
handleLobbyCreate model lobbyModel =
    case ( Lobby.getGameType lobbyModel, Lobby.getCardSet lobbyModel ) of
        ( Just gameType, Just cardSet ) ->
            let
                vsAI =
                    gameType == Lobby.VsAI

                action =
                    PendingCreate vsAI cardSet
            in
            case model.currentPlayer of
                Just session ->
                    ( { model | page = AwaitingGame action session.name }
                    , Cmd.batch
                        [ Ports.closeLobbyStream ()
                        , Cmd.map GotServerMsg (Api.createGame session.name vsAI cardSet (Just session.token))
                        ]
                    )

                Nothing ->
                    ( { model | page = EnterNamePage action (EnterName.Entering "") }
                    , Ports.closeLobbyStream ()
                    )

        _ ->
            ( model, Cmd.none )



-- ENTER NAME HANDLERS


handleEnterNameMsg : EnterName.Msg -> Model -> ( Model, Cmd Msg )
handleEnterNameMsg enterNameMsg model =
    case ( enterNameMsg, model.page ) of
        ( EnterName.RequestJoin, EnterNamePage pendingAction enterNameModel ) ->
            handleIdentitySubmit model pendingAction enterNameModel

        ( EnterName.Cancel, EnterNamePage _ _ ) ->
            ( { model | page = Loading LobbyRoute }
            , Cmd.batch
                [ Nav.pushUrl model.key "/"
                , Cmd.map GotServerMsg Api.getGameSummariesFromServer
                ]
            )

        ( _, EnterNamePage action enterNameModel ) ->
            ( { model | page = EnterNamePage action (EnterName.update enterNameMsg enterNameModel) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleIdentitySubmit : Model -> PendingAction -> EnterName.Model -> ( Model, Cmd Msg )
handleIdentitySubmit model pendingAction enterNameModel =
    case EnterName.getName enterNameModel of
        Just name ->
            if String.isEmpty (String.trim name) then
                ( model, Cmd.none )

            else
                let
                    maybeToken =
                        findTokenForName name model.storedPlayers

                    session =
                        { name = name, token = Maybe.withDefault "" maybeToken }
                in
                ( { model | currentPlayer = Just session, page = AwaitingGame pendingAction name }
                , Cmd.map GotServerMsg (fireCreateOrJoin pendingAction name maybeToken)
                )

        Nothing ->
            ( model, Cmd.none )



-- GAME HANDLERS


handleGameMsg : Game.Msg -> Model -> ( Model, Cmd Msg )
handleGameMsg gamemsg model =
    case model.page of
        GamePage gameid game log ->
            case ( gamemsg, model.currentPlayer ) of
                ( Game.UserClickedConcede, Just session ) ->
                    ( model, Cmd.map GotServerMsg (Api.concede gameid session.token) )

                _ ->
                    let
                        updatedGame =
                            Game.update gamemsg game

                        cmd =
                            case ( updatedGame.state, model.currentPlayer ) of
                                ( MoveDone gameMove, Just session ) ->
                                    transformGameMove gameMove
                                        |> Api.postNewGameMove gameid session.token
                                        |> Cmd.map GotServerMsg

                                _ ->
                                    Cmd.none
                    in
                    ( { model | page = GamePage gameid updatedGame log }, cmd )

        _ ->
            ( model, Cmd.none )



-- SERVER RESPONSE HANDLERS


handleServerMsg : Api.Msg -> Model -> ( Model, Cmd Msg )
handleServerMsg servermsg model =
    case servermsg of
        ReceivedGameSummariesFromServer result ->
            handleGameSummaries result model

        ReceivedJoinGameResponse result ->
            handleJoinResponse result model

        ReceivedPostCreatedFromServer result ->
            handleMoveConfirmation result model

        ReceivedConcedeResponse _ ->
            ( model, Cmd.none )

        ReceivedGameFromServer result ->
            handleSpectateGame result model

        ReceivedNewGameResponse result ->
            handleNewGameResponse result model


handleGameSummaries : Result Http.Error (List Lobby.GameSummary) -> Model -> ( Model, Cmd Msg )
handleGameSummaries result model =
    case ( result, model.page ) of
        ( Ok summaries, Loading route ) ->
            case route of
                GameRoute gameid ->
                    if List.any (\s -> s.summaryId == gameid) summaries then
                        resolveGameRoute model summaries gameid

                    else
                        toLobby model summaries True

                LobbyRoute ->
                    toLobby model summaries False

        ( Err _, Loading _ ) ->
            toLobby model [] False

        ( Ok summaries, LobbyPage lobbyModel ) ->
            ( { model | page = LobbyPage (Lobby.updateGames summaries lobbyModel) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleJoinResponse : Result Http.Error (Result Api.JoinError Api.JoinGameResponse) -> Model -> ( Model, Cmd Msg )
handleJoinResponse result model =
    case ( result, model.page ) of
        ( Ok (Ok joinResponse), AwaitingGame (PendingJoin gameid) _ ) ->
            joinGameSuccess model gameid joinResponse

        ( Ok (Err joinError), AwaitingGame action playerName ) ->
            ( { model | page = EnterNamePage action (EnterName.JoinError playerName (Api.joinErrorToString joinError)) }
            , Cmd.none
            )

        ( Err httpError, AwaitingGame action playerName ) ->
            ( { model | page = EnterNamePage action (EnterName.JoinError playerName (httpErrorToString httpError)) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


joinGameSuccess : Model -> GameId -> Api.JoinGameResponse -> ( Model, Cmd Msg )
joinGameSuccess model gameid joinResponse =
    let
        servergame =
            joinResponse.responseGame

        name =
            joinResponse.responsePlayerName

        session =
            { name = name, token = joinResponse.responseToken }

        finalgame =
            buildGame name servergame

        isBlack =
            name == servergame.gameBlackName

        myColor =
            if isBlack then
                "black"

            else
                "white"

        opponentName =
            if isBlack then
                servergame.gameWhiteName

            else
                servergame.gameBlackName

        opponentColor =
            if isBlack then
                "white"

            else
                "black"

        bothPresent =
            not (String.isEmpty opponentName)

        moveEntries =
            List.map Game.MoveEntry servergame.gameHistory

        joinMsg =
            Game.SystemEntry ("You joined game as " ++ myColor ++ ".")

        initialLog =
            if bothPresent then
                moveEntries
                    ++ [ Game.SystemEntry "Both players are present, the game begins."
                       , Game.SystemEntry (opponentName ++ " has joined the game as " ++ opponentColor ++ ".")
                       , joinMsg
                       ]

            else
                moveEntries ++ [ joinMsg ]

        gameWithState =
            if bothPresent then
                finalgame

            else
                { finalgame | state = WaitingForOpponent }

        saveCmd =
            if not (String.isEmpty name) then
                Ports.savePlayer (encodePlayer session)

            else
                Cmd.none

        urlCmd =
            case model.page of
                AwaitingGame (PendingCreate _ _) _ ->
                    Nav.pushUrl model.key ("/" ++ String.fromInt gameid)

                _ ->
                    Nav.replaceUrl model.key ("/" ++ String.fromInt gameid)
    in
    ( { model | currentPlayer = Just session, page = GamePage gameid gameWithState initialLog }
    , Cmd.batch
        [ saveCmd
        , Ports.closeLobbyStream ()
        , Ports.openGameStream gameid
        , urlCmd
        ]
    )


handleNewGameResponse : Result Http.Error (Result Api.JoinError Api.NewGameResponse) -> Model -> ( Model, Cmd Msg )
handleNewGameResponse result model =
    case ( result, model.page ) of
        ( Ok (Ok response), AwaitingGame _ _ ) ->
            joinGameSuccess model response.newGameId response.newGameJoinResponse

        ( Ok (Err joinError), AwaitingGame action playerName ) ->
            ( { model | page = EnterNamePage action (EnterName.JoinError playerName (Api.joinErrorToString joinError)) }
            , Cmd.none
            )

        ( Err httpError, AwaitingGame action playerName ) ->
            ( { model | page = EnterNamePage action (EnterName.JoinError playerName (httpErrorToString httpError)) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleMoveConfirmation : Result Http.Error (Result Api.MoveError Game.GameMove) -> Model -> ( Model, Cmd Msg )
handleMoveConfirmation result model =
    case ( result, model.page ) of
        ( Ok (Ok _), _ ) ->
            ( model, Cmd.none )

        ( _, GamePage gameid game log ) ->
            case game.state of
                MoveDone _ ->
                    ( { model | page = GamePage gameid { game | state = Thinking } log }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleSpectateGame : Result Http.Error ServerGame -> Model -> ( Model, Cmd Msg )
handleSpectateGame result model =
    case ( result, model.page ) of
        ( Ok servergame, Loading (GameRoute gameid) ) ->
            ( { model | page = GamePage gameid (buildSpectatorGame servergame) (buildSpectatorLog servergame) }
            , Ports.openGameStream gameid
            )

        _ ->
            ( model, Cmd.none )



-- SSE EVENT HANDLERS


handleGameEvent : Encode.Value -> Model -> ( Model, Cmd Msg )
handleGameEvent value model =
    case Decode.decodeValue Api.decodeGameEvent value of
        Ok event ->
            case model.page of
                GamePage gameid game log ->
                    case game.state of
                        GameOver _ ->
                            ( model, Cmd.none )

                        _ ->
                            applyGameEvent gameid game log event model

                _ ->
                    ( model, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


applyGameEvent : GameId -> Game -> List Game.LogEntry -> Api.GameEvent -> Model -> ( Model, Cmd Msg )
applyGameEvent gameid game log event model =
    case event of
        Api.MoveEvent moveStr _ maybeWinner ->
            case Api.stringToGameMove moveStr of
                Just gameMove ->
                    let
                        movedGame =
                            game |> Game.update (NewGameMove <| transformGameMove gameMove)

                        updatedGame =
                            case maybeWinner of
                                Just winnerStr ->
                                    { movedGame | state = GameOver (parseWinnerColor winnerStr) }

                                Nothing ->
                                    movedGame
                    in
                    ( { model | page = GamePage gameid updatedGame (Game.MoveEntry gameMove :: log) }
                    , Ports.playSound "move"
                    )

                Nothing ->
                    ( model, Cmd.none )

        Api.ConcedeEvent winnerStr ->
            ( { model | page = GamePage gameid { game | state = GameOver (parseWinnerColor winnerStr) } log }
            , Cmd.none
            )

        Api.PlayerJoinedEvent joinedName joinedColor ->
            let
                joinedMsg =
                    Game.SystemEntry (joinedName ++ " has joined the game as " ++ String.toLower joinedColor ++ ".")

                startMsg =
                    Game.SystemEntry "Both players are present, the game begins."
            in
            ( { model | page = GamePage gameid { game | state = Thinking } (startMsg :: joinedMsg :: log) }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Onitama"
        [ Html.div [ HtmlA.class "page-wrapper" ]
            [ case model.page of
                Loading _ ->
                    Html.div [ HtmlA.class "landing-screen" ]
                        [ Html.div [ HtmlA.class "spinner" ] [] ]

                LobbyPage lobbyModel ->
                    let
                        playerNames =
                            case model.currentPlayer of
                                Just s ->
                                    [ s.name ]

                                Nothing ->
                                    []
                    in
                    Html.div [ HtmlA.class "lobby" ]
                        [ Lobby.view playerNames lobbyModel |> Html.map GotLobbyMsg ]

                EnterNamePage _ enterNameModel ->
                    Html.div [ HtmlA.class "landing-screen" ]
                        (EnterName.view enterNameModel (List.map .name model.storedPlayers)
                            |> List.map (Html.map GotEnterNameMsg)
                        )

                AwaitingGame _ playerName ->
                    Html.div [ HtmlA.class "landing-screen" ]
                        [ Html.h2 [] [ Html.text "Joining game..." ]
                        , Html.p [] [ Html.text ("Joining as " ++ playerName) ]
                        , Html.div [ HtmlA.class "spinner" ] []
                        ]

                GamePage _ game log ->
                    Html.div [ HtmlA.class "game-container" ]
                        ((game
                            |> Game.view
                            |> List.map (Html.map GotGameMsg)
                         )
                            ++ [ Game.viewLog log |> Html.map GotGameMsg ]
                        )
            , viewFooter
            ]
        ]


viewFooter : Html Msg
viewFooter =
    Html.footer [ HtmlA.class "game-footer" ]
        [ Html.a
            [ HtmlA.href "https://pegasus.de/Onitama/51855G"
            , HtmlA.target "_blank"
            , HtmlA.rel "noopener noreferrer"
            , HtmlA.class "game-title"
            ]
            [ Html.text "Onitama" ]
        , Html.span [ HtmlA.class "separator" ] [ Html.text "•" ]
        , Html.text "Made with "
        , Html.a
            [ HtmlA.href "https://elm-lang.org"
            , HtmlA.target "_blank"
            , HtmlA.rel "noopener noreferrer"
            ]
            [ Html.i [ HtmlA.class "nf nf-dev-elm tech-icon" ] [] ]
        , Html.text " & "
        , Html.a
            [ HtmlA.href "https://www.haskell.org"
            , HtmlA.target "_blank"
            , HtmlA.rel "noopener noreferrer"
            ]
            [ Html.i [ HtmlA.class "nf nf-dev-haskell tech-icon" ] [] ]
        , Html.span [ HtmlA.class "separator" ] [ Html.text "•" ]
        , Html.text "View source on "
        , Html.a
            [ HtmlA.href "https://github.com/Tasm-Devil/Onitama"
            , HtmlA.target "_blank"
            , HtmlA.rel "noopener noreferrer"
            ]
            [ Html.i [ HtmlA.class "nf nf-dev-github tech-icon" ] [] ]
        ]



-- HELPERS


routeFromUrl : Url -> Route
routeFromUrl url =
    case String.toInt (String.dropLeft 1 url.path) of
        Just gameid ->
            GameRoute gameid

        Nothing ->
            LobbyRoute


encodePlayer : PlayerSession -> Encode.Value
encodePlayer player =
    Encode.object
        [ ( "playerName", Encode.string player.name )
        , ( "token", Encode.string player.token )
        ]


decodePlayers : Encode.Value -> List PlayerSession
decodePlayers value =
    let
        playerDecoder =
            Decode.map2 PlayerSession
                (Decode.field "playerName" Decode.string)
                (Decode.field "token" Decode.string)
    in
    Decode.decodeValue (Decode.list playerDecoder) value
        |> Result.withDefault []


parseWinnerColor : String -> Color
parseWinnerColor str =
    if str == "White" then
        White

    else
        Black


findTokenForName : String -> List PlayerSession -> Maybe String
findTokenForName name players =
    players
        |> List.filter (\p -> p.name == name)
        |> List.head
        |> Maybe.map .token


buildGame : String -> ServerGame -> Game
buildGame name servergame =
    let
        commonCard =
            Maybe.withDefault dummyCard (List.head <| List.drop 4 <| servergame.gameCards)

        newgame =
            Game.setupNewGame servergame.gameCards
                (if name == servergame.gameBlackName then
                    Black

                 else
                    White
                )
                commonCard.startPlayer
    in
    List.foldr (\gameMove -> Game.update (NewGameMove <| transformGameMove gameMove)) newgame servergame.gameHistory


buildSpectatorGame : ServerGame -> Game
buildSpectatorGame servergame =
    let
        game =
            buildGame servergame.gameWhiteName servergame

        gameWithWinner =
            case servergame.gameWinner of
                Just winnerColor ->
                    { game | state = GameOver winnerColor }

                Nothing ->
                    game
    in
    { gameWithWinner | spectating = True }


buildSpectatorLog : ServerGame -> List Game.LogEntry
buildSpectatorLog servergame =
    let
        whiteName =
            servergame.gameWhiteName

        blackName =
            servergame.gameBlackName

        playerMsgs name color =
            if String.isEmpty name then
                []

            else
                [ Game.SystemEntry (name ++ " has joined the game as " ++ color ++ ".") ]

        startMsg =
            if not (String.isEmpty whiteName) && not (String.isEmpty blackName) then
                [ Game.SystemEntry "Both players are present, the game begins." ]

            else
                []
    in
    List.map Game.MoveEntry servergame.gameHistory ++ startMsg ++ playerMsgs blackName "black" ++ playerMsgs whiteName "white"


fireCreateOrJoin : PendingAction -> String -> Maybe String -> Cmd Api.Msg
fireCreateOrJoin action name maybeToken =
    case action of
        PendingCreate vsAI cardSet ->
            Api.createGame name vsAI cardSet maybeToken

        PendingJoin gameid ->
            Api.joinGame gameid name maybeToken


toLobby : Model -> List Lobby.GameSummary -> Bool -> ( Model, Cmd Msg )
toLobby model summaries redirectToRoot =
    ( { model | page = LobbyPage (Lobby.init summaries) }
    , Cmd.batch
        (Ports.openLobbyStream ()
            :: (if redirectToRoot then
                    [ Nav.pushUrl model.key "/" ]

                else
                    []
               )
        )
    )


httpErrorToString : Http.Error -> String
httpErrorToString httpError =
    case httpError of
        Http.BadUrl _ ->
            "Invalid URL"

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error. Check your connection."

        Http.BadStatus code ->
            "Server error: " ++ String.fromInt code

        Http.BadBody msg_ ->
            "Invalid response: " ++ msg_
