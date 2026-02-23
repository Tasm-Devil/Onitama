module Main exposing (main)

import Api exposing (GameEvent(..), GameId, Msg(..), ServerGame)
import Browser
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Game.Card exposing (dummyCard)
import Game.Figure exposing (Color(..))
import Game.Game as Game exposing (Game, GameState(..), Msg(..), transformGameMove)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Lobby
import Ports
import Task
import Time
import Url exposing (Url)



-- TYPES


type alias Model =
    { key : Key
    , currentUser : Maybe Api.AuthUser
    , page : Page
    , userMenuOpen : Bool
    }


type Route
    = LobbyRoute
    | GameRoute GameId


type Page
    = Loading Route
    | LobbyPage Lobby.Model
    | AwaitingGame (Maybe GameId) String
    | GamePage GameId Game (List Game.LogEntry)


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Game.Msg
    | GotLobbyMsg Lobby.Msg
    | GotServerMsg Api.Msg
    | LobbyEventReceived Encode.Value
    | GameEventReceived Encode.Value
    | LobbyTick Time.Posix
    | ToggleUserMenu
    | CloseUserMenu
    | ClickedConcede



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
    ( { key = key, currentUser = Nothing, page = Loading (routeFromUrl url), userMenuOpen = False }
    , Cmd.batch
        [ Cmd.map GotServerMsg Api.getMe
        , Cmd.map GotServerMsg Api.getGameSummariesFromServer
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
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

        userMenuSub =
            if model.userMenuOpen then
                Browser.Events.onClick (Decode.succeed CloseUserMenu)

            else
                Sub.none
    in
    Sub.batch [ sseSub, userMenuSub ]



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

        GotLobbyMsg lobbyMsg ->
            handleLobbyMsg lobbyMsg model

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

        ToggleUserMenu ->
            ( { model | userMenuOpen = not model.userMenuOpen }, Cmd.none )

        CloseUserMenu ->
            ( { model | userMenuOpen = False }, Cmd.none )

        ClickedConcede ->
            case model.page of
                GamePage gameid _ _ ->
                    ( model, Cmd.map GotServerMsg (Api.concede gameid) )

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
            if String.startsWith "/oauth2/" url.path then
                ( model, Nav.load (Url.toString url) )

            else
                ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
            ( model, Nav.load href )


handleGameNavigation : Model -> Lobby.Model -> GameId -> ( Model, Cmd Msg )
handleGameNavigation model lobbyModel gameid =
    case resolveGameNavigation model.currentUser (Lobby.getGames lobbyModel) gameid of
        JoinDirectly ->
            let
                displayName =
                    model.currentUser |> Maybe.map .displayName |> Maybe.withDefault ""
            in
            ( { model | page = AwaitingGame (Just gameid) displayName }
            , Cmd.batch
                [ Ports.closeLobbyStream ()
                , Cmd.map GotServerMsg (Api.joinGame gameid)
                ]
            )

        SpectateGame ->
            ( { model | page = Loading (GameRoute gameid) }
            , Cmd.batch [ Ports.closeLobbyStream (), Cmd.map GotServerMsg (Api.getGameFromServer gameid) ]
            )


type NavigationAction
    = JoinDirectly
    | SpectateGame


resolveGameNavigation : Maybe Api.AuthUser -> List Lobby.GameSummary -> GameId -> NavigationAction
resolveGameNavigation currentUser summaries gameid =
    let
        summary =
            List.filter (\s -> s.summaryId == gameid) summaries |> List.head

        status =
            Maybe.map .summaryStatus summary

        matchesGamePlayer user =
            case summary of
                Just s ->
                    user.displayName == s.summaryPlayer1 || user.displayName == s.summaryPlayer2

                Nothing ->
                    False
    in
    case status of
        Just Lobby.Completed ->
            SpectateGame

        Just Lobby.WaitingForPlayers ->
            case currentUser of
                Just _ ->
                    JoinDirectly

                Nothing ->
                    SpectateGame

        Just Lobby.InProgress ->
            case currentUser of
                Just user ->
                    if matchesGamePlayer user then
                        JoinDirectly

                    else
                        SpectateGame

                Nothing ->
                    SpectateGame

        Nothing ->
            case currentUser of
                Just _ ->
                    JoinDirectly

                Nothing ->
                    SpectateGame


resolveGameRoute : Model -> List Lobby.GameSummary -> GameId -> ( Model, Cmd Msg )
resolveGameRoute model summaries gameid =
    case resolveGameNavigation model.currentUser summaries gameid of
        JoinDirectly ->
            let
                displayName =
                    model.currentUser |> Maybe.map .displayName |> Maybe.withDefault ""
            in
            ( { model | page = AwaitingGame (Just gameid) displayName }
            , Cmd.map GotServerMsg (Api.joinGame gameid)
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
                    ( { model | page = AwaitingGame (Just gameid) (model.currentUser |> Maybe.map .displayName |> Maybe.withDefault "") }
                    , Cmd.batch
                        [ Ports.closeLobbyStream ()
                        , Cmd.map GotServerMsg (Api.joinGame gameid)
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

                displayName =
                    model.currentUser |> Maybe.map .displayName |> Maybe.withDefault ""
            in
            ( { model | page = AwaitingGame Nothing displayName }
            , Cmd.batch
                [ Ports.closeLobbyStream ()
                , Cmd.map GotServerMsg (Api.createGame vsAI cardSet)
                ]
            )

        _ ->
            ( model, Cmd.none )



-- GAME HANDLERS


handleGameMsg : Game.Msg -> Model -> ( Model, Cmd Msg )
handleGameMsg gamemsg model =
    case model.page of
        GamePage gameid game log ->
            let
                updatedGame =
                    Game.update gamemsg game

                cmd =
                    case updatedGame.state of
                        MoveDone gameMove ->
                            transformGameMove gameMove
                                |> Api.postNewGameMove gameid
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

        ReceivedMe result ->
            handleMeResponse result model


handleMeResponse : Result Http.Error Api.AuthUser -> Model -> ( Model, Cmd Msg )
handleMeResponse result model =
    case result of
        Ok user ->
            ( { model | currentUser = Just user }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


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
        ( Ok (Ok joinResponse), AwaitingGame (Just gameid) _ ) ->
            joinGameSuccess model gameid joinResponse

        ( Ok (Err _), AwaitingGame _ _ ) ->
            ( { model | page = Loading LobbyRoute }
            , Cmd.batch
                [ Nav.pushUrl model.key "/"
                , Cmd.map GotServerMsg Api.getGameSummariesFromServer
                ]
            )

        ( Err _, AwaitingGame _ _ ) ->
            ( { model | page = Loading LobbyRoute }
            , Cmd.batch
                [ Nav.pushUrl model.key "/"
                , Cmd.map GotServerMsg Api.getGameSummariesFromServer
                ]
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
            Game.SystemEntry (name ++ " joined game as " ++ myColor ++ ".")

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

        urlCmd =
            case model.page of
                AwaitingGame Nothing _ ->
                    -- Game creation: push new URL
                    Nav.pushUrl model.key ("/" ++ String.fromInt gameid)

                _ ->
                    -- Join: URL was already set
                    Nav.replaceUrl model.key ("/" ++ String.fromInt gameid)
    in
    ( { model | page = GamePage gameid gameWithState initialLog }
    , Cmd.batch
        [ Ports.closeLobbyStream ()
        , Ports.openGameStream gameid
        , urlCmd
        ]
    )


handleNewGameResponse : Result Http.Error (Result Api.JoinError Api.NewGameResponse) -> Model -> ( Model, Cmd Msg )
handleNewGameResponse result model =
    case ( result, model.page ) of
        ( Ok (Ok response), AwaitingGame _ _ ) ->
            joinGameSuccess model response.newGameId response.newGameJoinResponse

        ( Ok (Err _), AwaitingGame _ _ ) ->
            ( { model | page = Loading LobbyRoute }
            , Cmd.batch
                [ Nav.pushUrl model.key "/"
                , Cmd.map GotServerMsg Api.getGameSummariesFromServer
                ]
            )

        ( Err _, AwaitingGame _ _ ) ->
            ( { model | page = Loading LobbyRoute }
            , Cmd.batch
                [ Nav.pushUrl model.key "/"
                , Cmd.map GotServerMsg Api.getGameSummariesFromServer
                ]
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
            [ viewHeader model
            , case model.page of
                Loading _ ->
                    Html.div [ HtmlA.class "landing-screen" ]
                        [ Html.div [ HtmlA.class "spinner" ] [] ]

                LobbyPage lobbyModel ->
                    Html.div [ HtmlA.class "lobby" ]
                        [ Lobby.view (Maybe.map .displayName model.currentUser) lobbyModel |> Html.map GotLobbyMsg ]

                AwaitingGame _ displayName ->
                    Html.div [ HtmlA.class "landing-screen" ]
                        [ Html.h2 [] [ Html.text "Joining game..." ]
                        , Html.p [] [ Html.text ("Joining as " ++ displayName) ]
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


viewHeader : Model -> Html Msg
viewHeader model =
    Html.header [ HtmlA.class "app-header" ]
        [ viewHeaderLeft model.currentUser model.userMenuOpen
        , viewHeaderCenter model.page
        , viewHeaderRight model.page
        ]


viewHeaderLeft : Maybe Api.AuthUser -> Bool -> Html Msg
viewHeaderLeft maybeUser menuOpen =
    Html.div [ HtmlA.class "header-left" ]
        (case maybeUser of
            Just user ->
                [ Html.div [ HtmlA.class "user-menu-wrapper" ]
                    [ Html.img
                        [ HtmlA.class "user-avatar"
                        , HtmlA.src ("https://github.com/" ++ user.displayName ++ ".png?size=56")
                        , HtmlA.alt user.displayName
                        , Html.Events.custom "click"
                            (Decode.succeed
                                { message = ToggleUserMenu
                                , stopPropagation = True
                                , preventDefault = False
                                }
                            )
                        ]
                        []
                    , if menuOpen then
                        Html.div [ HtmlA.class "user-dropdown" ]
                            [ Html.div [ HtmlA.class "dropdown-name" ] [ Html.text user.displayName ]
                            , Html.a [ HtmlA.class "dropdown-item", HtmlA.href "/oauth2/sign_out?rd=/" ] [ Html.text "Logout" ]
                            ]

                      else
                        Html.text ""
                    ]
                ]

            Nothing ->
                []
        )


viewHeaderCenter : Page -> Html Msg
viewHeaderCenter page =
    Html.div [ HtmlA.class "header-center" ]
        [ Html.span [ HtmlA.class "header-status" ]
            (case page of
                GamePage _ game _ ->
                    [ Html.text (gameStatusText game) ]

                AwaitingGame _ _ ->
                    [ Html.text "Joining game..." ]

                _ ->
                    []
            )
        ]


gameStatusText : Game -> String
gameStatusText game =
    let
        colorStr color =
            case color of
                White ->
                    "White"

                Black ->
                    "Black"
    in
    case game.state of
        WaitingForOpponent ->
            "Waiting for opponent..."

        GameOver winner ->
            colorStr winner ++ " wins!"

        _ ->
            let
                nextStr =
                    colorStr game.nextColor
            in
            if game.spectating then
                nextStr ++ " to move"

            else if game.nextColor == game.myColor then
                nextStr ++ " (you) to move"

            else
                nextStr ++ " to move"


viewHeaderRight : Page -> Html Msg
viewHeaderRight page =
    Html.div [ HtmlA.class "header-right" ]
        (case page of
            GamePage _ game _ ->
                Html.a [ HtmlA.class "header-btn", HtmlA.href "/" ] [ Html.text "Back" ]
                    :: (if not game.spectating then
                            case game.state of
                                WaitingForOpponent ->
                                    []

                                GameOver _ ->
                                    []

                                _ ->
                                    [ Html.button
                                        [ HtmlA.class "header-btn concede-btn"
                                        , Html.Events.onClick ClickedConcede
                                        ]
                                        [ Html.text "Concede" ]
                                    ]

                        else
                            []
                       )

            _ ->
                []
        )


viewFooter : Html Msg
viewFooter =
    Html.footer [ HtmlA.class "footer" ]
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


parseWinnerColor : String -> Color
parseWinnerColor str =
    if str == "White" then
        White

    else
        Black


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
