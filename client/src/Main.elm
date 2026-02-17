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



-- TYPES AND MODEL


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
    , page : Page
    }


type GameCreation
    = JoinExisting GameId
    | CreateNew
    | CreateNewVsAI


type Page
    = Redirect Url
    | LobbyPage Lobby.Model
    | EnterNamePage GameCreation EnterName.Model
    | GamePage GameId (Maybe PlayerSession) Game (List Game.LogEntry)



-- MAIN


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, storedPlayers = [], page = Redirect url }
    , Cmd.map GotServerMsg Api.getGameSummariesFromServer
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        playersSub =
            Ports.loadPlayers StoredPlayersLoaded

        sseSub =
            case model.page of
                GamePage _ _ game _ ->
                    case game.state of
                        GameOver _ ->
                            Sub.none

                        _ ->
                            Ports.gameEventReceived GameEventReceived

                LobbyPage _ ->
                    Sub.batch
                        [ Ports.lobbyEventReceived LobbyEventReceived
                        , Time.every (1 * 60 * 1000) Tick
                        ]

                _ ->
                    Sub.none
    in
    Sub.batch [ playersSub, sseSub ]


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
        [ Html.div [ HtmlA.class "page-wrapper" ]
            [ case model.page of
                Redirect url ->
                    Html.div [ HtmlA.class "lobby" ]
                        [ Html.h1 []
                            [ Html.text url.path ]
                        ]

                LobbyPage m ->
                    Html.div [ HtmlA.class "lobby" ]
                        [ Lobby.view (List.map .name model.storedPlayers) m
                        ]

                EnterNamePage _ enterNameModel ->
                    Html.div [ HtmlA.class "landing-screen" ]
                        (EnterName.view enterNameModel (List.map .name model.storedPlayers)
                            |> List.map (Html.map GotEnterNameMsg)
                        )

                GamePage _ _ game log ->
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


shouldJoinGame : List PlayerSession -> List Lobby.GameSummary -> GameId -> Bool
shouldJoinGame storedPlayers summaries gameid =
    let
        summary =
            List.filter (\s -> s.summaryId == gameid) summaries |> List.head

        storedNames =
            List.map .name storedPlayers

        isReturningPlayer =
            case summary of
                Just s ->
                    List.member s.summaryPlayer1 storedNames
                        || List.member s.summaryPlayer2 storedNames

                Nothing ->
                    False

        status =
            Maybe.map .summaryStatus summary
    in
    status /= Just Lobby.Completed && (isReturningPlayer || status == Just Lobby.WaitingForPlayers)


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



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Game.Msg
    | GotEnterNameMsg EnterName.Msg
    | StoredPlayersLoaded Encode.Value
    | GotServerMsg Api.Msg
    | LobbyEventReceived Encode.Value
    | GameEventReceived Encode.Value
    | Tick Time.Posix


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

        StoredPlayersLoaded value ->
            ( { model | storedPlayers = decodePlayers value }, Cmd.none )

        GotServerMsg servermsg ->
            handleServerMsg servermsg model

        LobbyEventReceived value ->
            handleLobbyEvent value model

        GameEventReceived value ->
            handleGameEvent value model

        Tick currentTime ->
            case model.page of
                LobbyPage lobbyModel ->
                    ( { model | page = LobbyPage { lobbyModel | currentTime = currentTime } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- URL CHANGE HANDLERS


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    let
        gameidStr =
            String.dropLeft 1 url.path
    in
    case model.page of
        LobbyPage lobbyModel ->
            if gameidStr == "newgame" then
                ( { model | page = EnterNamePage CreateNew (EnterName.Entering "") }
                , Ports.closeLobbyStream ()
                )

            else if gameidStr == "newgame-ai" then
                ( { model | page = EnterNamePage CreateNewVsAI (EnterName.Entering "") }
                , Ports.closeLobbyStream ()
                )

            else if String.isEmpty gameidStr then
                ( model, Cmd.none )

            else
                case String.toInt gameidStr of
                    Just gameid ->
                        if shouldJoinGame model.storedPlayers lobbyModel.games gameid then
                            ( { model | page = EnterNamePage (JoinExisting gameid) (EnterName.Entering "") }
                            , Ports.closeLobbyStream ()
                            )

                        else
                            ( { model | page = Redirect url }
                            , Cmd.batch [ Ports.closeLobbyStream (), Cmd.map GotServerMsg (Api.getGameFromServer gameid) ]
                            )

                    Nothing ->
                        ( model, Cmd.none )

        EnterNamePage creation _ ->
            case creation of
                JoinExisting currentGameId ->
                    if String.toInt gameidStr == Just currentGameId then
                        ( model, Cmd.none )

                    else
                        ( { model | page = Redirect url }, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

                _ ->
                    ( { model | page = Redirect url }, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        GamePage currentGameId _ _ _ ->
            if String.toInt gameidStr == Just currentGameId then
                ( model, Cmd.none )

            else
                ( { model | page = Redirect url }
                , Cmd.batch
                    [ Ports.closeGameStream ()
                    , Cmd.map GotServerMsg Api.getGameSummariesFromServer
                    ]
                )

        _ ->
            ( model, Cmd.none )



-- LINK CLICK HANDLERS


handleClickedLink : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
handleClickedLink urlRequest model =
    case model.page of
        LobbyPage _ ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- GAME MESSAGE HANDLERS


handleGameMsg : Game.Msg -> Model -> ( Model, Cmd Msg )
handleGameMsg gamemsg model =
    case model.page of
        GamePage gameid (Just session) game log ->
            case gamemsg of
                Game.UserClickedConcede ->
                    ( model, Cmd.map GotServerMsg <| Api.concede gameid session.token )

                _ ->
                    let
                        game_after =
                            Game.update gamemsg game

                        cmd =
                            case game_after.state of
                                MoveDone gameMove ->
                                    transformGameMove gameMove
                                        |> Api.postNewGameMove gameid session.token
                                        |> Cmd.map GotServerMsg

                                _ ->
                                    Cmd.none
                    in
                    ( { model | page = GamePage gameid (Just session) game_after log }, cmd )

        GamePage gameid Nothing game log ->
            let
                game_after =
                    Game.update gamemsg game
            in
            ( { model | page = GamePage gameid Nothing game_after log }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- ENTER NAME MESSAGE HANDLERS


handleEnterNameMsg : EnterName.Msg -> Model -> ( Model, Cmd Msg )
handleEnterNameMsg enterNameMsg model =
    case ( enterNameMsg, model.page ) of
        ( EnterName.RequestJoin, EnterNamePage creation enterNameModel ) ->
            handleRequestGame model creation enterNameModel

        ( _, EnterNamePage creation enterNameModel ) ->
            ( { model | page = EnterNamePage creation (EnterName.update enterNameMsg enterNameModel) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleRequestGame : Model -> GameCreation -> EnterName.Model -> ( Model, Cmd Msg )
handleRequestGame model creation enterNameModel =
    case EnterName.getName enterNameModel of
        Just name ->
            if String.isEmpty (String.trim name) then
                ( model, Cmd.none )

            else
                let
                    maybeToken =
                        model.storedPlayers
                            |> List.filter (\p -> p.name == name)
                            |> List.head
                            |> Maybe.map .token

                    cmd =
                        case creation of
                            JoinExisting gameid ->
                                Api.joinGame gameid name maybeToken

                            CreateNew ->
                                Api.createGame name False maybeToken

                            CreateNewVsAI ->
                                Api.createGame name True maybeToken
                in
                ( { model | page = EnterNamePage creation (EnterName.Joining name) }
                , Cmd.map GotServerMsg cmd
                )

        Nothing ->
            ( model, Cmd.none )



-- SERVER MESSAGE HANDLERS


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
        ( Ok summaries, Redirect url ) ->
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
                        if shouldJoinGame model.storedPlayers summaries gameid then
                            ( { model | page = EnterNamePage (JoinExisting gameid) (EnterName.Entering "") }
                            , Cmd.none
                            )

                        else
                            ( model
                            , Cmd.map GotServerMsg (Api.getGameFromServer gameid)
                            )

                    else
                        ( { model | page = LobbyPage { games = summaries, currentTime = Time.millisToPosix 0 } }
                        , Cmd.batch [ Nav.pushUrl model.key "/", Ports.openLobbyStream (), Task.perform Tick Time.now ]
                        )

                Nothing ->
                    if gameidStr == "newgame" then
                        ( { model | page = EnterNamePage CreateNew (EnterName.Entering "") }
                        , Cmd.none
                        )

                    else if gameidStr == "newgame-ai" then
                        ( { model | page = EnterNamePage CreateNewVsAI (EnterName.Entering "") }
                        , Cmd.none
                        )

                    else if String.isEmpty gameidStr then
                        ( { model | page = LobbyPage { games = summaries, currentTime = Time.millisToPosix 0 } }
                        , Cmd.batch [ Ports.openLobbyStream (), Task.perform Tick Time.now ]
                        )

                    else
                        ( { model | page = LobbyPage { games = summaries, currentTime = Time.millisToPosix 0 } }
                        , Cmd.batch [ Nav.pushUrl model.key "/", Ports.openLobbyStream (), Task.perform Tick Time.now ]
                        )

        ( Err _, Redirect _ ) ->
            ( { model | page = LobbyPage { games = [], currentTime = Time.millisToPosix 0 } }
            , Cmd.batch [ Ports.openLobbyStream (), Task.perform Tick Time.now ]
            )

        ( Ok summaries, LobbyPage lobby ) ->
            ( { model | page = LobbyPage { lobby | games = summaries } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleJoinResponse : Result Http.Error (Result Api.JoinError Api.JoinGameResponse) -> Model -> ( Model, Cmd Msg )
handleJoinResponse result model =
    case ( result, model.page ) of
        ( Ok (Ok joinResponse), EnterNamePage (JoinExisting gameid) _ ) ->
            joinGameSuccess model gameid joinResponse

        ( Ok (Err joinError), EnterNamePage creation (EnterName.Joining name) ) ->
            ( { model | page = EnterNamePage creation (EnterName.JoinError name (Api.joinErrorToString joinError)) }
            , Cmd.none
            )

        ( Err httpError, EnterNamePage creation (EnterName.Joining name) ) ->
            let
                errorMsg =
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
            in
            ( { model | page = EnterNamePage creation (EnterName.JoinError name errorMsg) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


joinGameSuccess : Model -> GameId -> Api.JoinGameResponse -> ( Model, Cmd Msg )
joinGameSuccess model gameid joinResponse =
    let
        servergame =
            joinResponse.responseGame

        token =
            joinResponse.responseToken

        name =
            joinResponse.responsePlayerName

        finalgame =
            buildGame name servergame

        session =
            { name = name
            , token = token
            }

        saveCmd =
            if not (String.isEmpty name) then
                Ports.savePlayer (encodePlayer session)

            else
                Cmd.none

        sseCmd =
            Cmd.batch
                [ Ports.closeLobbyStream ()
                , Ports.openGameStream gameid
                ]

        myColor =
            if name == servergame.gameBlackName then
                "black"

            else
                "white"

        opponentColor =
            if myColor == "white" then
                "black"

            else
                "white"

        opponentName =
            if myColor == "white" then
                servergame.gameBlackName

            else
                servergame.gameWhiteName

        bothPresent =
            not (String.isEmpty opponentName)

        joinMsg =
            Game.SystemEntry ("You joined game as " ++ myColor ++ ".")

        opponentMsg =
            Game.SystemEntry (opponentName ++ " has joined the game as " ++ opponentColor ++ ".")

        startMsg =
            Game.SystemEntry "Both players are present, the game begins."

        initialLog =
            if bothPresent then
                List.map Game.MoveEntry servergame.gameHistory ++ [ startMsg, opponentMsg, joinMsg ]

            else
                List.map Game.MoveEntry servergame.gameHistory ++ [ joinMsg ]

        gameWithState =
            if bothPresent then
                finalgame

            else
                { finalgame | state = WaitingForOpponent }
    in
    ( { model | page = GamePage gameid (Just session) gameWithState initialLog }
    , Cmd.batch [ saveCmd, sseCmd, Nav.replaceUrl model.key ("/" ++ String.fromInt gameid) ]
    )


handleNewGameResponse : Result Http.Error (Result Api.JoinError Api.NewGameResponse) -> Model -> ( Model, Cmd Msg )
handleNewGameResponse result model =
    case ( result, model.page ) of
        ( Ok (Ok response), EnterNamePage _ _ ) ->
            joinGameSuccess model response.newGameId response.newGameJoinResponse

        ( Ok (Err joinError), EnterNamePage creation (EnterName.Joining name) ) ->
            ( { model | page = EnterNamePage creation (EnterName.JoinError name (Api.joinErrorToString joinError)) }
            , Cmd.none
            )

        ( Err httpError, EnterNamePage creation (EnterName.Joining name) ) ->
            let
                errorMsg =
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
            in
            ( { model | page = EnterNamePage creation (EnterName.JoinError name errorMsg) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleMoveConfirmation : Result Http.Error (Result Api.MoveError Game.GameMove) -> Model -> ( Model, Cmd Msg )
handleMoveConfirmation result model =
    case ( result, model.page ) of
        ( Ok (Ok _), _ ) ->
            -- Move accepted: will be applied via GameStream SSE
            ( model, Cmd.none )

        ( _, GamePage gameid (Just session) game log ) ->
            -- Move rejected or network error: revert to Thinking if still pending
            case game.state of
                MoveDone _ ->
                    ( { model | page = GamePage gameid (Just session) { game | state = Thinking } log }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleSpectateGame : Result Http.Error ServerGame -> Model -> ( Model, Cmd Msg )
handleSpectateGame result model =
    case ( result, model.page ) of
        ( Ok servergame, Redirect url ) ->
            let
                gameidStr =
                    String.dropLeft 1 url.path

                maybeGameId =
                    String.toInt gameidStr
            in
            case maybeGameId of
                Just gameid ->
                    let
                        game =
                            buildSpectatorGame servergame

                        whiteName =
                            servergame.gameWhiteName

                        blackName =
                            servergame.gameBlackName

                        whiteMsg =
                            if not (String.isEmpty whiteName) then
                                [ Game.SystemEntry (whiteName ++ " has joined the game as white.") ]

                            else
                                []

                        blackMsg =
                            if not (String.isEmpty blackName) then
                                [ Game.SystemEntry (blackName ++ " has joined the game as black.") ]

                            else
                                []

                        startMsg =
                            if not (String.isEmpty whiteName) && not (String.isEmpty blackName) then
                                [ Game.SystemEntry "Both players are present, the game begins." ]

                            else
                                []

                        initialLog =
                            List.map Game.MoveEntry servergame.gameHistory ++ startMsg ++ blackMsg ++ whiteMsg
                    in
                    ( { model | page = GamePage gameid Nothing game initialLog }
                    , Ports.openGameStream gameid
                    )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SSE EVENT HANDLERS


handleLobbyEvent : Encode.Value -> Model -> ( Model, Cmd Msg )
handleLobbyEvent _ model =
    case model.page of
        LobbyPage _ ->
            ( model, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        _ ->
            ( model, Cmd.none )


handleGameEvent : Encode.Value -> Model -> ( Model, Cmd Msg )
handleGameEvent value model =
    case Decode.decodeValue Api.decodeGameEvent value of
        Ok event ->
            case model.page of
                GamePage gameid session game log ->
                    case game.state of
                        GameOver _ ->
                            ( model, Cmd.none )

                        _ ->
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
                                            ( { model | page = GamePage gameid session updatedGame (Game.MoveEntry gameMove :: log) }
                                            , Ports.playSound "move"
                                            )

                                        Nothing ->
                                            ( model, Cmd.none )

                                Api.ConcedeEvent winnerStr ->
                                    ( { model | page = GamePage gameid session { game | state = GameOver (parseWinnerColor winnerStr) } log }
                                    , Cmd.none
                                    )

                                Api.PlayerJoinedEvent joinedName joinedColor ->
                                    let
                                        colorStr =
                                            String.toLower joinedColor

                                        joinedMsg =
                                            Game.SystemEntry (joinedName ++ " has joined the game as " ++ colorStr ++ ".")

                                        startMsg =
                                            Game.SystemEntry "Both players are present, the game begins."

                                        updatedGame =
                                            { game | state = Thinking }
                                    in
                                    ( { model | page = GamePage gameid session updatedGame (startMsg :: joinedMsg :: log) }
                                    , Cmd.none
                                    )

                _ ->
                    ( model, Cmd.none )

        Err _ ->
            ( model, Cmd.none )
