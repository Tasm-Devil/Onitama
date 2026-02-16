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


type alias PlayerIdentity =
    { playerName : PlayerName
    , token : PlayerToken
    }


type alias Model =
    { key : Key
    , storedPlayers : List PlayerIdentity
    , page : Page
    }


type Page
    = Redirect Url
    | LobbyPage Lobby.Model
    | EnterNamePage GameId EnterName.Model
    | PlayingPage GameId PlayerName PlayerToken Game (List Game.LogEntry)
    | SpectatingPage GameId Game (List Game.LogEntry)



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
                PlayingPage _ _ _ game _ ->
                    case game.state of
                        GameOver _ ->
                            Sub.none

                        _ ->
                            Ports.gameEventReceived GameEventReceived

                SpectatingPage _ game _ ->
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
                        [ Lobby.view m
                        ]

                EnterNamePage _ enterNameModel ->
                    Html.div [ HtmlA.class "landing-screen" ]
                        (EnterName.view enterNameModel (List.map .playerName model.storedPlayers)
                            |> List.map (Html.map GotEnterNameMsg)
                        )

                PlayingPage _ _ _ game log ->
                    Html.div [ HtmlA.class "game-container" ]
                        ((game
                            |> Game.view
                            |> List.map (Html.map GotGameMsg)
                         )
                            ++ [ Game.viewLog log ]
                        )

                SpectatingPage _ game log ->
                    Html.div [ HtmlA.class "game-container" ]
                        ((game
                            |> Game.view
                            |> List.map (Html.map GotGameMsg)
                         )
                            ++ [ Game.viewLog log ]
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


encodePlayer : PlayerIdentity -> Encode.Value
encodePlayer player =
    Encode.object
        [ ( "playerName", Encode.string player.playerName )
        , ( "token", Encode.string player.token )
        ]


decodePlayers : Encode.Value -> List PlayerIdentity
decodePlayers value =
    let
        playerDecoder =
            Decode.map2 PlayerIdentity
                (Decode.field "playerName" Decode.string)
                (Decode.field "token" Decode.string)

        playersDecoder =
            Decode.list playerDecoder
    in
    Decode.decodeValue playersDecoder value
        |> Result.withDefault []





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
                ( model, Cmd.batch [ Nav.replaceUrl model.key "/", Cmd.map GotServerMsg Api.getGameIdFromServer ] )

            else if String.isEmpty gameidStr then
                ( model, Cmd.none )

            else
                case String.toInt gameidStr of
                    Just gameid ->
                        let
                            summary =
                                List.filter (\s -> s.summaryId == gameid) lobbyModel.games |> List.head

                            storedNames =
                                List.map .playerName model.storedPlayers

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
                        if isReturningPlayer || status == Just Lobby.WaitingForPlayers then
                            ( { model | page = EnterNamePage gameid (EnterName.Entering "") }
                            , Ports.closeLobbyStream ()
                            )

                        else
                            ( { model | page = Redirect url }
                            , Cmd.batch [ Ports.closeLobbyStream (), Cmd.map GotServerMsg (Api.getGameFromServer gameid) ]
                            )

                    Nothing ->
                        ( model, Cmd.none )

        EnterNamePage currentGameId _ ->
            if String.toInt gameidStr == Just currentGameId then
                ( model, Cmd.none )

            else
                ( { model | page = Redirect url }, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        PlayingPage _ _ _ _ _ ->
            ( { model | page = Redirect url }
            , Cmd.batch
                [ Ports.closeGameStream ()
                , Cmd.map GotServerMsg Api.getGameSummariesFromServer
                ]
            )

        SpectatingPage _ _ _ ->
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
        PlayingPage gameid name token game history_ ->
            case gamemsg of
                Game.UserClickedConcede ->
                    ( model, Cmd.map GotServerMsg <| Api.concede gameid token )

                _ ->
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
                    ( { model | page = PlayingPage gameid name token game_after history_ }, cmd )

        _ ->
            ( model, Cmd.none )



-- ENTER NAME MESSAGE HANDLERS


handleEnterNameMsg : EnterName.Msg -> Model -> ( Model, Cmd Msg )
handleEnterNameMsg enterNameMsg model =
    case ( enterNameMsg, model.page ) of
        ( EnterName.RequestJoin, EnterNamePage gameid enterNameModel ) ->
            handleRequestGame model gameid enterNameModel

        ( _, EnterNamePage gameid enterNameModel ) ->
            ( { model | page = EnterNamePage gameid (EnterName.update enterNameMsg enterNameModel) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleRequestGame : Model -> GameId -> EnterName.Model -> ( Model, Cmd Msg )
handleRequestGame model gameid enterNameModel =
    case enterNameModel of
        EnterName.Entering name ->
            if String.isEmpty (String.trim name) then
                ( model, Cmd.none )

            else
                let
                    maybeToken =
                        model.storedPlayers
                            |> List.filter (\p -> p.playerName == name)
                            |> List.head
                            |> Maybe.map .token
                in
                ( { model | page = EnterNamePage gameid (EnterName.Joining name) }
                , Cmd.map GotServerMsg <| Api.joinGame gameid name maybeToken
                )

        EnterName.JoinError name _ ->
            if String.isEmpty (String.trim name) then
                ( model, Cmd.none )

            else
                let
                    maybeToken =
                        model.storedPlayers
                            |> List.filter (\p -> p.playerName == name)
                            |> List.head
                            |> Maybe.map .token
                in
                ( { model | page = EnterNamePage gameid (EnterName.Joining name) }
                , Cmd.map GotServerMsg <| Api.joinGame gameid name maybeToken
                )

        EnterName.Joining _ ->
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

        ReceivedPostCreatedFromServer result ->
            handleMoveConfirmation result model

        ReceivedConcedeResponse _ ->
            ( model, Cmd.none )

        ReceivedGameFromServer result ->
            handleSpectateGame result model


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
                        let
                            summary =
                                List.filter (\s -> s.summaryId == gameid) summaries |> List.head

                            storedNames =
                                List.map .playerName model.storedPlayers

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
                        if isReturningPlayer || status == Just Lobby.WaitingForPlayers then
                            ( { model | page = EnterNamePage gameid (EnterName.Entering "") }
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
                        ( { model | page = LobbyPage { games = summaries, currentTime = Time.millisToPosix 0 } }
                        , Cmd.batch [ Nav.replaceUrl model.key "/", Ports.openLobbyStream (), Task.perform Tick Time.now, Cmd.map GotServerMsg Api.getGameIdFromServer ]
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


handleNewGameId : Result Http.Error GameId -> Model -> ( Model, Cmd Msg )
handleNewGameId result model =
    case ( result, model.page ) of
        ( Ok gameId, LobbyPage _ ) ->
            ( model, Nav.pushUrl model.key <| "/" ++ String.fromInt gameId )

        _ ->
            ( model, Cmd.none )


handleJoinResponse : Result Http.Error (Result Api.JoinError Api.JoinGameResponse) -> Model -> ( Model, Cmd Msg )
handleJoinResponse result model =
    case ( result, model.page ) of
        ( Ok (Ok joinResponse), EnterNamePage gameid _ ) ->
            joinGameSuccess model gameid joinResponse

        ( Ok (Err joinError), EnterNamePage gameid (EnterName.Joining name) ) ->
            ( { model | page = EnterNamePage gameid (EnterName.JoinError name (Api.joinErrorToString joinError)) }
            , Cmd.none
            )

        ( Err httpError, EnterNamePage gameid (EnterName.Joining name) ) ->
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
            ( { model | page = EnterNamePage gameid (EnterName.JoinError name errorMsg) }
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

        newPlayer =
            { playerName = name
            , token = token
            }

        saveCmd =
            if not (String.isEmpty name) then
                Ports.savePlayer (encodePlayer newPlayer)

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
    ( { model | page = PlayingPage gameid name token gameWithState initialLog }
    , Cmd.batch [ saveCmd, sseCmd ]
    )


handleMoveConfirmation : Result Http.Error (Result Api.MoveError Game.GameMove) -> Model -> ( Model, Cmd Msg )
handleMoveConfirmation result model =
    case ( result, model.page ) of
        ( Ok (Ok _), _ ) ->
            -- Move accepted: will be applied via GameStream SSE
            ( model, Cmd.none )

        ( _, PlayingPage gameid name token game history_ ) ->
            -- Move rejected or network error: revert to Thinking if still pending
            case game.state of
                MoveDone _ ->
                    ( { model | page = PlayingPage gameid name token { game | state = Thinking } history_ }
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
                    ( { model | page = SpectatingPage gameid game initialLog }
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
                PlayingPage gameid name token game log ->
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
                                                            let
                                                                winnerColor =
                                                                    if winnerStr == "White" then
                                                                        White

                                                                    else
                                                                        Black
                                                            in
                                                            { movedGame | state = GameOver winnerColor }

                                                        Nothing ->
                                                            movedGame

                                                soundCmd =
                                                    Ports.playSound "move"
                                            in
                                            ( { model | page = PlayingPage gameid name token updatedGame (Game.MoveEntry gameMove :: log) }
                                            , soundCmd
                                            )

                                        Nothing ->
                                            ( model, Cmd.none )

                                Api.ConcedeEvent winnerStr ->
                                    let
                                        winnerColor =
                                            if winnerStr == "White" then
                                                White

                                            else
                                                Black

                                        updatedGame =
                                            { game | state = GameOver winnerColor }
                                    in
                                    ( { model | page = PlayingPage gameid name token updatedGame log }
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
                                    ( { model | page = PlayingPage gameid name token updatedGame (startMsg :: joinedMsg :: log) }
                                    , Cmd.none
                                    )

                SpectatingPage gameid game log ->
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
                                                            let
                                                                winnerColor =
                                                                    if winnerStr == "White" then
                                                                        White

                                                                    else
                                                                        Black
                                                            in
                                                            { movedGame | state = GameOver winnerColor }

                                                        Nothing ->
                                                            movedGame
                                            in
                                            ( { model | page = SpectatingPage gameid updatedGame (Game.MoveEntry gameMove :: log) }
                                            , Ports.playSound "move"
                                            )

                                        Nothing ->
                                            ( model, Cmd.none )

                                Api.ConcedeEvent winnerStr ->
                                    let
                                        winnerColor =
                                            if winnerStr == "White" then
                                                White

                                            else
                                                Black

                                        updatedGame =
                                            { game | state = GameOver winnerColor }
                                    in
                                    ( { model | page = SpectatingPage gameid updatedGame log }
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
                                    in
                                    ( { model | page = SpectatingPage gameid game (startMsg :: joinedMsg :: log) }
                                    , Cmd.none
                                    )

                _ ->
                    ( model, Cmd.none )

        Err _ ->
            ( model, Cmd.none )
