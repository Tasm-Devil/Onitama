module Main exposing (main)

import Api exposing (Msg(..), ServerGame)
import Browser
import Browser.Navigation as Nav exposing (Key)
import Game.Card exposing (dummyCard)
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
import Time
import Url exposing (Url)



-- TYPES AND MODEL


type alias PlayerName =
    String


type alias PlayerToken =
    String


type alias PlayerIdentity =
    { playerName : String
    , token : PlayerToken
    }


type Model
    = Redirect Key Url (Maybe PlayerIdentity)
    | Lobby Lobby.Model (Maybe PlayerIdentity)
    | EnterName Key GameId PlayerName (Maybe PlayerIdentity)
    | Playing Key GameId PlayerName PlayerToken Game (List Game.GameMove) (Maybe PlayerIdentity)
    | Rejoining Key GameId PlayerName PlayerToken (Maybe PlayerIdentity)



-- MAIN


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( Redirect key url Nothing, Cmd.map GotServerMsg Api.getGameSummariesFromServer )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        sessionSub =
            Ports.loadPlayer PlayerLoadedFromStorage

        pollingSub =
            case model of
                Playing _ _ _ _ game _ _ ->
                    -- Poll every 2 seconds during active games
                    case game.state of
                        GameOver _ ->
                            Sub.none

                        _ ->
                            Time.every 2000 Tick

                Lobby _ _ ->
                    -- Poll every 3 seconds in lobby for new games
                    Time.every 3000 Tick

                _ ->
                    Sub.none
    in
    Sub.batch [ sessionSub, pollingSub ]


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

            EnterName _ _ player _ ->
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
                                [ viewHistory history
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


-- Encode player identity for localStorage
encodePlayer : PlayerIdentity -> Encode.Value
encodePlayer player =
    Encode.object
        [ ( "playerName", Encode.string player.playerName )
        , ( "token", Encode.string player.token )
        ]


-- Decode player identity from localStorage
decodePlayer : Encode.Value -> Maybe PlayerIdentity
decodePlayer value =
    let
        playerDecoder =
            Decode.map2 PlayerIdentity
                (Decode.field "playerName" Decode.string)
                (Decode.field "token" Decode.string)
    in
    Decode.decodeValue playerDecoder value
        |> Result.toMaybe


-- Check if we lost and should concede
checkAndConcede : Game -> GameId -> PlayerToken -> Cmd Msg
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



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Game.Msg
    | GotLobbyMsg Lobby.Msg
    | TypingName String
    | RequestGameFromServer
    | PlayerLoadedFromStorage Encode.Value
    | GotServerMsg Api.Msg
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

        GotLobbyMsg lobbymsg ->
            handleLobbyMsg lobbymsg model

        TypingName newname ->
            handleTypingName newname model

        RequestGameFromServer ->
            handleRequestGame model

        PlayerLoadedFromStorage value ->
            handlePlayerLoaded value model

        GotServerMsg servermsg ->
            handleServerMsg servermsg model

        Tick _ ->
            -- Auto-poll for updates
            case model of
                Playing _ _ _ _ _ _ _ ->
                    -- Poll for game state updates
                    handleRequestGame model

                Lobby _ _ ->
                    -- Poll for lobby/game summaries updates
                    ( model, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

                _ ->
                    ( model, Cmd.none )



-- URL CHANGE HANDLERS


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    let
        gameidStr =
            String.dropLeft 1 url.path
    in
    case model of
        Lobby lobby player ->
            if String.isEmpty gameidStr then
                ( model, Cmd.none )

            else
                case String.toInt gameidStr of
                    Just gameid ->
                        case player of
                            Just p ->
                                -- We have player identity, try to rejoin
                                ( Rejoining lobby.key gameid p.playerName p.token player
                                , Cmd.map GotServerMsg <| Api.joinGame gameid p.playerName (Just p.token)
                                )

                            Nothing ->
                                -- No player identity, need to enter name
                                ( EnterName lobby.key gameid "" Nothing, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        EnterName key currentGameId name player ->
            if String.toInt gameidStr == Just currentGameId then
                ( EnterName key currentGameId name player, Cmd.none )

            else
                ( Redirect key url player, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        Playing key _ _ _ _ _ player ->
            ( Redirect key url player, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        _ ->
            ( model, Cmd.none )



-- LINK CLICK HANDLERS


handleClickedLink : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
handleClickedLink urlRequest model =
    case model of
        Lobby lobby player ->
            case urlRequest of
                Browser.Internal url ->
                    ( Lobby lobby player, Nav.pushUrl lobby.key <| Url.toString url )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- GAME MESSAGE HANDLERS


handleGameMsg : Game.Msg -> Model -> ( Model, Cmd Msg )
handleGameMsg gamemsg model =
    case model of
        Playing key gameid name token game history_ player ->
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
            ( Playing key gameid name token game_after history_ player, cmd )

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
        EnterName key gameid _ player ->
            ( EnterName key gameid newname player, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleRequestGame : Model -> ( Model, Cmd Msg )
handleRequestGame model =
    case model of
        EnterName _ gameid name player ->
            let
                -- Use stored token if available
                maybeToken =
                    Maybe.map .token player
            in
            ( model, Cmd.map GotServerMsg <| Api.joinGame gameid name maybeToken )

        Playing _ gameid _ _ _ _ _ ->
            ( model, Cmd.map GotServerMsg <| Api.getGameFromServer gameid )

        _ ->
            ( model, Cmd.none )



-- PLAYER IDENTITY HANDLERS


handlePlayerLoaded : Encode.Value -> Model -> ( Model, Cmd Msg )
handlePlayerLoaded value model =
    case model of
        Redirect key url _ ->
            let
                loadedPlayer =
                    decodePlayer value

                gameidStr =
                    String.dropLeft 1 url.path

                maybeGameId =
                    String.toInt gameidStr
            in
            case maybeGameId of
                Just gameId ->
                    case loadedPlayer of
                        Just player ->
                            -- Have player identity and game ID, try to rejoin
                            ( Rejoining key gameId player.playerName player.token loadedPlayer
                            , Cmd.map GotServerMsg <| Api.joinGame gameId player.playerName (Just player.token)
                            )

                        Nothing ->
                            -- No player identity, need to enter name
                            ( Redirect key url Nothing, Cmd.none )

                Nothing ->
                    -- No game ID in URL
                    ( Redirect key url loadedPlayer, Cmd.none )

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
        ( Ok summaries, Redirect key url player ) ->
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
                        ( EnterName key gameid "" player, Cmd.none )

                    else
                        ( Lobby { status = Home summaries, key = key } player, Nav.pushUrl key "/" )

                Nothing ->
                    if String.isEmpty gameidStr then
                        ( Lobby { status = Home summaries, key = key } player, Cmd.none )

                    else
                        ( Lobby { status = Home summaries, key = key } player, Nav.pushUrl key "/" )

        ( Err _, Redirect key _ player ) ->
            ( Lobby { status = Home [], key = key } player, Cmd.none )

        ( Ok summaries, Lobby lobby player ) ->
            -- Update lobby with fresh game summaries (from polling)
            ( Lobby { lobby | status = Home summaries } player, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleNewGameId : Result Http.Error GameId -> Model -> ( Model, Cmd Msg )
handleNewGameId result model =
    case ( result, model ) of
        ( Ok gameId, Lobby lobby player ) ->
            ( Lobby lobby player, Nav.pushUrl lobby.key <| "/" ++ String.fromInt gameId )

        _ ->
            ( model, Cmd.none )


handleJoinResponse : Result Http.Error Api.JoinGameResponse -> Model -> ( Model, Cmd Msg )
handleJoinResponse result model =
    case ( result, model ) of
        ( Ok joinResponse, EnterName key gameid name _ ) ->
            -- Always save player identity when joining
            joinGameSuccess key gameid name joinResponse True

        ( Err _, EnterName key gameid name player ) ->
            ( EnterName key gameid name player, Cmd.none )

        ( Ok joinResponse, Rejoining key gameId playerName _ _ ) ->
            -- Update token from server when rejoining
            joinGameSuccess key gameId playerName joinResponse True

        ( Err _, Rejoining key gameId playerName _ _ ) ->
            -- Rejoin failed, enter name screen
            ( EnterName key gameId playerName Nothing, Cmd.none )

        ( Ok joinResponse, Redirect key url _ ) ->
            let
                gameid =
                    String.dropLeft 1 url.path
                        |> String.toInt
                        |> Maybe.withDefault 0

                -- Use player name from response (server knows who we are)
                playerName =
                    joinResponse.responseGame.gameWhiteName
                        |> (\name -> if String.isEmpty name then joinResponse.responseGame.gameBlackName else name)
            in
            joinGameSuccess key gameid playerName joinResponse True

        ( Err _, Redirect key url player ) ->
            let
                gameId =
                    String.dropLeft 1 url.path
                        |> String.toInt
                        |> Maybe.withDefault 0
            in
            ( EnterName key gameId "" player, Cmd.none )

        _ ->
            ( model, Cmd.none )


joinGameSuccess : Key -> GameId -> String -> Api.JoinGameResponse -> Bool -> ( Model, Cmd Msg )
joinGameSuccess key gameid name joinResponse shouldSavePlayer =
    let
        servergame =
            joinResponse.responseGame

        token =
            joinResponse.responseToken

        finalgame =
            buildGame name servergame

        newPlayer =
            { playerName = name
            , token = token
            }

        saveCmd =
            if shouldSavePlayer && not (String.isEmpty name) then
                Ports.savePlayer (encodePlayer newPlayer)

            else
                Cmd.none

        concedeCmd =
            checkAndConcede finalgame gameid token
    in
    ( Playing key gameid name token finalgame servergame.gameHistory (Just newPlayer)
    , Cmd.batch [ saveCmd, concedeCmd ]
    )


handleGameUpdate : Result Http.Error Api.ServerGame -> Model -> ( Model, Cmd Msg )
handleGameUpdate result model =
    case ( result, model ) of
        ( Ok servergame, Playing key gameid name token game currentHistory player ) ->
            case game.state of
                GameOver _ ->
                    ( Playing key gameid name token game servergame.gameHistory player, Cmd.none )

                _ ->
                    -- Only update if there's a NEW move (server history changed)
                    if List.head servergame.gameHistory == List.head currentHistory then
                        -- No new moves, keep current game state (preserves UI like selected pieces)
                        ( model, Cmd.none )

                    else
                        -- New move detected, apply it
                        List.head servergame.gameHistory
                            |> Maybe.map
                                (\gameMove ->
                                    let
                                        updatedGame =
                                            game |> Game.update (NewGameMove <| transformGameMove gameMove)

                                        concedeCmd =
                                            checkAndConcede updatedGame gameid token
                                    in
                                    ( Playing key gameid name token updatedGame servergame.gameHistory player
                                    , concedeCmd
                                    )
                                )
                            |> Maybe.withDefault ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleMoveConfirmation : Result Http.Error Game.GameMove -> Model -> ( Model, Cmd Msg )
handleMoveConfirmation result model =
    case ( result, model ) of
        ( Ok gameMove, Playing key gameid name token game history_ player ) ->
            let
                updatedGame =
                    game |> Game.update (NewGameMove <| transformGameMove gameMove)

                concedeCmd =
                    checkAndConcede updatedGame gameid token
            in
            ( Playing key gameid name token updatedGame (gameMove :: history_) player
            , concedeCmd
            )

        _ ->
            ( model, Cmd.none )
