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


type EnterNameState
    = Entering PlayerName -- current name, all stored players
    | Joining PlayerName -- waiting for response
    | JoinError PlayerName String -- name, error, all stored players


type Model
    = Redirect Key Url (List PlayerIdentity)
    | Lobby Key Lobby.Model (List PlayerIdentity)
    | EnterName Key GameId EnterNameState (List PlayerIdentity)
    | Playing Key GameId PlayerName PlayerToken Game (List Game.GameMove) (List PlayerIdentity)



-- MAIN


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( Redirect key url [], Cmd.map GotServerMsg Api.getGameSummariesFromServer )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        playersSub =
            Ports.loadPlayers StoredPlayersLoaded

        pollingSub =
            case model of
                Playing _ _ _ _ game _ _ ->
                    -- Poll every 2 seconds during active games
                    case game.state of
                        GameOver _ ->
                            Sub.none

                        _ ->
                            Time.every 2000 Tick

                Lobby _ _ _ ->
                    -- Poll every 3 seconds in lobby for new games
                    Time.every 3000 Tick

                _ ->
                    Sub.none
    in
    Sub.batch [ playersSub, pollingSub ]


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
                    , viewFooter
                    ]

            Lobby _ m _ ->
                Html.div [ HtmlA.class "lobby" ]
                    [ Lobby.view m
                        |> Html.map GotLobbyMsg
                    , viewFooter
                    ]

            EnterName _ _ state storedPlayers ->
                case state of
                    Entering currentName ->
                        Html.div [ HtmlA.class "landing-screen" ]
                            [ Html.form [ HtmlA.id "name-form" ]
                                [ Html.h1 []
                                    [ Html.text "Onitama" ]
                                , Html.small [] [ Html.text "Enter your name or select from existing players..." ]
                                , Html.div [ HtmlA.class "name-line" ]
                                    [ Html.input
                                        [ HtmlA.id "name"
                                        , HtmlA.placeholder "Enter your name"
                                        , HtmlA.value currentName
                                        , HtmlA.attribute "autocomplete" "off"
                                        , HtmlA.attribute "list" "player-names"
                                        , onInput TypingName
                                        ]
                                        []
                                    , Html.datalist [ HtmlA.id "player-names" ]
                                        (List.map (\p -> Html.option [ HtmlA.value p.playerName ] []) storedPlayers)
                                    , Html.input
                                        [ HtmlA.type_ "button"
                                        , HtmlA.value "Join"
                                        , HtmlA.disabled (String.isEmpty currentName)
                                        , onClick RequestGameFromServer
                                        ]
                                        []
                                    ]
                                ]
                            , viewFooter
                            ]

                    Joining playerName ->
                        Html.div [ HtmlA.class "landing-screen" ]
                            [ Html.h2 [] [ Html.text "Joining game..." ]
                            , Html.p [] [ Html.text ("Joining as " ++ playerName) ]
                            , Html.div [ HtmlA.class "spinner" ] []
                            , viewFooter
                            ]

                    JoinError playerName errorMsg ->
                        Html.div [ HtmlA.class "landing-screen" ]
                            [ Html.form [ HtmlA.id "name-form" ]
                                [ Html.h1 [] [ Html.text "Onitama" ]
                                , Html.div [ HtmlA.class "error-message" ]
                                    [ Html.text errorMsg ]
                                , Html.div [ HtmlA.class "name-line" ]
                                    [ Html.input
                                        [ HtmlA.id "name"
                                        , HtmlA.placeholder "Enter your name"
                                        , HtmlA.value playerName
                                        , HtmlA.attribute "autocomplete" "off"
                                        , HtmlA.attribute "list" "player-names"
                                        , onInput TypingName
                                        ]
                                        []
                                    , Html.datalist [ HtmlA.id "player-names" ]
                                        (List.map (\p -> Html.option [ HtmlA.value p.playerName ] []) storedPlayers)
                                    , Html.input
                                        [ HtmlA.type_ "button"
                                        , HtmlA.value "Try Again"
                                        , HtmlA.disabled (String.isEmpty playerName)
                                        , onClick RequestGameFromServer
                                        ]
                                        []
                                    ]
                                ]
                            , viewFooter
                            ]

            Playing _ _ _ _ game history _ ->
                Html.div [ HtmlA.class "game-container", HtmlA.style "display" "flex" ]
                    ((game
                        |> Game.view
                        |> List.map (Html.map GotGameMsg)
                     )
                        ++ [ viewHistory history
                           , viewFooter
                           ]
                    )
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


viewHistory : List GameMove -> Html Msg
viewHistory history =
    Html.div [ HtmlA.class "game-log" ]
        [ Html.ul [ HtmlA.id "log-lines" ]
            (List.map viewGameMove history)
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



-- Decode list of player identities from localStorage


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
    | StoredPlayersLoaded Encode.Value
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

        StoredPlayersLoaded value ->
            handleStoredPlayersLoaded value model

        GotServerMsg servermsg ->
            handleServerMsg servermsg model

        Tick currentTime ->
            -- Auto-poll for updates
            case model of
                Playing _ _ _ _ _ _ _ ->
                    -- Poll for game state updates
                    handleRequestGame model

                Lobby key lobbyModel storedPlayers ->
                    -- Update current time and poll for lobby/game summaries updates
                    ( Lobby key { lobbyModel | currentTime = currentTime } storedPlayers
                    , Cmd.map GotServerMsg Api.getGameSummariesFromServer
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
    case model of
        Lobby key _ storedPlayers ->
            if String.isEmpty gameidStr then
                ( model, Cmd.none )

            else
                case String.toInt gameidStr of
                    Just gameid ->
                        -- Transition to EnterName (player names come via subscription)
                        ( EnterName key gameid (Entering "") storedPlayers
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

        EnterName key currentGameId _ storedPlayers ->
            -- Stay in EnterName if same game, otherwise redirect
            if String.toInt gameidStr == Just currentGameId then
                ( model, Cmd.none )

            else
                ( Redirect key url storedPlayers, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        Playing key _ _ _ _ _ storedPlayers ->
            ( Redirect key url storedPlayers, Cmd.map GotServerMsg Api.getGameSummariesFromServer )

        _ ->
            ( model, Cmd.none )



-- LINK CLICK HANDLERS


handleClickedLink : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
handleClickedLink urlRequest model =
    case model of
        Lobby key lobby storedPlayers ->
            case urlRequest of
                Browser.Internal url ->
                    ( Lobby key lobby storedPlayers, Nav.pushUrl key <| Url.toString url )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- GAME MESSAGE HANDLERS


handleGameMsg : Game.Msg -> Model -> ( Model, Cmd Msg )
handleGameMsg gamemsg model =
    case model of
        Playing key gameid name token game history_ storedPlayers ->
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
            ( Playing key gameid name token game_after history_ storedPlayers, cmd )

        _ ->
            ( model, Cmd.none )



-- LOBBY MESSAGE HANDLERS


handleLobbyMsg : Lobby.Msg -> Model -> ( Model, Cmd Msg )
handleLobbyMsg lobbymsg model =
    case ( lobbymsg, model ) of
        ( RequestNewGameFromServer, Lobby _ _ _ ) ->
            ( model, Cmd.map GotServerMsg Api.getGameIdFromServer )

        _ ->
            ( model, Cmd.none )



-- USER INPUT HANDLERS


handleTypingName : String -> Model -> ( Model, Cmd Msg )
handleTypingName newname model =
    case model of
        EnterName key gameid (Entering _) storedPlayers ->
            ( EnterName key gameid (Entering newname) storedPlayers, Cmd.none )

        EnterName key gameid (JoinError _ error) storedPlayers ->
            ( EnterName key gameid (JoinError newname error) storedPlayers, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleRequestGame : Model -> ( Model, Cmd Msg )
handleRequestGame model =
    case model of
        EnterName key gameid (Entering name) storedPlayers ->
            let
                -- Look up token from stored players list
                maybeToken =
                    storedPlayers
                        |> List.filter (\p -> p.playerName == name)
                        |> List.head
                        |> Maybe.map .token
            in
            ( EnterName key gameid (Joining name) storedPlayers
            , Cmd.map GotServerMsg <| Api.joinGame gameid name maybeToken
            )

        EnterName key gameid (JoinError name _) storedPlayers ->
            -- Retry after error, look up token from stored players
            let
                maybeToken =
                    storedPlayers
                        |> List.filter (\p -> p.playerName == name)
                        |> List.head
                        |> Maybe.map .token
            in
            ( EnterName key gameid (Joining name) storedPlayers
            , Cmd.map GotServerMsg <| Api.joinGame gameid name maybeToken
            )

        Playing _ gameid _ _ _ _ _ ->
            ( model, Cmd.map GotServerMsg <| Api.getGameFromServer gameid )

        _ ->
            ( model, Cmd.none )



-- PLAYER IDENTITY HANDLERS


handleStoredPlayersLoaded : Encode.Value -> Model -> ( Model, Cmd Msg )
handleStoredPlayersLoaded value model =
    let
        players =
            decodePlayers value
    in
    case model of
        Redirect key url _ ->
            ( Redirect key url players, Cmd.none )

        Lobby key lobby _ ->
            ( Lobby key lobby players, Cmd.none )

        EnterName key gameid (Entering currentName) _ ->
            ( EnterName key gameid (Entering currentName) players, Cmd.none )

        EnterName key gameid (Joining name) _ ->
            ( EnterName key gameid (Joining name) players, Cmd.none )

        EnterName key gameid (JoinError name errorMsg) _ ->
            ( EnterName key gameid (JoinError name errorMsg) players, Cmd.none )

        Playing key gameid name token game history _ ->
            ( Playing key gameid name token game history players, Cmd.none )



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

        ReceivedConcedeResponse result ->
            handleConcedeResponse result model


handleGameSummaries : Result Http.Error (List Lobby.GameSummary) -> Model -> ( Model, Cmd Msg )
handleGameSummaries result model =
    case ( result, model ) of
        ( Ok summaries, Redirect key url storedPlayers ) ->
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
                        ( EnterName key gameid (Entering "") storedPlayers
                        , Cmd.none
                        )

                    else
                        ( Lobby key { status = Home summaries, currentTime = Time.millisToPosix 0 } storedPlayers, Nav.pushUrl key "/" )

                Nothing ->
                    if String.isEmpty gameidStr then
                        ( Lobby key { status = Home summaries, currentTime = Time.millisToPosix 0 } storedPlayers, Cmd.none )

                    else
                        ( Lobby key { status = Home summaries, currentTime = Time.millisToPosix 0 } storedPlayers, Nav.pushUrl key "/" )

        ( Err _, Redirect key _ storedPlayers ) ->
            ( Lobby key { status = Home [], currentTime = Time.millisToPosix 0 } storedPlayers, Cmd.none )

        ( Ok summaries, Lobby key lobby storedPlayers ) ->
            -- Update lobby with fresh game summaries (from polling)
            ( Lobby key { lobby | status = Home summaries } storedPlayers, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleNewGameId : Result Http.Error GameId -> Model -> ( Model, Cmd Msg )
handleNewGameId result model =
    case ( result, model ) of
        ( Ok gameId, Lobby key lobby storedPlayers ) ->
            ( Lobby key lobby storedPlayers, Nav.pushUrl key <| "/" ++ String.fromInt gameId )

        _ ->
            ( model, Cmd.none )


handleJoinResponse : Result Http.Error (Result Api.JoinError Api.JoinGameResponse) -> Model -> ( Model, Cmd Msg )
handleJoinResponse result model =
    case ( result, model ) of
        ( Ok (Ok joinResponse), EnterName key gameid _ storedPlayers ) ->
            -- Success: save token and transition to Playing
            -- Server explicitly tells us which player we are
            joinGameSuccess key gameid joinResponse storedPlayers

        ( Ok (Err joinError), EnterName key gameid (Joining name) storedPlayers ) ->
            -- Join error from server: show error in EnterName screen
            ( EnterName key gameid (JoinError name (Api.joinErrorToString joinError)) storedPlayers
            , Cmd.none
            )

        ( Err httpError, EnterName key gameid (Joining name) storedPlayers ) ->
            -- Network error: show error in EnterName screen
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

                        Http.BadBody msg ->
                            "Invalid response: " ++ msg
            in
            ( EnterName key gameid (JoinError name errorMsg) storedPlayers
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


joinGameSuccess : Key -> GameId -> Api.JoinGameResponse -> List PlayerIdentity -> ( Model, Cmd Msg )
joinGameSuccess key gameid joinResponse storedPlayers =
    let
        servergame =
            joinResponse.responseGame

        token =
            joinResponse.responseToken

        name =
            joinResponse.responsePlayerName

        finalgame =
            buildGame name servergame

        -- IMPORTANT: Always use token from server response
        -- Server is source of truth
        newPlayer =
            { playerName = name
            , token = token
            }

        saveCmd =
            if not (String.isEmpty name) then
                Ports.savePlayer (encodePlayer newPlayer)

            else
                Cmd.none

        concedeCmd =
            checkAndConcede finalgame gameid token
    in
    ( Playing key gameid name token finalgame servergame.gameHistory storedPlayers
    , Cmd.batch [ saveCmd, concedeCmd ]
    )


handleGameUpdate : Result Http.Error Api.ServerGame -> Model -> ( Model, Cmd Msg )
handleGameUpdate result model =
    case ( result, model ) of
        ( Ok servergame, Playing key gameid name token game currentHistory storedPlayers ) ->
            case game.state of
                GameOver _ ->
                    ( Playing key gameid name token game servergame.gameHistory storedPlayers, Cmd.none )

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
                                    ( Playing key gameid name token updatedGame servergame.gameHistory storedPlayers
                                    , concedeCmd
                                    )
                                )
                            |> Maybe.withDefault ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleMoveConfirmation : Result Http.Error (Result Api.MoveError Game.GameMove) -> Model -> ( Model, Cmd Msg )
handleMoveConfirmation result model =
    case ( result, model ) of
        ( Ok (Ok gameMove), Playing key gameid name token game history_ storedPlayers ) ->
            let
                updatedGame =
                    game |> Game.update (NewGameMove <| transformGameMove gameMove)

                concedeCmd =
                    checkAndConcede updatedGame gameid token
            in
            ( Playing key gameid name token updatedGame (gameMove :: history_) storedPlayers
            , concedeCmd
            )

        ( Ok (Err _), Playing _ _ _ _ _ _ _ ) ->
            -- Move was rejected by server, but we don't need to do anything
            -- The game state remains unchanged
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleConcedeResponse : Result Http.Error (Result Api.ConcedeError Color) -> Model -> ( Model, Cmd Msg )
handleConcedeResponse result model =
    case result of
        Ok (Ok _) ->
            -- Concede was successful, no action needed (will be reflected in next game update)
            ( model, Cmd.none )

        Ok (Err _) ->
            -- Concede failed, but we don't need to do anything
            ( model, Cmd.none )

        Err _ ->
            -- Network error
            ( model, Cmd.none )
