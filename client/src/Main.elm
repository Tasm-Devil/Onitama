module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Game.Card exposing (Card)
import Game.Figure exposing (Color(..), colorToString)
import Game.Game as Game exposing (Game, GameMove, GameState(..), Msg(..))
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url exposing (Url)


-- MODEL


type alias GameId =
    String


type alias PlayerName =
    String


type AppState
    = Lobby (List GameId)
    | EnterName PlayerName GameId
    | Playing PlayerName GameId Game (List Game.GameMove)


type alias Model =
    { state : AppState
    , url : Url
    , navkey : Nav.Key
    , errorMessage : Maybe String
    }



-- VIEW


view : Model -> Browser.Document Msg
view ({ state } as model) =
    Browser.Document
        "Onitama"
        [ case state of
            Lobby ids ->
                Html.div [ HtmlA.class "lobby" ]
                    [ Html.h1 []
                        [ Html.text "ONITAMA" ]
                    , Html.p []
                        [ Html.text "Onitama is a two player abstract board game. You can play it online here! Below is a list of in-progress games. To start a new game click the \"new game\" button and distribute the url to another player to join." ]
                    , Html.p []
                        [ Html.text "Be sure to read "
                        , Html.a [ HtmlA.href "https://www.arcanewonders.com/wp-content/uploads/2021/05/Onitama-Rulebook.pdf" ]
                            [ Html.text "the rules" ]
                        , Html.text "if you haven't played before."
                        ]
                    , Html.a [ HtmlA.class "new-game", onClick RequestNewGameFromServer ]
                        [ Html.text "New Game" ]
                    , Html.table [ HtmlA.id "game-table" ]
                        (Html.thead []
                            [ Html.tr []
                                [ Html.td []
                                    [ Html.text "Game" ]
                                , Html.td []
                                    [ Html.text "Last Action" ]
                                , Html.td []
                                    [ Html.text "State" ]
                                , Html.td []
                                    [ Html.text "Spectators" ]
                                , Html.td []
                                    []
                                ]
                            ]
                            :: List.map createGameTableRow ids
                        )
                    ]

            EnterName player _ ->
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

                        {-
                           , Html.div [ HtmlA.class "rejoin", HtmlA.attribute "style" "display: block" ]
                               [ Html.span [] [ Html.text "You appear to have a rejoin code for this room. Click the button below if you want to rejoin as the player you were previously.          " ]
                               , Html.button [ HtmlA.id "rejoin-button", HtmlA.type_ "button" ] [ Html.text "Rejoin" ]
                               ]
                        -}
                        ]
                    ]

            Playing _ _ game _ ->
                Html.div [ HtmlA.class "game-container", HtmlA.style "display" "flex" ]
                    ((game
                        |> Game.view
                        |> List.map (Html.map GameMsg)
                     )
                        ++ [ Html.div []
                                [ Html.input [ HtmlA.type_ "button", HtmlA.value "Update", onClick RequestGameFromServer ] []
                                , viewHistoryOrError model
                                ]
                           ]
                    )
        ]


createGameTableRow : GameId -> Html Msg
createGameTableRow id =
    Html.tr [ HtmlA.class "game-row" ]
        [ Html.td []
            [ Html.text "Wendy vs. Bob" ]
        , Html.td []
            [ Html.text "N/A" ]
        , Html.td []
            [ Html.text "done, WHITE won" ]
        , Html.td []
            [ Html.text "0" ]
        , Html.td []
            [ Html.a [ HtmlA.class "join-game", HtmlA.href id ]
                [ Html.text "Join" ]
            ]
        ]


viewHistoryOrError : Model -> Html Msg
viewHistoryOrError ({ state } as model) =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            case state of
                Playing _ _ _ history ->
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
        [ Html.text (colorToString gameMove.color ++ " moved from " ++ from ++ " to " ++ to ++ " by playing the " ++ gameMove.card.name ++ " card.") ]



-- UPDATE


type alias ServerGame =
    { player_white : String
    , player_black : String
    , cards : List Card
    , history : List Game.GameMove
    }


type Msg
    = GameMsg Game.Msg
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | RequestNewGameFromServer
    | TypingName String
    | RequestGameFromServer
    | ReceivedGameIdsFromServer (Result Http.Error (List GameId)) -- all current game ids
    | ReceivedGameIdFromServer (Result Http.Error GameId) -- the game id of the new game
    | ReceivedGameFromServer (Result Http.Error ServerGame)
    | ReceivedPostCreatedFromServer (Result Http.Error Game.GameMove)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state } as model) =
    case ( state, msg ) of
        ( Lobby gamesids, ChangedUrl url ) ->
            let gameid = String.dropLeft 1 url.path
            in
            if List.member gameid gamesids then
                -- this never happens
                ( { model | state = EnterName "Charlie" gameid }, Cmd.none )
            else
                ( model , Cmd.none )
        ( _ , ChangedUrl url ) ->
            let gameid = String.dropLeft 1 url.path
            in
            if String.isEmpty gameid then
                ( { model | state = Lobby [] }, getGamesIdsFromServer )
            else
                ( model , Cmd.none )

        ( Lobby _, ReceivedGameIdsFromServer (Ok gamesids) ) ->
            let gameid = String.dropLeft 1 model.url.path
            in
            if List.member gameid gamesids then
                ( { model | state = EnterName "Alice" gameid }, Cmd.none )

            else if String.isEmpty gameid then
                ( { model | state = Lobby gamesids }, Cmd.none )

            else
                ( { model | state = Lobby gamesids }, Nav.pushUrl model.navkey <| "/" )

        ( Lobby _, RequestNewGameFromServer ) ->
            ( model
            , getGameIdFromServer
            )

        ( Lobby _, ClickedLink urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | state = EnterName "Bob" <| String.dropLeft 1 url.path }
                    , Nav.pushUrl model.navkey <| Url.toString url
                    )

                _ ->
                    ( model, Cmd.none )

        ( Lobby _, ReceivedGameIdFromServer (Ok gameId) ) ->
            ( { model | state = EnterName "Wendy" gameId }
            , Nav.pushUrl model.navkey <| "/" ++ gameId
            )

        ( EnterName _ gameid, TypingName newname ) ->
            ( { model | state = EnterName newname gameid } , Cmd.none  )

        ( EnterName name gameid, RequestGameFromServer ) ->
            ( model, joinGame gameid name )

        ( EnterName name gameid, ReceivedGameFromServer (Ok { player_white, player_black, cards, history }) ) ->
            let newgame = Game.setupNewGame cards (if name == player_black then Black else White ) White
                -- ToDo: White is not always the first player!
                finalgame =
                    List.foldr (\gameMove -> Game.update (NewGameMove <| transformGameMove gameMove)) newgame history
            in
            ( { model
                | state = Playing name gameid finalgame history
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        ( Playing name gameid game history_, GameMsg gamemsg ) ->
            let
                game_after =
                    Game.update gamemsg game
            in
            ( { model | state = Playing name gameid game_after history_ }
            , case game_after.state of
                MoveDone gameMove ->
                    postNewGameMove gameid <| transformGameMove gameMove

                _ ->
                    Cmd.none
            )

        ( Playing name gameid game _, ReceivedGameFromServer (Ok { player_white, player_black, cards, history }) ) ->
            Maybe.withDefault ( model, Cmd.none ) <|
                Maybe.map
                    (\gameMove ->
                        ( { model
                            | state =
                                Playing name
                                    gameid
                                    (game |> Game.update (NewGameMove <| transformGameMove gameMove))
                                    (if List.head history /= Just gameMove then
                                        gameMove :: history

                                     else
                                        history
                                    )
                          }
                        , Cmd.none
                        )
                    )
                    (List.head history)

        ( Playing name gameid game history_, ReceivedPostCreatedFromServer (Ok gameMove) ) ->
            ( { model
                | state = Playing name gameid (game |> Game.update (NewGameMove <| transformGameMove gameMove)) (gameMove :: history_)
              }
            , Cmd.none
            )

        ( Playing _ gameid _ _, RequestGameFromServer ) ->
            ( model, getGameFromServer gameid )

        ( Lobby _, ReceivedGameIdsFromServer (Err httpError) ) ->
            ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

        ( Lobby _, ReceivedGameIdFromServer (Err httpError) ) ->
            ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

        ( _ , ReceivedGameFromServer (Err httpError) ) ->
            ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

        ( Playing _ _ _ _, ReceivedPostCreatedFromServer (Err httpError) ) ->
            ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


transformGameMove : Game.GameMove -> Game.GameMove
transformGameMove g =
    case g.color of
        Black ->
            Game.rotateGameMove g

        White ->
            g


getGamesIdsFromServer : Cmd Msg
getGamesIdsFromServer =
    Http.get
        { url = "/game"
        , expect = Http.expectJson ReceivedGameIdsFromServer (Decode.list Decode.string)
        }


getGameIdFromServer : Cmd Msg
getGameIdFromServer =
    Http.post
        { url = "/game"
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedGameIdFromServer Decode.string
        }


joinGame : GameId -> String -> Cmd Msg
joinGame gameid name =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/game/" ++ gameid ++ "?name=" ++ name
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedGameFromServer decodeGame
        , timeout = Nothing
        , tracker = Nothing
        }


getGameFromServer : GameId -> Cmd Msg
getGameFromServer gameid =
    Http.get
        { url = "/game/" ++ gameid
        , expect =
            Http.expectJson ReceivedGameFromServer decodeGame
        }


postNewGameMove : GameId -> Game.GameMove -> Cmd Msg
postNewGameMove gameid gameMove =
    Http.post
        { url = "/game/" ++ gameid
        , body = Http.jsonBody (enecodergameMove gameMove)
        , expect = Http.expectJson ReceivedPostCreatedFromServer decodeGameMove
        }


decodeTuple : Decoder ( Int, Int )
decodeTuple =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)


decodeGameMove : Decoder Game.GameMove
decodeGameMove =
    Decode.succeed Game.GameMove
        |> required "color" (Decode.map Game.Figure.colorFromString Decode.string)
        |> required "card" (Decode.map Game.Card.cardByName Decode.string)
        |> required "from" decodeTuple
        |> required "move" decodeTuple


decodeGame : Decoder ServerGame
decodeGame =
    Decode.succeed ServerGame
        |> required "player_white" Decode.string
        |> required "player_black" Decode.string
        |> required "cards" (Decode.list (Decode.map Game.Card.cardByName Decode.string))
        |> required "history" (Decode.list decodeGameMove)


enecodergameMove : Game.GameMove -> Encode.Value
enecodergameMove gameMove =
    Encode.object
        [ ( "color", Encode.string (Game.Figure.colorToString gameMove.color) )
        , ( "card", Encode.string gameMove.card.name )
        , ( "from", Encode.list Encode.int [ Tuple.first gameMove.from, Tuple.second gameMove.from ] )
        , ( "move", Encode.list Encode.int [ Tuple.first gameMove.move, Tuple.second gameMove.move ] )
        ]


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



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { state = Lobby []
      , url = url
      , navkey = key
      , errorMessage = Nothing
      }
    , getGamesIdsFromServer
    )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
