module Main exposing (main)

import Browser
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



-- MODEL


type alias GameId =
    Int


type alias Model =
    { state : AppState
    , errorMessage : Maybe String
    }


type alias Player =
    { name : String
    , color : Color
    }


type AppState
    = Lobby (List GameId)
    | EnterName Player GameId
    | Playing Player GameId Game (List Game.GameMove)



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
                            [ Html.input [ HtmlA.id "name", HtmlA.placeholder "Enter your name", HtmlA.value <| player.name, onInput NameEntered ] []
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
            [ Html.a [ HtmlA.class "join-game", HtmlA.href "#", onClick (JoinGame id) ]
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


type Msg
    = GameMsg Game.Msg
    | RequestNewGameFromServer -- player white
    | JoinGame GameId -- player black
    | NameEntered String
    | RequestGameFromServer -- get geme by id
    | ReceivedGameIdsFromServer (Result Http.Error (List Int)) -- all current game ids
    | ReceivedGameIdFromServer (Result Http.Error Int) -- the game id of the new game
    | ReceivedGameFromServer (Result Http.Error ( List Card, List Game.GameMove ))
    | ReceivedPostCreatedFromServer (Result Http.Error Game.GameMove)



--  | WssIncome String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state } as model) =
    case state of
        Lobby _ ->
            case msg of
                ReceivedGameIdsFromServer (Ok gamesids) ->
                    ( { model | state = Lobby gamesids }, Cmd.none )

                ReceivedGameIdsFromServer (Err httpError) ->
                    ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

                RequestNewGameFromServer ->
                    ( model
                    , getGameIdFromServer
                    )

                JoinGame gameId ->
                    ( { model | state = EnterName { name = "Bob", color = Black } gameId }
                    , Cmd.none
                    )

                ReceivedGameIdFromServer (Ok gameId) ->
                    ( { model | state = EnterName { name = "Wendy", color = White } gameId }
                    , Cmd.none
                    )

                ReceivedGameIdFromServer (Err httpError) ->
                    ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EnterName ({ color } as player) gameid ->
            case msg of
                NameEntered newname ->
                    ( { model
                        | state = EnterName { player | name = newname } gameid
                      }
                    , Cmd.none
                    )

                RequestGameFromServer ->
                    ( model, getGameFromServer gameid )

                ReceivedGameFromServer (Ok ( cards, history )) ->
                    let
                        newgame =
                            Game.setupNewGame cards color White 

                        -- ToDo: White is not always the first player!
                        finalgame =
                            List.foldr (\gameMove -> Game.update (NewGameMove <| transformGameMove gameMove)) newgame history
                    in
                    ( { model
                        | state = Playing player gameid finalgame history
                        , errorMessage = Nothing
                      }
                    , Cmd.none
                    )

                ReceivedGameFromServer (Err httpError) ->
                    ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Playing player gameid game history ->
            case msg of
                GameMsg gamemsg ->
                    let
                        game_after =
                            Game.update gamemsg game
                    in
                    ( { model | state = Playing player gameid game_after history }
                    , case game_after.state of
                        MoveDone gameMove ->
                            postNewGameMove gameid <| transformGameMove gameMove

                        _ ->
                            Cmd.none
                    )

                ReceivedGameFromServer (Ok ( _, gameMove :: _ )) ->
                    ( { model
                        | state =
                            Playing player
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

                ReceivedPostCreatedFromServer (Ok gameMove) ->
                    ( { model
                        | state = Playing player gameid (game |> Game.update (NewGameMove <| transformGameMove gameMove)) (gameMove :: history)
                      }
                    , Cmd.none
                    )

                RequestGameFromServer ->
                    ( model, getGameFromServer gameid )

                ReceivedPostCreatedFromServer (Err httpError) ->
                    ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

                ReceivedGameFromServer (Err httpError) ->
                    ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



{-
      WssIncome _ ->
          Debug.todo "branch 'Recv _' not implemented"

   send : msg -> Cmd msg
   send msg =
       Task.succeed msg
           |> Task.perform identity
-}


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
        , expect = Http.expectJson ReceivedGameIdsFromServer (Decode.list Decode.int)
        }


getGameIdFromServer : Cmd Msg
getGameIdFromServer =
    Http.post
        { url = "/game"
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedGameIdFromServer Decode.int
        }


getGameFromServer : Int -> Cmd Msg
getGameFromServer gameid =
    Http.get
        { url = "/game/" ++ String.fromInt gameid
        , expect =
            Http.expectJson ReceivedGameFromServer decodeGame
        }


postNewGameMove : Int -> Game.GameMove -> Cmd Msg
postNewGameMove gameid gameMove =
    Http.post
        { url = "/game/" ++ String.fromInt gameid
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


decodeGame : Decoder ( List Card, List Game.GameMove )
decodeGame =
    Decode.map2 Tuple.pair
        (Decode.field "cards" (Decode.list (Decode.map Game.Card.cardByName Decode.string)))
        (Decode.field "history" (Decode.list decodeGameMove))


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = Lobby []
      , errorMessage = Nothing
      }
    , getGamesIdsFromServer
    )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



{-
   main =
       TimeTravel.document Debug.toString
           Debug.toString
           defaultConfig
           { init = init
           , view = view
           , update = update
           , subscriptions = \_ -> Sub.none
           }
-}
{-
   -- PORTS
   port sendMessage : String -> Cmd msg
   port messageReceiver : (String -> msg) -> Sub msg

   -- SUBSCRIPTIONS
   -- Subscribe to the `messageReceiver` port to hear about messages coming in
   -- from JS. Check out the index.html file to see how this is hooked up to a
   -- WebSocket.
   --
   subscriptions : Model -> Sub Msg
   subscriptions _ =
     messageReceiver WssIncome
-}
