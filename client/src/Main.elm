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
import Task


{-
   import TimeTravel.Browser as TimeTravel exposing (defaultConfig)
-}
-- MODEL


type alias Model =
    { game : Game
    , gameid : Int
    , cards : List Card
    , history : List Game.GameMove
    , errorMessage : Maybe String
    }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Onitama"
        [ Html.div [ HtmlA.class "game-container", HtmlA.style "display" "flex", HtmlA.style "height" "100vh" ]
            ((model.game
                |> Game.view
                |> List.map (Html.map GameMsg)
             )
                ++ [ Html.div []
                        [ Html.button [ onClick RequestNewGameFromServer ]
                            [ Html.text "New Game" ]
                        , Html.input [ HtmlA.id "gameid", HtmlA.type_ "number", HtmlA.value <| String.fromInt model.gameid, onInput GameIdEntered ] []
                        , Html.button [ onClick JoinGame ]
                            [ Html.text "Join" ]
                        , Html.button [ onClick RequestGameFromServer ]
                            [ Html.text "Update" ]
                        , viewHistoryOrError model
                        ]
                   ]
            )
        ]


viewHistoryOrError : Model -> Html Msg
viewHistoryOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewHistory model.history


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
    | RequestNewGameFromServer
    | RequestGameFromServer
    | ReceivedGameIdFromServer (Result Http.Error Int)
    | ReceivedGameFromServer (Result Http.Error ( List Card, List Game.GameMove ))
    | ReceivedPostCreatedFromServer (Result Http.Error Game.GameMove)
    | JoinGame
    | GameIdEntered String



--    | WssIncome String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game } as model) =
    case msg of
        GameMsg gamemsg ->
            let
                game_ =
                    Game.update gamemsg game
            in
            ( { model | game = game_ }
            , case game_.state of
                MoveDone gameMove ->
                    postNewGameMove model.gameid <| transformGameMove gameMove

                _ ->
                    Cmd.none
            )

        ReceivedPostCreatedFromServer (Ok gameMove) ->
            ( { model
                | game = Game.update (NewGameMove <| transformGameMove gameMove) game
                , history = gameMove :: model.history
              }
            , Cmd.none
            )

        ReceivedPostCreatedFromServer (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        RequestNewGameFromServer ->
            ( model
            , getGameIdFromServer
            )

        ReceivedGameIdFromServer (Ok gameId) ->
            ( { model
                | gameid = gameId
              }
            , send RequestGameFromServer
            )

        ReceivedGameIdFromServer (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        RequestGameFromServer ->
            ( model
            , getGameFromServer model.gameid
            )

        ReceivedGameFromServer (Ok ( cards, history )) ->
            case history of
                [] ->
                    let
                        game_ =
                            Game.setupNewGame White White
                                |> Game.newCards cards
                    in
                    ( { model
                        | game = game_
                        , history = []
                        , errorMessage = Nothing
                      }
                    , Cmd.none
                    )

                gameMove :: [] ->
                    let
                        game_ =
                            Game.setupNewGame Black White
                                |> Game.newCards cards
                                |> Game.update (NewGameMove <| transformGameMove gameMove)
                    in
                    ( { model
                        | game = game_
                        , history = [ gameMove ]
                        , errorMessage = Nothing
                      }
                    , Cmd.none
                    )

                gameMove :: pastHistory ->
                    if pastHistory == model.history then
                        let
                            game_ =
                                game
                                    |> Game.update (NewGameMove <| transformGameMove gameMove)
                        in
                        ( { model
                            | game = game_
                            , history = history
                            , errorMessage = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        ReceivedGameFromServer (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        GameIdEntered gameId ->
            ( { model
                | gameid = Maybe.withDefault 0 <| String.toInt gameId
              }
            , Cmd.none
            )

        JoinGame ->
            ( model
            , send RequestGameFromServer
            )



{-
   WssIncome _ ->
       Debug.todo "branch 'Recv _' not implemented"
-}


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


transformGameMove : Game.GameMove -> Game.GameMove
transformGameMove g =
    case g.color of
        Black ->
            Game.rotateGameMove g

        White ->
            g


getGameIdFromServer : Cmd Msg
getGameIdFromServer =
    Http.post
        { url = "/game" --http://localhost:3000/game
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
    ( { game = Game.setupNewGame White White
      , gameid = 0
      , cards = []
      , history = []
      , errorMessage = Nothing
      }
    , Cmd.none
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
