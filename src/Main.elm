module Main exposing (main)

import Browser
import Game.Card exposing (Card)
import Game.Figure exposing (Color(..), colorToString)
import Game.Game as Game exposing (Game, GameMove, Msg(..), rotateCardMove)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode



--import TimeTravel.Browser as TimeTravel exposing (defaultConfig)
-- MODEL


type alias Model =
    { game : Game
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
                        [ Html.button [ onClick RequestHistoryFromServer ]
                            [ Html.text "Update (Poll Server)" ]
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
            (List.map viewGameMove history)
        , Html.input [ HtmlA.id "chat-box", HtmlA.type_ "text" ] []
        ]


viewGameMove : GameMove -> Html Msg
viewGameMove gameMove =
    let
        ( from_x, from_y ) =
            gameMove.from

        ( move_x, move_y ) =
            if gameMove.color == White then
                gameMove.move

            else
                gameMove |> rotateCardMove |> .move

        from =
            String.fromInt from_x ++ " , " ++ String.fromInt from_y

        to =
            String.fromInt (from_x + move_x) ++ " , " ++ String.fromInt (from_y + move_y)
    in
    Html.li [ HtmlA.class "log-message" ]
        [ Html.text (colorToString gameMove.color ++ " moved from ( " ++ from ++ " ) to ( " ++ to ++ " ) by playing the " ++ gameMove.card.name ++ " card.") ]



-- UPDATE


type Msg
    = GameMsg Game.Msg
    | RequestHistoryFromServer
    | ReceivedHistoryFromServer (Result Http.Error (List Game.GameMove))
    | ReceivedCardsFromServer (Result Http.Error (List String))
    | ReceivedPostCreatedFromServer (Result Http.Error Game.GameMove)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game } as model) =
    case msg of
        GameMsg (NewGameMove gameMove) ->
            let
                ( game_, msg_ ) =
                    Game.update (NewGameMove gameMove) game
            in
            ( { model | game = game_ }
            , Cmd.batch [ Cmd.map GameMsg msg_, postNewGameMove gameMove ]
            )

        GameMsg gamemsg ->
            let
                ( game_, msg_ ) =
                    Game.update gamemsg game
            in
            ( { model | game = game_ }
            , Cmd.map GameMsg msg_
            )

        ReceivedCardsFromServer (Ok cards) ->
            ( { model | cards = cards |> List.map Game.Card.cardByName }
            , Game.send RequestHistoryFromServer
            )

        ReceivedCardsFromServer (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        RequestHistoryFromServer ->
            ( model
            , getHistoryFromServer
            )

        ReceivedHistoryFromServer (Ok history_) ->
            let
                history =
                    List.reverse history_
            in
            case history of
                [] ->
                    ( { model
                        | game = Game.newCards model.cards game
                        , history = history
                        , errorMessage = Nothing
                      }
                    , Cmd.none
                    )

                gameMove :: [] ->
                    let
                        ( game_, msg_ ) =
                            Game.setupNewGame Black White
                                |> Game.newCards model.cards
                                |> Game.update (NewGameMove gameMove)
                    in
                    ( { model
                        | history = history
                        , errorMessage = Nothing
                        , game = game_
                      }
                    , Cmd.map GameMsg msg_
                    )

                gameMove :: pastHistory ->
                    if pastHistory == model.history then
                        let
                            ( game_, msg_ ) =
                                game
                                    |> Game.update (NewGameMove gameMove)
                        in
                        ( { model
                            | history = history
                            , errorMessage = Nothing
                            , game = game_
                          }
                        , Cmd.map GameMsg msg_
                        )

                    else
                        ( model, Cmd.none )

        ReceivedHistoryFromServer (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        ReceivedPostCreatedFromServer (Ok gameMove) ->
            ( { model | history = gameMove :: model.history }
            , Cmd.none
            )

        ReceivedPostCreatedFromServer (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )


getCardsFromServer : Cmd Msg
getCardsFromServer =
    Http.get
        { url = "http://localhost:5019/cards"
        , expect = Http.expectJson ReceivedCardsFromServer (Decode.list Decode.string)
        }


getHistoryFromServer : Cmd Msg
getHistoryFromServer =
    Http.get
        { url = "http://localhost:5019/history"
        , expect =
            Http.expectJson ReceivedHistoryFromServer (Decode.list decodeGameMove)
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


postNewGameMove : Game.GameMove -> Cmd Msg
postNewGameMove gameMove =
    Http.post
        { url = "http://localhost:5019/history"
        , body = Http.jsonBody (enecodergameMove gameMove)
        , expect = Http.expectJson ReceivedPostCreatedFromServer decodeGameMove
        }


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
      , cards = []
      , history = []
      , errorMessage = Nothing
      }
    , getCardsFromServer
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
