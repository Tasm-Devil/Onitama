module Main exposing (main)

import Browser
import Game.Game as Game
import Html


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Running Game.Game
    | NotRunning


init : () -> ( Model, Cmd Msg )
init _ =
    ( Running Game.setupNewGame, Cmd.map GameMsg Game.startNewGame )



-- UPDATE


type Msg
    = GameMsg Game.Msg



--    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GameMsg msg_, Running game_ ) ->
            ( Running (Game.updateGame msg_ game_), Cmd.none )

        ( _, NotRunning ) ->
            ( Running Game.setupNewGame, Cmd.map GameMsg Game.startNewGame )



--        ( NewGame, Running _ ) ->
--            ( Running Game.gameSetup, Cmd.map GameMsg Game.startNewGame )
--        ( NewGame, NotRunning ) ->
--            ( NotRunning, Cmd.none )
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Onitama"
        (case model of
            NotRunning ->
                [ Html.div [] [] ]

            Running game ->
                List.map (Html.map GameMsg) (Game.view game)
        )
