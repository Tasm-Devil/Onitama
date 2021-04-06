module Main exposing (main)

import Browser
import Game.Game as Game
import Html
import Html.Attributes as HtmlA



-- MODEL


type alias Model =
    { game : Game.Game
    , errorMessage : Maybe String
    }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Onitama"
        [ Html.div [ HtmlA.style "display" "flex", HtmlA.style "height" "100vh", HtmlA.style "flex-direction" "column", HtmlA.style "padding" "8px", HtmlA.style "box-sizing" "border-box" ]
            (model.game
                |> Game.view
                |> List.map (Html.map GameMsg)
            )
        ]



-- UPDATE


type Msg
    = GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game } as model) =
    case msg of
        GameMsg gamemsg ->
            ( { model | game = Game.update gamemsg game }
            , Cmd.none
            )



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Game.setupNewGame
      , errorMessage = Nothing
      }
    , Cmd.map GameMsg Game.giveNewCards
    )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
