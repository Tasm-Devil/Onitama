module Lobby exposing (GameId, Model, Msg(..), Status(..), view)

import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick)



-- TYPES


type alias GameId =
    String


type alias Model =
    { key : Key
    , status : Status
    }


type Status
    = Home (List GameId)



-- ToDo: Refactor this in the model
-- VIEW


view : Model -> Html Msg
view model =
    case model.status of
        Home ids ->
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


type Msg
    = RequestNewGameFromServer -- get rid of this using href /newgame
