module Lobby exposing (GameId, Model, Msg(..), Status(..), GameSummary, GameStatus(..), view)

import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick)



-- TYPES


type alias GameId =
    Int


type alias Model =
    { status : Status
    }


type Status
    = Home (List GameSummary)

type GameStatus
    = WaitingForPlayers
    | InProgress
    | Completed


type alias GameSummary =
    { summaryId : GameId
    , summaryPlayer1 : String
    , summaryPlayer2 : String
    , summaryMoveCount : Int
    , summaryStatus : GameStatus
    }


-- ToDo: Refactor this in the model
-- VIEW


view : Model -> Html Msg
view model =
    case model.status of
        Home summaries ->
            Html.div [ HtmlA.class "lobby" ]
                [ Html.h1 []
                    [ Html.text "ONITAMA" ]
                , Html.p []
                    [ Html.text "Onitama is a two player abstract board game. You can play it online here! Below is a list of in-progress games. To start a new game click the \"new game\" button and distribute the url to another player to join." ]
                , Html.p []
                    [ Html.text "Be sure to read "
                    , Html.a [ HtmlA.href "https://www.arcanewonders.com/wp-content/uploads/2021/05/Onitama-Rulebook.pdf" ]
                        [ Html.text "the rules" ]
                    , Html.text " if you haven't played before."
                    ]
                , Html.a [ HtmlA.class "new-game", onClick RequestNewGameFromServer ]
                    [ Html.text "New Game" ]
                , Html.table [ HtmlA.id "game-table" ]
                    (Html.thead []
                        [ Html.tr []
                            [ Html.td []
                                [ Html.text "Game" ]
                            , Html.td []
                                [ Html.text "Moves" ]
                            , Html.td []
                                [ Html.text "State" ]
                            , Html.td []
                                [ Html.text "" ]
                            ]
                        ]
                        :: List.map createGameTableRow summaries
                    )
                ]


createGameTableRow : GameSummary -> Html Msg
createGameTableRow summary =
    let
        player1Display =
            if String.isEmpty summary.summaryPlayer1 then
                "(waiting)"
            else
                summary.summaryPlayer1

        player2Display =
            if String.isEmpty summary.summaryPlayer2 then
                "(waiting)"
            else
                summary.summaryPlayer2

        gameDisplay =
            player1Display ++ " vs. " ++ player2Display

        statusDisplay =
            case summary.summaryStatus of
                WaitingForPlayers ->
                    "Waiting for players"

                InProgress ->
                    "In progress"

                Completed ->
                    "Completed"

        movesDisplay =
            String.fromInt summary.summaryMoveCount
    in
    Html.tr [ HtmlA.class "game-row" ]
        [ Html.td []
            [ Html.text gameDisplay ]
        , Html.td []
            [ Html.text movesDisplay ]
        , Html.td []
            [ Html.text statusDisplay ]
        , Html.td []
            [ Html.a [ HtmlA.class "join-game", HtmlA.href (String.fromInt summary.summaryId) ]
                [ Html.text "Join" ]
            ]
        ]





type Msg
    = RequestNewGameFromServer -- get rid of this using href /newgame
