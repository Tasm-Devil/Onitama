module Lobby exposing (GameStatus(..), GameSummary, Model, view)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Time exposing (Posix)



-- TYPES


type alias Model =
    { games : List GameSummary
    , currentTime : Posix
    }


type GameStatus
    = WaitingForPlayers
    | InProgress
    | Completed


type alias GameSummary =
    { summaryId : Int
    , summaryPlayer1 : String
    , summaryPlayer2 : String
    , summaryMoveCount : Int
    , summaryStatus : GameStatus
    , summaryCreatedAt : Posix
    , summaryLastActivity : Posix
    }



-- VIEW


view : Model -> Html msg
view model =
    Html.div [ HtmlA.class "gamelist" ]
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
        , Html.a [ HtmlA.class "new-game", HtmlA.href "/newgame" ]
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
                        [ Html.text "Created" ]
                    , Html.td []
                        [ Html.text "Last Activity" ]
                    , Html.td []
                        [ Html.text "" ]
                    ]
                ]
                :: List.map (createGameTableRow model.currentTime) model.games
            )
        ]


createGameTableRow : Posix -> GameSummary -> Html msg
createGameTableRow currentTime summary =
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

        createdAgo =
            formatRelativeTime currentTime summary.summaryCreatedAt

        lastActivityAgo =
            formatRelativeTime currentTime summary.summaryLastActivity
    in
    Html.tr [ HtmlA.class "game-row" ]
        [ Html.td []
            [ Html.text gameDisplay ]
        , Html.td []
            [ Html.text movesDisplay ]
        , Html.td []
            [ Html.text statusDisplay ]
        , Html.td []
            [ Html.text createdAgo ]
        , Html.td []
            [ Html.text lastActivityAgo ]
        , Html.td []
            [ Html.a [ HtmlA.class "join-game", HtmlA.href (String.fromInt summary.summaryId) ]
                [ Html.text "Join" ]
            ]
        ]



-- Format time relative to current time (e.g., "5 minutes ago")


formatRelativeTime : Posix -> Posix -> String
formatRelativeTime current past =
    let
        diffMs =
            Time.posixToMillis current - Time.posixToMillis past

        diffSeconds =
            diffMs // 1000

        diffMinutes =
            diffSeconds // 60

        diffHours =
            diffMinutes // 60

        diffDays =
            diffHours // 24
    in
    if diffSeconds < 60 then
        "just now"

    else if diffMinutes < 60 then
        String.fromInt diffMinutes ++ " min ago"

    else if diffHours < 24 then
        String.fromInt diffHours ++ " hr ago"

    else
        String.fromInt diffDays ++ " days ago"
