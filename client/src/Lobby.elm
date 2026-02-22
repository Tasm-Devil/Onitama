module Lobby exposing (CardSet(..), GameStatus(..), GameSummary, GameType(..), Model, Msg(..), getCardSet, getGameType, getGames, init, update, updateGames, updateTime, view)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onCheck, onClick)
import Time exposing (Posix)


type CardSet
    = BaseOnly
    | WithExpansion



-- TYPES


type alias BrowsingData =
    { games : List GameSummary
    , currentTime : Posix
    }


type Model
    = Browsing BrowsingData
    | PickingCardSet GameType CardSet BrowsingData


type GameType
    = Multiplayer
    | VsAI


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


type Msg
    = ClickNewGame
    | ClickNewGameAI
    | ToggleExpansion Bool
    | ConfirmCreate
    | CancelCreate
    | ClickPlay Int



-- INIT


init : List GameSummary -> Model
init games =
    Browsing { games = games, currentTime = Time.millisToPosix 0 }



-- HELPERS


getBrowsingData : Model -> BrowsingData
getBrowsingData model =
    case model of
        Browsing data ->
            data

        PickingCardSet _ _ data ->
            data


getGames : Model -> List GameSummary
getGames model =
    (getBrowsingData model).games


getCardSet : Model -> Maybe CardSet
getCardSet model =
    case model of
        PickingCardSet _ cardSet _ ->
            Just cardSet

        _ ->
            Nothing


getGameType : Model -> Maybe GameType
getGameType model =
    case model of
        PickingCardSet gameType _ _ ->
            Just gameType

        _ ->
            Nothing


updateGames : List GameSummary -> Model -> Model
updateGames games model =
    let
        data =
            getBrowsingData model
    in
    case model of
        Browsing _ ->
            Browsing { data | games = games }

        PickingCardSet gameType cardSet _ ->
            PickingCardSet gameType cardSet { data | games = games }


updateTime : Posix -> Model -> Model
updateTime time model =
    let
        data =
            getBrowsingData model
    in
    case model of
        Browsing _ ->
            Browsing { data | currentTime = time }

        PickingCardSet gameType cardSet _ ->
            PickingCardSet gameType cardSet { data | currentTime = time }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        data =
            getBrowsingData model
    in
    case msg of
        ClickNewGame ->
            PickingCardSet Multiplayer BaseOnly data

        ClickNewGameAI ->
            PickingCardSet VsAI BaseOnly data

        ToggleExpansion checked ->
            case model of
                PickingCardSet gameType _ _ ->
                    PickingCardSet gameType
                        (if checked then
                            WithExpansion

                         else
                            BaseOnly
                        )
                        data

                _ ->
                    model

        ConfirmCreate ->
            model

        CancelCreate ->
            Browsing data

        ClickPlay _ ->
            model



-- VIEW


view : Maybe String -> Model -> Html Msg
view myName model =
    let
        data =
            getBrowsingData model
    in
    Html.div [ HtmlA.class "gamelist" ]
        [ Html.h1 []
            [ Html.text "ONITAMA" ]
        , Html.small [ HtmlA.class "lobby-description" ]
            [ Html.text "A two-player abstract board game. Play with friends or challenge the AI." ]
        , Html.small [ HtmlA.class "lobby-description" ]
            [ Html.text "Be sure to read "
            , Html.a [ HtmlA.href "https://www.arcanewonders.com/wp-content/uploads/2021/05/Onitama-Rulebook.pdf", HtmlA.target "_blank" ]
                [ Html.text "the rules" ]
            , Html.text " if you haven't played before."
            ]
        , case myName of
            Just _ ->
                Html.div [ HtmlA.class "lobby-actions" ]
                    [ Html.button [ HtmlA.class "new-game", onClick ClickNewGame ]
                        [ Html.text "New Game" ]
                    , Html.button [ HtmlA.class "new-game", onClick ClickNewGameAI ]
                        [ Html.text "Play vs AI" ]
                    ]

            Nothing ->
                Html.div [ HtmlA.class "lobby-actions" ]
                    [ Html.a [ HtmlA.class "new-game", HtmlA.href "/oauth2/sign_in" ]
                        [ Html.text "Login with GitHub" ]
                    ]
        , case model of
            PickingCardSet gameType cardSet _ ->
                viewCardSetPicker gameType cardSet

            _ ->
                Html.text ""
        , if List.isEmpty data.games then
            Html.div [ HtmlA.class "empty-state" ]
                [ Html.text "No games yet — start one!" ]

          else
            Html.table [ HtmlA.id "game-table" ]
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
                            [ Html.text "Action" ]
                        ]
                    ]
                    :: List.map (createGameTableRow myName data.currentTime) data.games
                )
        ]


viewCardSetPicker : GameType -> CardSet -> Html Msg
viewCardSetPicker gameType cardSet =
    let
        title =
            case gameType of
                Multiplayer ->
                    "New Game"

                VsAI ->
                    "Play vs AI"
    in
    Html.div [ HtmlA.class "card-set-picker" ]
        [ Html.h3 [] [ Html.text title ]
        , Html.label [ HtmlA.class "card-set-toggle" ]
            [ Html.input
                [ HtmlA.type_ "checkbox"
                , HtmlA.checked (cardSet == WithExpansion)
                , onCheck ToggleExpansion
                ]
                []
            , Html.text " Include Sensei's Path expansion cards"
            ]
        , Html.div [ HtmlA.class "card-set-picker-actions" ]
            [ Html.button [ HtmlA.class "new-game", onClick ConfirmCreate ]
                [ Html.text "Create" ]
            , Html.button [ HtmlA.class "new-game", onClick CancelCreate ]
                [ Html.text "Cancel" ]
            ]
        ]


createGameTableRow : Maybe String -> Posix -> GameSummary -> Html Msg
createGameTableRow myName currentTime summary =
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

        ( statusText, statusClass ) =
            case summary.summaryStatus of
                WaitingForPlayers ->
                    ( "Waiting", "status-waiting" )

                InProgress ->
                    ( "In progress", "status-active" )

                Completed ->
                    ( "Completed", "status-completed" )

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
            [ Html.span [ HtmlA.class ("status-badge " ++ statusClass) ]
                [ Html.text statusText ]
            ]
        , Html.td []
            [ Html.text createdAgo ]
        , Html.td []
            [ Html.text lastActivityAgo ]
        , Html.td []
            (let
                isMyGame =
                    case myName of
                        Just n ->
                            n == summary.summaryPlayer1 || n == summary.summaryPlayer2

                        Nothing ->
                            False

                gameLink label =
                    Html.a [ HtmlA.class "join-game", HtmlA.href (String.fromInt summary.summaryId) ]
                        [ Html.text label ]
             in
             case summary.summaryStatus of
                WaitingForPlayers ->
                    if myName == Nothing then
                        [ gameLink "Watch" ]

                    else
                        [ gameLink "Join" ]

                InProgress ->
                    if isMyGame then
                        [ gameLink "Play" ]

                    else
                        [ gameLink "Watch" ]

                Completed ->
                    [ gameLink "Review" ]
            )
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
