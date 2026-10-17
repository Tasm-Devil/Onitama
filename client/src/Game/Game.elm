module Game.Game exposing (..)

import Game.Card exposing (..)
import Game.Cell exposing (..)
import Game.Figure exposing (..)
import Global exposing (gridsize)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events
import List.Extra
import Svg
import Svg.Attributes as SvgA



-- MODEL


type GameState
    = WaitingForOpponent
    | Thinking
    | FigureSelected ( Int, Int )
    | ChosingCard ( ( Int, Int ), ( Int, Int ) )
    | MoveDone GameMove
    | GameOver Color


type alias Game =
    { myColor : Color
    , nextColor : Color
    , myFigures : List Figure
    , opFigures : List Figure
    , myCards : ( Card, Card )
    , opCards : ( Card, Card )
    , commonCard : Card
    , state : GameState
    , spectating : Bool
    , hoveredMove : Maybe GameMove
    }


{-| GameMove

    - `Step from move`: `from` is the start position and is always seen from the perspective of the grid. Bottom left cell is (0,0) for white and black.
      `move` is independent from the player color. See it from the perspectiv of a non rotated card
    - `Pass`: the player has no legal step and only exchanges the card with the common card.

-}
type alias GameMove =
    { color : Color
    , card : Card
    , action : MoveAction
    }


type MoveAction
    = Step ( Int, Int ) ( Int, Int )
    | Pass


type LogEntry
    = MoveEntry GameMove
    | SystemEntry String



-- VIEW


view : Game -> List (Html Msg)
view game =
    [ Svg.svg [ SvgA.id "game-board", SvgA.viewBox "-1 -1 147 152" ]
        [ predefinedSymbols
        , Svg.g [ SvgA.transform <| "translate(0, " ++ (String.fromInt <| gridsize + 5) ++ ")" ]
            [ Svg.g [ SvgA.class "grid-lines" ] <|
                -- the grid is drawn here
                List.map (Game.Cell.draw NormalCell UserPressedOnCell UserReleasedOnCell) grid
                    ++ (case game.state of
                            FigureSelected ( u, v ) ->
                                -- draw the highlighted cells for next possible moves
                                List.append [ Game.Cell.draw MoveToCell UserPressedOnCell UserReleasedOnCell ( u, v ) ]
                                    ((List.Extra.unique ((Tuple.first game.myCards).moves ++ (Tuple.second game.myCards).moves)
                                        |> List.map (\( x, y ) -> ( x + u, y + v ))
                                        |> List.filter (\move -> grid |> List.member move)
                                        |> List.Extra.filterNot
                                            (\move ->
                                                game.myFigures
                                                    |> figurePositions
                                                    |> List.member move
                                            )
                                     )
                                        |> List.map (Game.Cell.draw SelectedCell UserPressedOnCell UserReleasedOnCell)
                                    )

                            _ ->
                                []
                       )
                    ++ (case game.hoveredMove |> Maybe.map .action of
                            Just (Step hmFrom hmMove) ->
                                let
                                    from =
                                        adjustPerspective game.myColor hmFrom

                                    to =
                                        adjustPerspective game.myColor
                                            ( Tuple.first hmFrom + Tuple.first hmMove
                                            , Tuple.second hmFrom + Tuple.second hmMove
                                            )
                                in
                                [ Game.Cell.draw MoveToCell UserPressedOnCell UserReleasedOnCell from
                                , Game.Cell.draw SelectedCell UserPressedOnCell UserReleasedOnCell to
                                ]

                            _ ->
                                []
                       )
            , Svg.g [ SvgA.class "pieces" ]
                (drawFigures game.myColor game.myFigures
                    ++ drawFigures (invert game.myColor) game.opFigures
                )
            ]
        , Svg.g [ SvgA.class "cards-group" ]
            (drawAllCards (Tuple.first game.myCards)
                (Tuple.second game.myCards)
                (Tuple.first game.opCards)
                (Tuple.second game.opCards)
                game.commonCard
                (game.nextColor /= game.myColor)
            )
        , Svg.g
            [ SvgA.class "card-prompt"
            , SvgA.display
                (case game.state of
                    ChosingCard ( _, _ ) ->
                        "block"

                    _ ->
                        "none"
                )
            ]
            (drawCardPrompt game.myCards UserChoseOneCard)
        , Svg.g
            [ SvgA.class "card-prompt pass-prompt"
            , SvgA.display
                (if canPass game then
                    "block"

                 else
                    "none"
                )
            ]
            (drawCardPrompt game.myCards UserPassedWithCard
                ++ [ Svg.text_
                        [ SvgA.class "pass-prompt-caption"
                        , SvgA.textAnchor "middle"
                        , SvgA.x "74.5"
                        , SvgA.y "56"
                        , SvgA.fontSize "6"
                        ]
                        [ Svg.text "No legal move: choose a card to pass" ]
                   ]
            )
        ]
    ]


{-| True if it is my turn but none of my figures can move with either card.
In that case the rules require me to pass one of my cards without moving.
-}
mustPass : Game -> Bool
mustPass game =
    let
        myPositions =
            figurePositions game.myFigures

        cardMoves =
            (Tuple.first game.myCards).moves ++ (Tuple.second game.myCards).moves

        canStep ( u, v ) =
            cardMoves
                |> List.any
                    (\( x, y ) ->
                        let
                            to =
                                ( u + x, v + y )
                        in
                        List.member to grid && not (List.member to myPositions)
                    )
    in
    not game.spectating
        && (game.myColor == game.nextColor)
        && not (List.any canStep myPositions)


{-| The pass prompt is shown while I still have to decide which card to pass.
-}
canPass : Game -> Bool
canPass game =
    case game.state of
        Thinking ->
            mustPass game

        FigureSelected _ ->
            mustPass game

        _ ->
            False



-- UPDATE


type Msg
    = UserChoseOneCard Card
    | UserPassedWithCard Card
    | UserPressedOnCell ( Int, Int )
    | UserReleasedOnCell ( Int, Int )
    | NewGameMove GameMove
    | HoverMove (Maybe GameMove)


update : Msg -> Game -> Game
update msg game =
    case msg of
        UserChoseOneCard card ->
            case game.state of
                ChosingCard ( from, move ) ->
                    { game | state = MoveDone { color = game.myColor, card = card, action = Step from move } }

                _ ->
                    game

        UserPassedWithCard card ->
            if canPass game then
                { game | state = MoveDone { color = game.myColor, card = card, action = Pass } }

            else
                game

        UserPressedOnCell ( x, y ) ->
            case game.state of
                WaitingForOpponent ->
                    game

                GameOver _ ->
                    game

                _ ->
                    if not game.spectating && game.myColor == game.nextColor then
                        handleClick ( x, y ) game

                    else
                        game

        UserReleasedOnCell ( x, y ) ->
            case game.state of
                FigureSelected ( u, v ) ->
                    if ( x, y ) /= ( u, v ) && not game.spectating && game.myColor == game.nextColor then
                        handleClick ( x, y ) game

                    else
                        game

                _ ->
                    game

        NewGameMove gm ->
            { game | state = MoveDone gm }
                |> execGameMove

        HoverMove maybeMove ->
            { game | hoveredMove = maybeMove }


handleClick : ( Int, Int ) -> Game -> Game
handleClick ( x, y ) game =
    if game.myFigures |> figurePositions |> List.member ( x, y ) then
        -- There is one of my own figures on this position
        case game.state of
            FigureSelected ( u, v ) ->
                if ( x, y ) == ( u, v ) then
                    -- my figure was allready selected -> Toggle it
                    { game | state = Thinking }

                else
                    -- select another figure
                    { game | state = FigureSelected ( x, y ) }

            Thinking ->
                -- none of my fugures was selected -> select this one
                { game | state = FigureSelected ( x, y ) }

            _ ->
                game

    else
        -- There is none of my own figures on this position -> move and/or eat opponents figures
        case game.state of
            FigureSelected ( u, v ) ->
                -- One of my figures was selected before
                let
                    move =
                        ( x - u, y - v )
                in
                case
                    ( (Tuple.first game.myCards).moves
                        |> List.member move
                    , (Tuple.second game.myCards).moves
                        |> List.member move
                    )
                of
                    ( True, True ) ->
                        { game | state = ChosingCard ( ( u, v ), move ) }

                    ( True, False ) ->
                        { game | state = MoveDone { color = game.myColor, card = Tuple.first game.myCards, action = Step ( u, v ) move } }

                    ( False, True ) ->
                        { game | state = MoveDone { color = game.myColor, card = Tuple.second game.myCards, action = Step ( u, v ) move } }

                    ( False, False ) ->
                        game

            _ ->
                game


adjustPerspective : Color -> ( Int, Int ) -> ( Int, Int )
adjustPerspective myColor ( x, y ) =
    case myColor of
        Black ->
            ( 4 - x, 4 - y )

        White ->
            ( x, y )


transformGameMove : GameMove -> GameMove
transformGameMove g =
    case g.color of
        Black ->
            rotateGameMove g

        White ->
            g


rotateGameMove : GameMove -> GameMove
rotateGameMove gameMove =
    case gameMove.action of
        Step ( from_x, from_y ) ( move_x, move_y ) ->
            { gameMove | action = Step ( 4 - from_x, 4 - from_y ) ( negate move_x, negate move_y ) }

        Pass ->
            gameMove


execGameMove : Game -> Game
execGameMove game =
    case game.state of
        MoveDone gameMove ->
            if gameMove.color == game.nextColor then
                case ( game.myColor, gameMove.color ) of
                    ( White, White ) ->
                        game
                            |> moveFigures gameMove

                    ( White, Black ) ->
                        game
                            |> flipFigures
                            |> flipCards
                            |> moveFigures (gameMove |> rotateGameMove)
                            |> flipFigures
                            |> flipCards

                    ( Black, White ) ->
                        game
                            |> flipFigures
                            |> flipCards
                            |> moveFigures (gameMove |> rotateGameMove)
                            |> flipFigures
                            |> flipCards

                    ( Black, Black ) ->
                        game
                            |> moveFigures gameMove

            else
                { game | state = Thinking }

        _ ->
            game


{-| The Move happens here

    - check if there is one of my figures on `from`
    - check if the move is possible (cannt move outside the grid and can't move to `Out`)
    - check if I have the card
    -> If all testes pass, then move my figure to the new pos and swap the cards
       Eat opponents figures if they are on `from + move`
    -> Do nothing if one test fails (all or nothing)
    A `Pass` only swaps the card (the server has already checked that it is legal).

-}
moveFigures : GameMove -> Game -> Game
moveFigures { color, action, card } game =
    let
        ( myFirstCard, mySecondCard ) =
            ( Tuple.first game.myCards, Tuple.second game.myCards )

        swapCards g =
            { g
                | commonCard = card
                , myCards =
                    if myFirstCard == card then
                        ( game.commonCard, mySecondCard )

                    else
                        ( myFirstCard, game.commonCard )
                , nextColor = invert color
                , state = Thinking
            }

        hasCard =
            myFirstCard == card || mySecondCard == card
    in
    case action of
        Pass ->
            if hasCard then
                swapCards game

            else
                game

        Step from ( move_x, move_y ) ->
            let
                ( from_x, from_y ) =
                    from

                to =
                    ( from_x + move_x, from_y + move_y )

                myNewFigures =
                    game.myFigures
                        |> List.map
                            (\f ->
                                if f.pos == from then
                                    -- move figure from from to newposition
                                    { f | pos = to }

                                else
                                    f
                            )

                opNewFigures =
                    game.opFigures
                        |> List.filter
                            (\f -> f.pos /= to)
            in
            if List.member from (figurePositions game.myFigures) && List.member to grid && hasCard then
                swapCards { game | myFigures = myNewFigures, opFigures = opNewFigures }

            else
                game


flipFigures : Game -> Game
flipFigures game =
    { game
        | opFigures = game.myFigures
        , myFigures = game.opFigures
    }


flipCards : Game -> Game
flipCards game =
    { game
        | myCards = game.opCards
        , opCards = game.myCards
    }


setupNewGame : List Card -> Color -> Color -> Game
setupNewGame cards playerColor nextColor =
    Game playerColor
        nextColor
        [ { kind = Pawn 1, pos = ( 0, 0 ) }
        , { kind = Pawn 2, pos = ( 1, 0 ) }
        , { kind = King, pos = ( 2, 0 ) }
        , { kind = Pawn 3, pos = ( 3, 0 ) }
        , { kind = Pawn 4, pos = ( 4, 0 ) }
        ]
        [ { kind = Pawn 4, pos = ( 0, 4 ) }
        , { kind = Pawn 3, pos = ( 1, 4 ) }
        , { kind = King, pos = ( 2, 4 ) }
        , { kind = Pawn 2, pos = ( 3, 4 ) }
        , { kind = Pawn 1, pos = ( 4, 4 ) }
        ]
        ( Maybe.withDefault dummyCard (List.head <| cards), Maybe.withDefault dummyCard (List.head <| List.drop 1 <| cards) )
        ( Maybe.withDefault dummyCard (List.head <| List.drop 2 <| cards), Maybe.withDefault dummyCard (List.head <| List.drop 3 <| cards) )
        (Maybe.withDefault dummyCard (List.head <| List.drop 4 <| cards))
        Thinking
        False
        Nothing
        |> (if playerColor == Black then
                flipCards

            else
                identity
           )


viewLog : List LogEntry -> Html Msg
viewLog log =
    Html.div [ HtmlA.class "game-log" ]
        [ Html.ul [ HtmlA.id "log-lines" ]
            (List.map viewLogEntry log)
        ]


viewLogEntry : LogEntry -> Html Msg
viewLogEntry entry =
    case entry of
        MoveEntry gameMove ->
            viewGameMove gameMove

        SystemEntry message ->
            Html.li [ HtmlA.class "log-message log-system" ]
                [ Html.text message ]


viewGameMove : GameMove -> Html Msg
viewGameMove gameMove =
    let
        colorStr =
            Game.Figure.colorToString gameMove.color
    in
    case gameMove.action of
        Pass ->
            Html.li [ HtmlA.class "log-message" ]
                [ Html.text (colorStr ++ " had no legal move and passed, playing " ++ gameMove.card.name ++ ".") ]

        Step ( fx, fy ) ( move_x, move_y ) ->
            let
                ( from_x, from_y ) =
                    ( 1 + fx, 1 + fy )

                ( to_x, to_y ) =
                    ( from_x + move_x, from_y + move_y )

                ( from_x_char, to_x_char ) =
                    ( Char.fromCode (96 + from_x), Char.fromCode (96 + to_x) )

                from =
                    String.fromChar from_x_char ++ String.fromInt from_y

                to =
                    String.fromChar to_x_char ++ String.fromInt to_y
            in
            Html.li
                [ HtmlA.class "log-message"
                , Html.Events.onMouseEnter (HoverMove (Just gameMove))
                , Html.Events.onMouseLeave (HoverMove Nothing)
                ]
                [ Html.text (colorStr ++ " moved from " ++ from ++ " to " ++ to ++ ", playing " ++ gameMove.card.name ++ ".") ]
