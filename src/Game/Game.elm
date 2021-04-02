module Game.Game exposing (..)

import Game.Card exposing (..)
import Game.Cell exposing (..)
import Game.Figure exposing (..)
import Global exposing (gridsize)
import Html exposing (Html)
import Html.Attributes as HtmlA
import List.Extra
import Random
import Svg
import Svg.Attributes as SvgA



-- MODEL


type alias Game =
    { myColor : Color
    , myFigures : List Figure
    , opFigures : List Figure
    , myCards : ( Card, Card )
    , opCards : ( Card, Card )
    , nextCard : Card
    , prevPosition : Position
    , chooseCard : Maybe ( Int, Int )
    }


setupNewGame : Game
setupNewGame =
    Game White
        [ { kind = Pawn 1, pos = On ( 0, 0 ) }
        , { kind = Pawn 2, pos = On ( 1, 0 ) }
        , { kind = King, pos = On ( 2, 0 ) }
        , { kind = Pawn 3, pos = On ( 3, 0 ) }
        , { kind = Pawn 4, pos = On ( 4, 0 ) }
        ]
        [ { kind = Pawn 4, pos = On ( 0, 4 ) }
        , { kind = Pawn 3, pos = On ( 1, 4 ) }
        , { kind = King, pos = On ( 2, 4 ) }
        , { kind = Pawn 2, pos = On ( 3, 4 ) }
        , { kind = Pawn 1, pos = On ( 4, 4 ) }
        ]
        ( dummyCard, dummyCard )
        ( dummyCard, dummyCard )
        dummyCard
        Out
        Nothing



-- UPDATE


type Msg
    = UserClickedOnCell Position
    | UserChoseOneCard Card
    | GotNewCards ( List Card, List Card )


updateGame : Msg -> Game -> Game
updateGame msg game =
    case msg of
        GotNewCards ( shuffledCards, _ ) ->
            { game
                | myCards =
                    ( case List.head <| shuffledCards of
                        Just c ->
                            c

                        Nothing ->
                            dummyCard
                    , case List.head <| List.drop 1 <| shuffledCards of
                        Just c ->
                            c

                        Nothing ->
                            dummyCard
                    )
                , opCards =
                    ( case List.head <| List.drop 2 <| shuffledCards of
                        Just c ->
                            c

                        Nothing ->
                            dummyCard
                    , case List.head <| List.drop 3 <| shuffledCards of
                        Just c ->
                            c

                        Nothing ->
                            dummyCard
                    )
                , nextCard =
                    case List.head <| List.drop 4 <| shuffledCards of
                        Just c ->
                            c

                        Nothing ->
                            dummyCard
            }

        UserChoseOneCard card ->
            let
                position =
                    case game.chooseCard of
                        Just p ->
                            p

                        Nothing ->
                            -- cannot happen but compiler needs it
                            ( 0, 0 )
            in
            (if card == Tuple.first game.myCards then
                { game
                    | chooseCard = Nothing
                    , nextCard = Tuple.first game.myCards
                    , myCards = ( game.nextCard, Tuple.second game.myCards )
                }

             else
                { game
                    | chooseCard = Nothing
                    , nextCard = Tuple.second game.myCards
                    , myCards = ( Tuple.first game.myCards, game.nextCard )
                }
            )
                |> movefigure position
                >> nextPlayer

        UserClickedOnCell Out ->
            { game | prevPosition = Out }

        UserClickedOnCell (On position) ->
            if game.myFigures |> figurePositions |> List.member position then
                -- There is one of my own figures on this position
                case game.prevPosition of
                    On previousPosition ->
                        if position == previousPosition then
                            -- my figure was allready selected -> Toggle it
                            { game | prevPosition = Out }

                        else
                            -- select another figure
                            { game | prevPosition = On position }

                    Out ->
                        -- none of my fugures was selected -> select this one
                        { game | prevPosition = On position }

            else
                -- There is none of my own figures on this position -> move and/or eat opponents figures
                case game.prevPosition of
                    On previousPosition ->
                        -- One of my figures was selected before
                        let
                            move =
                                ( Tuple.first position - Tuple.first previousPosition, Tuple.second position - Tuple.second previousPosition )
                        in
                        case ( List.member move (Tuple.first game.myCards).moves, List.member move (Tuple.second game.myCards).moves ) of
                            ( True, True ) ->
                                { game | chooseCard = Just position }

                            ( True, False ) ->
                                { game | nextCard = Tuple.first game.myCards, myCards = ( game.nextCard, Tuple.second game.myCards ) }
                                    |> movefigure position
                                    >> nextPlayer

                            ( False, True ) ->
                                { game | nextCard = Tuple.second game.myCards, myCards = ( Tuple.first game.myCards, game.nextCard ) }
                                    |> movefigure position
                                    >> nextPlayer

                            ( False, False ) ->
                                game

                    Out ->
                        -- none of my own figures was selected before
                        { game | prevPosition = Out }


movefigure : ( Int, Int ) -> Game -> Game
movefigure newposition game =
    { game
        | prevPosition = Out
        , myFigures =
            game.myFigures
                |> List.map
                    (\f ->
                        if f.pos == game.prevPosition then
                            -- move figure from prev to newposition
                            { f | pos = On newposition }

                        else
                            f
                    )
        , opFigures =
            game.opFigures
                |> List.map
                    (\f ->
                        if f.pos == On newposition then
                            -- eat opponents figure
                            { f | pos = Out }

                        else
                            f
                    )
    }


nextPlayer : Game -> Game
nextPlayer game =
    { game
        | opFigures =
            game.myFigures
                |> List.map
                    (\fig ->
                        { fig
                            | pos =
                                case fig.pos of
                                    On ( x, y ) ->
                                        On ( 4 - x, 4 - y )

                                    Out ->
                                        fig.pos
                        }
                    )
        , myFigures =
            game.opFigures
                |> List.map
                    (\fig ->
                        { fig
                            | pos =
                                case fig.pos of
                                    On ( x, y ) ->
                                        On ( 4 - x, 4 - y )

                                    Out ->
                                        fig.pos
                        }
                    )
        , myCards = game.opCards
        , opCards = game.myCards
        , myColor = invert game.myColor
    }



-- VIEW


view : Game -> List (Html Msg)
view game =
    [ Html.div [ HtmlA.style "display" "flex", HtmlA.style "height" "100vh", HtmlA.style "flex-direction" "column", HtmlA.style "padding" "8px", HtmlA.style "box-sizing" "border-box" ]
        [ Svg.svg [ SvgA.id "game-board", SvgA.viewBox "-1 -1 152 152" ]
            [ predefinedSymbols
            , Svg.g [ SvgA.transform <| "translate(0, " ++ (String.fromInt <| gridsize + 5) ++ ")" ]
                [ Svg.g [ SvgA.class "pieces" ]
                    (drawFigures game.myColor game.myFigures
                        ++ drawFigures (invert game.myColor) game.opFigures
                    )
                , Svg.g [ SvgA.class "grid-lines" ] <|
                    -- the grid is drawn here
                    List.map (Game.Cell.draw NormalCell UserClickedOnCell) grid
                        ++ (case game.prevPosition of
                                On ( u, v ) ->
                                    -- draw the highlighted cells for next possible moves
                                    List.append [ Game.Cell.draw MoveToCell UserClickedOnCell ( u, v ) ]
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
                                            |> List.map (Game.Cell.draw SelectedCell UserClickedOnCell)
                                        )

                                Out ->
                                    []
                           )
                ]
            , Svg.g [ SvgA.class "cards-group" ]
                (drawAllCards (Tuple.first game.myCards)
                    (Tuple.second game.myCards)
                    (Tuple.first game.opCards)
                    (Tuple.second game.opCards)
                    game.nextCard
                )
            , Svg.g
                [ SvgA.class "card-prompt"
                , SvgA.display
                    (case game.chooseCard of
                        Just _ ->
                            "block"

                        Nothing ->
                            "none"
                    )
                ]
                (drawCardPrompt game.myCards UserChoseOneCard)
            , Svg.text_ [ SvgA.class "status-line", SvgA.x "145", SvgA.y "2", SvgA.fontSize "3", SvgA.textAnchor "end" ]
                [ Svg.text <|
                    case game.myColor of
                        White ->
                            "white to move"

                        Black ->
                            "black to move"
                ]
            ]
        ]
    ]


startNewGame : Cmd Msg
startNewGame =
    Random.generate GotNewCards chooseFiveCards
