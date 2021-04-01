module Figure exposing (..)

import Global exposing (gridsize)
import Svg
import Svg.Attributes as SvgA


type Position
    = On ( Int, Int ) -- Position from (0, 0) to (4, 4)
    | Out


type Color
    = White
    | Black


invert : Color -> Color
invert color =
    case color of
        White ->
            Black

        Black ->
            White


type FigureKind
    = King
    | Pawn Int -- Pawnnumber from 1 to 4 (in both colors)


type alias Figure =
    { kind : FigureKind
    , pos : Position
    }


figurePositions : List Figure -> List ( Int, Int )
figurePositions figs =
    figs
        --|> List.filter (\f -> f.color == color)
        |> List.filter
            (\f ->
                case f.pos of
                    On ( _, _ ) ->
                        True

                    Out ->
                        False
            )
        |> List.map
            (\f ->
                case f.pos of
                    On ( x, y ) ->
                        ( x, y )

                    Out ->
                        -- impossible case but compiler needs it
                        ( 0, 0 )
            )


drawFigures : Color -> List Figure -> List (Svg.Svg msg)
drawFigures color_ figs =
    case figs of
        [] ->
            []

        head :: tail ->
            let
                color =
                    case color_ of
                        Black ->
                            "black"

                        White ->
                            "white"

                ( kind, num ) =
                    case head.kind of
                        King ->
                            ( "King", 0 )

                        Pawn n ->
                            ( "Pawn", n )

                ( x, y ) =
                    case head.pos of
                        On ( x_, y_ ) ->
                            ( x_, y_ )

                        Out ->
                            ( 5, 5 )
            in
            case ( x, y ) of
                ( 5, 5 ) ->
                    drawFigures color_ tail

                _ ->
                    List.append
                        [ Svg.use
                            [ SvgA.xlinkHref ("#" ++ color ++ kind)
                            , SvgA.width <| String.fromInt <| (gridsize - 5)
                            , SvgA.height <| String.fromInt <| (gridsize - 5)
                            , SvgA.x <| String.fromFloat <| toFloat gridsize * toFloat x + 2.8
                            , SvgA.y <| String.fromFloat <| toFloat gridsize * toFloat (4 - y) + 2.5
                            ]
                            []
                        , Svg.text_
                            [ SvgA.class "status-line"
                            , SvgA.x <| String.fromFloat <| toFloat gridsize * toFloat x + 3
                            , SvgA.y <| String.fromFloat <| toFloat gridsize * toFloat (4 - y) - 1.5 + toFloat gridsize
                            , SvgA.fontSize "3"
                            , SvgA.textAnchor "start"
                            ]
                            [ Svg.text <|
                                kind
                                    ++ " "
                                    ++ (if num /= 0 then
                                            String.fromInt num

                                        else
                                            ""
                                       )
                            ]
                        ]
                        (drawFigures color_ tail)
