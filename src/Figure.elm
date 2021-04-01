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


predefinedSymbols : Svg.Svg msg
predefinedSymbols =
    Svg.g [ SvgA.class "symbol-definitions" ]
        [ Svg.symbol [ SvgA.id "blackKing", SvgA.width "45", SvgA.height "45", SvgA.viewBox "0 0 45 45" ]
            [ Svg.g [ SvgA.style "fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-SvgA.width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ]
                [ Svg.path [ SvgA.d "M 22.5,11.63 L 22.5,6", SvgA.style "fill:none; stroke:#000000; stroke-linejoin:miter;", SvgA.id "path6570" ] []
                , Svg.path [ SvgA.d "M 22.5,25 C 22.5,25 27,17.5 25.5,14.5 C 25.5,14.5 24.5,12 22.5,12 C 20.5,12 19.5,14.5 19.5,14.5 C 18,17.5 22.5,25 22.5,25", SvgA.style "fill:#000000;fill-opacity:1; stroke-linecap:butt; stroke-linejoin:miter;" ] []
                , Svg.path [ SvgA.d "M 11.5,37 C 17,40.5 27,40.5 32.5,37 L 32.5,30 C 32.5,30 41.5,25.5 38.5,19.5 C 34.5,13 25,16 22.5,23.5 L 22.5,27 L 22.5,23.5 C 19,16 9.5,13 6.5,19.5 C 3.5,25.5 11.5,29.5 11.5,29.5 L 11.5,37 z ", SvgA.style "fill:#000000; stroke:#000000;" ] []
                , Svg.path [ SvgA.d "M 20,8 L 25,8", SvgA.style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] []
                , Svg.path [ SvgA.d "M 32,29.5 C 32,29.5 40.5,25.5 38.03,19.85 C 34.15,14 25,18 22.5,24.5 L 22.51,26.6 L 22.5,24.5 C 20,18 9.906,14 6.997,19.85 C 4.5,25.5 11.85,28.85 11.85,28.85", SvgA.style "fill:none; stroke:#ffffff;" ] []
                , Svg.path [ SvgA.d "M 11.5,30 C 17,27 27,27 32.5,30 M 11.5,33.5 C 17,30.5 27,30.5 32.5,33.5 M 11.5,37 C 17,34 27,34 32.5,37", SvgA.style "fill:none; stroke:#ffffff;" ] []
                ]
            ]
        , Svg.symbol [ SvgA.id "whiteKing", SvgA.width "45", SvgA.height "45", SvgA.viewBox "0 0 45 45" ]
            [ Svg.g [ SvgA.style "fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-SvgA.width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ]
                [ Svg.path [ SvgA.d "M 22.5,11.63 L 22.5,6", SvgA.style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] []
                , Svg.path [ SvgA.d "M 20,8 L 25,8", SvgA.style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] []
                , Svg.path [ SvgA.d "M 22.5,25 C 22.5,25 27,17.5 25.5,14.5 C 25.5,14.5 24.5,12 22.5,12 C 20.5,12 19.5,14.5 19.5,14.5 C 18,17.5 22.5,25 22.5,25", SvgA.style "fill:#ffffff; stroke:#000000; stroke-linecap:butt; stroke-linejoin:miter;" ] []
                , Svg.path [ SvgA.d "M 11.5,37 C 17,40.5 27,40.5 32.5,37 L 32.5,30 C 32.5,30 41.5,25.5 38.5,19.5 C 34.5,13 25,16 22.5,23.5 L 22.5,27 L 22.5,23.5 C 19,16 9.5,13 6.5,19.5 C 3.5,25.5 11.5,29.5 11.5,29.5 L 11.5,37 z ", SvgA.style "fill:#ffffff; stroke:#000000;" ] []
                , Svg.path [ SvgA.d "M 11.5,30 C 17,27 27,27 32.5,30", SvgA.style "fill:none; stroke:#000000;" ] []
                , Svg.path [ SvgA.d "M 11.5,33.5 C 17,30.5 27,30.5 32.5,33.5", SvgA.style "fill:none; stroke:#000000;" ] []
                , Svg.path [ SvgA.d "M 11.5,37 C 17,34 27,34 32.5,37", SvgA.style "fill:none; stroke:#000000;" ] []
                ]
            ]
        , Svg.symbol [ SvgA.id "blackPawn", SvgA.width "45", SvgA.height "45", SvgA.viewBox "0 0 45 45" ]
            [ Svg.path [ SvgA.d "M 22,9 C 19.79,9 18,10.79 18,13 C 18,13.89 18.29,14.71 18.78,15.38 C 16.83,16.5 15.5,18.59 15.5,21 C 15.5,23.03 16.44,24.84 17.91,26.03 C 14.91,27.09 10.5,31.58 10.5,39.5 L 33.5,39.5 C 33.5,31.58 29.09,27.09 26.09,26.03 C 27.56,24.84 28.5,23.03 28.5,21 C 28.5,18.59 27.17,16.5 25.22,15.38 C 25.71,14.71 26,13.89 26,13 C 26,10.79 24.21,9 22,9 z ", SvgA.style "opacity:1; fill:#000000; fill-opacity:1; fill-rule:nonzero; stroke:#000000; stroke-SvgA.width:1.5; stroke-linecap:round; stroke-linejoin:miter; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [] ]
        , Svg.symbol [ SvgA.id "whitePawn", SvgA.width "45", SvgA.height "45", SvgA.viewBox "0 0 45 45" ]
            [ Svg.path [ SvgA.d "M 22,9 C 19.79,9 18,10.79 18,13 C 18,13.89 18.29,14.71 18.78,15.38 C 16.83,16.5 15.5,18.59 15.5,21 C 15.5,23.03 16.44,24.84 17.91,26.03 C 14.91,27.09 10.5,31.58 10.5,39.5 L 33.5,39.5 C 33.5,31.58 29.09,27.09 26.09,26.03 C 27.56,24.84 28.5,23.03 28.5,21 C 28.5,18.59 27.17,16.5 25.22,15.38 C 25.71,14.71 26,13.89 26,13 C 26,10.79 24.21,9 22,9 z ", SvgA.style "opacity:1; fill:#ffffff; fill-opacity:1; fill-rule:nonzero; stroke:#000000; stroke-SvgA.width:1.5; stroke-linecap:round; stroke-linejoin:miter; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [] ]
        ]
