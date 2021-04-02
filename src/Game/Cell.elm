module Game.Cell exposing (..)

import Game.Figure exposing (Position(..))
import Global exposing (gridsize)
import List.Extra
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE


grid : List ( Int, Int )
grid =
    -- creates the 25 cell grid
    List.Extra.lift2 Tuple.pair [ 0, 1, 2, 3, 4 ] [ 0, 1, 2, 3, 4 ]


type CellType
    = NormalCell
    | SelectedCell
    | MoveToCell


draw : CellType -> (Position -> msg) -> ( Int, Int ) -> Svg.Svg msg
draw celltype callback ( u, v ) =
    Svg.rect
        -- ToDO: Use class attibutes instead of stroke SvgA.class="highlighted-cell" SvgA.class="possible-move"
        [ SvgA.x <| String.fromInt <| u * gridsize
        , SvgA.y <| String.fromInt <| (4 - v) * gridsize
        , SvgA.width <| String.fromInt gridsize
        , SvgA.height <| String.fromInt gridsize
        , SvgA.fill "white"
        , SvgA.fillOpacity "0"
        , SvgA.stroke
            (case celltype of
                NormalCell ->
                    "black"

                SelectedCell ->
                    "green"

                MoveToCell ->
                    "yellow"
            )
        , SvgA.strokeWidth "1"
        , SvgE.onClick <| callback (On ( u, v ))
        ]
        []


drawSimpleCell : ( Int, Int ) -> Svg.Svg msg
drawSimpleCell ( u, v ) =
    Svg.rect
        -- ToDO: Use class attibutes instead of stroke SvgA.class="highlighted-cell" SvgA.class="possible-move"
        [ SvgA.x <| String.fromInt <| u * gridsize
        , SvgA.y <| String.fromInt <| (4 - v) * gridsize
        , SvgA.width <| String.fromInt gridsize
        , SvgA.height <| String.fromInt gridsize
        , SvgA.fill "white"
        , SvgA.fillOpacity "0"
        , SvgA.stroke "black"
        , SvgA.strokeWidth "1"
        ]
        []
