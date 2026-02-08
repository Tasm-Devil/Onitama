module Game.Cell exposing (..)

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


draw : CellType -> (( Int, Int ) -> msg) -> ( Int, Int ) -> Svg.Svg msg
draw celltype callback ( u, v ) =
    Svg.rect
        [ SvgA.x <| String.fromInt <| u * gridsize
        , SvgA.y <| String.fromInt <| (4 - v) * gridsize
        , SvgA.width <| String.fromInt gridsize
        , SvgA.height <| String.fromInt gridsize
        , SvgA.class
            (case celltype of
                NormalCell ->
                    "cell"

                SelectedCell ->
                    "cell cell--target"

                MoveToCell ->
                    "cell cell--selected"
            )
        , SvgE.onClick <| callback ( u, v )
        ]
        []


drawSimpleCell : ( Int, Int ) -> Svg.Svg msg
drawSimpleCell ( u, v ) =
    Svg.rect
        [ SvgA.x <| String.fromInt <| u * gridsize
        , SvgA.y <| String.fromInt <| (4 - v) * gridsize
        , SvgA.width <| String.fromInt gridsize
        , SvgA.height <| String.fromInt gridsize
        , SvgA.class "cell"
        ]
        []
