module Card exposing (..)

import Cell exposing (CellType(..), drawSimpleCell, grid)
import Global exposing (gridsize)
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE


type alias Card =
    { name : String
    , moves : List ( Int, Int )
    }


dummyCard : Card
dummyCard =
    { name = "Error", moves = [ ( 0, 1 ), ( 0, 2 ), ( 0, -1 ), ( 0, -2 ),( -2, 0 ), ( -1, 0 ), ( 1, 0 ), ( 2, 0 ) ] }


allCards : List Card
allCards =
    [ { name = "Tiger", moves = [ ( 0, 2 ), ( 0, -1 ) ] }
    , { name = "Dragon", moves = [ ( -2, 1 ), ( -1, -1 ), ( 2, 1 ), ( 1, -1 ) ] }
    , { name = "Frog", moves = [ ( -2, 0 ), ( -1, 1 ), ( 1, -1 ) ] }
    , { name = "Rabbit", moves = [ ( 2, 0 ), ( 1, 1 ), ( -1, -1 ) ] }
    , { name = "Crab", moves = [ ( -2, 0 ), ( 2, 0 ), ( 0, 1 ) ] }
    , { name = "Elephant", moves = [ ( -1, 0 ), ( -1, 1 ), ( 1, 0 ), ( 1, 1 ) ] }
    , { name = "Goose", moves = [ ( -1, 0 ), ( -1, 1 ), ( 1, 0 ), ( 1, -1 ) ] }
    , { name = "Rooster", moves = [ ( -1, 0 ), ( -1, -1 ), ( 1, 0 ), ( 1, 1 ) ] }
    , { name = "Monkey", moves = [ ( -1, 1 ), ( -1, -1 ), ( 1, 1 ), ( 1, -1 ) ] }
    , { name = "Mantis", moves = [ ( -1, 1 ), ( 1, 1 ), ( 0, -1 ) ] }
    , { name = "Horse", moves = [ ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ] }
    , { name = "Ox", moves = [ ( 1, 0 ), ( 0, 1 ), ( 0, -1 ) ] }
    , { name = "Crane", moves = [ ( 0, 1 ), ( -1, -1 ), ( 1, -1 ) ] }
    , { name = "Boar", moves = [ ( -1, 0 ), ( 1, 0 ), ( 0, 1 ) ] }
    , { name = "Eel", moves = [ ( -1, 1 ), ( -1, -1 ), ( 1, 0 ) ] }
    , { name = "Cobra", moves = [ ( 1, 1 ), ( 1, -1 ), ( -1, 0 ) ] }
    ]


drawCardPrompt : ( Card, Card ) -> (Card -> msg) -> List (Svg.Svg msg)
drawCardPrompt ( cardA, cardB ) callback =
    [ Svg.rect
        [ SvgA.class "overlay"
        , SvgA.fill "rgba(0,0,0,0.7)"
        , SvgA.x "-1"
        , SvgA.y "-1"
        , SvgA.width "152"
        , SvgA.height "152"
        ]
        []
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(0.2,0,0,0.2,16,62.5)", SvgE.onClick <| callback cardA ]
        (drawCard cardA)
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(0.2,0,0,0.2,83,62.5)", SvgE.onClick <| callback cardB ]
        (drawCard cardB)
    ]


drawAllCards : Card -> Card -> Card -> Card -> Card -> List (Svg.Svg msg)
drawAllCards card1 card2 card3 card4 card5 =
    [ Svg.g [ SvgA.class "card", SvgA.transform "matrix(0.16,  0, 0, 0.16,  6.6, 130)" ]
        (drawCard card1)
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(0.16,  0, 0, 0.16,  53.3, 130)" ]
        (drawCard card2)
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(-0.16, 0, 0, -0.16, 93.3, 20)" ]
        (drawCard card3)
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(-0.16, 0, 0, -0.16, 46.6, 20)" ]
        (drawCard card4)
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(0.16,  0, 0, 0.16,  105,  65)" ]
        (drawCard card5)
    ]


drawCard : Card -> List (Svg.Svg msg)
drawCard { name, moves } =
    let
        stroke_ =
            "black"
    in
    [ Svg.rect
        [ SvgA.class "background"
        , SvgA.width "250"
        , SvgA.height "125"
        , SvgA.fill "white"
        , SvgA.stroke stroke_
        , SvgA.strokeWidth "1"
        ]
        []
    , Svg.g [ SvgA.class "move-grid", SvgA.transform "translate(12.5,12.5)" ] <|
        List.map drawSimpleCell grid
            ++ (moves
                    |> List.map
                        (\( u, v ) ->
                            Svg.rect
                                [ SvgA.class "card-move"
                                , SvgA.x <| String.fromInt <| (gridsize * (u + 2))
                                , SvgA.y <| String.fromInt <| (gridsize * (2 - v))
                                , SvgA.width <| String.fromInt gridsize
                                , SvgA.height <| String.fromInt gridsize
                                , SvgA.fill "grey"
                                , SvgA.stroke stroke_
                                ]
                                []
                        )
               )
            ++ [ Svg.rect
                    [ SvgA.class "card-center"
                    , SvgA.x <| String.fromInt <| gridsize * 2
                    , SvgA.y <| String.fromInt <| gridsize * 2
                    , SvgA.width <| String.fromInt gridsize
                    , SvgA.height <| String.fromInt gridsize
                    , SvgA.fill "black"
                    , SvgA.stroke stroke_
                    ]
                    []
               ]
    , Svg.text_ [ SvgA.class "card-caption", SvgA.textAnchor "middle", SvgA.x "175", SvgA.y "65" ]
        [ Svg.text name ]
    ]


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
