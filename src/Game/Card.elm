module Game.Card exposing (..)

import Game.Cell exposing (CellType(..), drawSimpleCell, grid)
import Global exposing (gridsize)
import Random
import Random.List exposing (choices)
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE


type alias Card =
    { name : String
    , moves : List ( Int, Int )
    }


dummyCard : Card
dummyCard =
    { name = "Error", moves = [ ( 0, 1 ), ( 0, 2 ), ( 0, -1 ), ( 0, -2 ), ( -2, 0 ), ( -1, 0 ), ( 1, 0 ), ( 2, 0 ) ] }


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


cardByName : String -> Card
cardByName name =
    case
        allCards
            |> List.filter (\card -> card.name == name)
    of
        x :: _ ->
            x

        [] ->
            dummyCard


chooseFiveCards : Random.Generator ( List Card, List Card )
chooseFiveCards =
    allCards |> choices 5


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
