module Game.Card exposing (..)

import Game.Cell exposing (CellType(..), drawSimpleCell, grid)
import Game.Figure exposing (Color(..))
import Global exposing (gridsize)
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE


type alias Card =
    { name : String
    , moves : List ( Int, Int )
    , startPlayer : Color
    }


dummyCard : Card
dummyCard =
    { name = "Error", moves = [ ( 0, 1 ), ( 0, 2 ), ( 0, -1 ), ( 0, -2 ), ( -2, 0 ), ( -1, 0 ), ( 1, 0 ), ( 2, 0 ) ], startPlayer = White }


allCards : List Card
allCards =
    [ { name = "Boar", moves = [ ( -1, 0 ), ( 1, 0 ), ( 0, 1 ) ], startPlayer = White }
    , { name = "Cobra", moves = [ ( 1, 1 ), ( 1, -1 ), ( -1, 0 ) ], startPlayer = White }
    , { name = "Crab", moves = [ ( -2, 0 ), ( 2, 0 ), ( 0, 1 ) ], startPlayer = Black }
    , { name = "Crane", moves = [ ( 0, 1 ), ( -1, -1 ), ( 1, -1 ) ], startPlayer = Black }
    , { name = "Dragon", moves = [ ( -2, 1 ), ( -1, -1 ), ( 2, 1 ), ( 1, -1 ) ], startPlayer = White }
    , { name = "Eel", moves = [ ( -1, 1 ), ( -1, -1 ), ( 1, 0 ) ], startPlayer = Black }
    , { name = "Elephant", moves = [ ( -1, 0 ), ( -1, 1 ), ( 1, 0 ), ( 1, 1 ) ], startPlayer = White }
    , { name = "Frog", moves = [ ( -2, 0 ), ( -1, 1 ), ( 1, -1 ) ], startPlayer = White }
    , { name = "Goose", moves = [ ( -1, 0 ), ( -1, 1 ), ( 1, 0 ), ( 1, -1 ) ], startPlayer = Black }
    , { name = "Horse", moves = [ ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ], startPlayer = White }
    , { name = "Mantis", moves = [ ( -1, 1 ), ( 1, 1 ), ( 0, -1 ) ], startPlayer = White }
    , { name = "Monkey", moves = [ ( -1, 1 ), ( -1, -1 ), ( 1, 1 ), ( 1, -1 ) ], startPlayer = Black }
    , { name = "Ox", moves = [ ( 1, 0 ), ( 0, 1 ), ( 0, -1 ) ], startPlayer = Black }
    , { name = "Rabbit", moves = [ ( 2, 0 ), ( 1, 1 ), ( -1, -1 ) ], startPlayer = Black }
    , { name = "Rooster", moves = [ ( -1, 0 ), ( -1, -1 ), ( 1, 0 ), ( 1, 1 ) ], startPlayer = White }
    , { name = "Tiger", moves = [ ( 0, 2 ), ( 0, -1 ) ], startPlayer = Black }
    ]



{-
   moreCards : List Card -- Senseis Path
   moreCards =
       [ { name = "bear", moves = [ ( -1, 1 ), ( 0, 1 ), ( 1, -1 ) ] , startPlayer = Black }
       , { name = "dog", moves = [ ( -1, 1 ), ( -1, 0 ), ( -1, -1 ) ] , startPlayer = Black }
       , { name = "fox", moves = [ ( 1, 1 ), ( 1, 0 ), ( 1, -1 ) ] , startPlayer = White }
       , { name = "giraffe", moves = [ ( -2, 1 ), ( 0, -1 ), ( 2, 1 ) ] , startPlayer = Black }
       , { name = "iguana", moves = [ ( -2, 1 ), ( 0, 1 ), ( 1, -1 ) ] , startPlayer = White }
       , { name = "kirin", moves = [ ( -1, 2 ), ( 0, -2 ), ( 1, 2 ) ] , startPlayer = White }
       , { name = "mouse", moves = [ ( -1, -1 ), ( 0, 1 ), ( 1, 0 ) ] , startPlayer = Black }
       , { name = "otter", moves = [ ( -1, 1 ), ( 1, -1 ), ( 2, 0 ) ] , startPlayer = White }
       , { name = "panda", moves = [ ( -1, -1 ), ( 0, 1 ), ( 1, 1 ) ] , startPlayer = White }
       , { name = "phoenix", moves = [ ( -2, 0 ), ( -1, 1 ), ( 1, 1 ), ( 2, 0 ) ] , startPlayer = Black }
       , { name = "rat", moves = [ ( -1, 0 ), ( 0, 1 ), ( 1, -1 ) ] , startPlayer = White }
       , { name = "sable", moves = [ ( -2, 0 ), ( -1, -1 ), ( 1, 1 ) ] , startPlayer = Black }
       , { name = "sea_snake", moves = [ ( -1, -1 ), ( 0, 1 ), ( 2, 0 ) ] , startPlayer = Black }
       , { name = "tanuki", moves = [ ( -1, -1 ), ( 0, 1 ), ( 2, 1 ) ] , startPlayer = Black }
       , { name = "turtle", moves = [ ( -2, 0 ), ( -1, -1 ), ( 1, -1 ), ( 2, 0 ) ] , startPlayer = White }
       , { name = "viper", moves = [ ( -2, 0 ), ( 0, 1 ), ( 1, -1 ) ] , startPlayer = White }
       ]
-}


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


drawCardPrompt : ( Card, Card ) -> (Card -> msg) -> List (Svg.Svg msg)
drawCardPrompt ( cardA, cardB ) callback =
    [ Svg.rect
        [ SvgA.class "card-overlay"
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


drawAllCards : Card -> Card -> Card -> Card -> Card -> Bool -> List (Svg.Svg msg)
drawAllCards card1 card2 card3 card4 card5 rotate =
    [ Svg.g [ SvgA.class "card", SvgA.transform "matrix(0.16,  0, 0, 0.16,  6.6, 130)" ]
        (drawCard card1)
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(0.16,  0, 0, 0.16,  53.3, 130)" ]
        (drawCard card2)
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(-0.16, 0, 0, -0.16, 93.3, 20)" ]
        (drawCard card3)
    , Svg.g [ SvgA.class "card", SvgA.transform "matrix(-0.16, 0, 0, -0.16, 46.6, 20)" ]
        (drawCard card4)
    , Svg.g
        [ SvgA.class "card"
        , SvgA.transform
            ("matrix(0.16,  0, 0, 0.16,  105,  65) "
                ++ (if rotate then
                        "rotate(180, 125, 62.5)"

                    else
                        ""
                   )
            )
        ]
        (drawCard card5)
    ]


drawCard : Card -> List (Svg.Svg msg)
drawCard { name, moves } =
    [ Svg.rect
        [ SvgA.class "card-bg"
        , SvgA.width "250"
        , SvgA.height "125"
        ]
        []
    , Svg.g [ SvgA.class "move-grid", SvgA.transform "translate(12.5,12.5)" ] <|
        List.map drawSimpleCell grid
            ++ (moves
                    |> List.map
                        (\( u, v ) ->
                            Svg.rect
                                [ SvgA.class "card-dot"
                                , SvgA.x <| String.fromInt <| (gridsize * (u + 2))
                                , SvgA.y <| String.fromInt <| (gridsize * (2 - v))
                                , SvgA.width <| String.fromInt gridsize
                                , SvgA.height <| String.fromInt gridsize
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
                    ]
                    []
               ]
    , Svg.text_ [ SvgA.class "card-caption", SvgA.textAnchor "middle", SvgA.dominantBaseline "central", SvgA.fontSize "28", SvgA.x "175", SvgA.y "62.5" ]
        [ Svg.text name ]
    ]
