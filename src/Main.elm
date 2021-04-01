module Main exposing (main)

--import TimeTravel.Browser as TimeTravel

import Browser
import Card exposing (..)
import Cell exposing (..)
import Figure exposing (..)
import Global exposing (..)
import Html
import Html.Attributes as HtmlA
import List.Extra
import Random
import Random.List exposing (choices)
import Svg
import Svg.Attributes as SvgA


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { myColor : Color
    , myFigures : List Figure
    , opFigures : List Figure
    , myCards : ( Card, Card )
    , opCards : ( Card, Card )
    , nextCard : Card
    , prevPosition : Position
    , chooseCard : Maybe ( Int, Int )
    }


type Msg
    = Clicked Position
    | ChoosedCard Card
    | NewCards ( List Card, List Card )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        White
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
    , Random.generate NewCards (allCards |> choices 5)
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCards ( shuffledCards, _ ) ->
            ( { model
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
            , Cmd.none
            )

        ChoosedCard card ->
            let
                position =
                    case model.chooseCard of
                        Just p ->
                            p

                        Nothing ->
                            -- cannot happen but compiler needs it
                            ( 0, 0 )
            in
            ( (if card == Tuple.first model.myCards then
                { model | chooseCard = Nothing, nextCard = Tuple.first model.myCards, myCards = ( model.nextCard, Tuple.second model.myCards ) }

               else
                { model | chooseCard = Nothing, nextCard = Tuple.second model.myCards, myCards = ( Tuple.first model.myCards, model.nextCard ) }
              )
                |> movefigure position
            , Cmd.none
            )

        Clicked Out ->
            ( { model | prevPosition = Out }
            , Cmd.none
            )

        Clicked (On position) ->
            if model.myFigures |> figurePositions |> List.member position then
                -- There is one of my own figures on this position
                case model.prevPosition of
                    On previousPosition ->
                        if position == previousPosition then
                            -- my figure was allready selected -> Toggle it
                            ( { model | prevPosition = Out }, Cmd.none )

                        else
                            -- select another figure
                            ( { model | prevPosition = On position }, Cmd.none )

                    Out ->
                        -- none of my fugures was selected -> select this one
                        ( { model | prevPosition = On position }, Cmd.none )

            else
                -- There is none of my own figures on this position -> move and/or eat opponents figures
                case model.prevPosition of
                    On previousPosition ->
                        -- One of my figures was selected before
                        let
                            move =
                                ( Tuple.first position - Tuple.first previousPosition, Tuple.second position - Tuple.second previousPosition )
                        in
                        case ( List.member move (Tuple.first model.myCards).moves, List.member move (Tuple.second model.myCards).moves ) of
                            ( True, True ) ->
                                ( { model | chooseCard = Just position }, Cmd.none )

                            ( True, False ) ->
                                ( { model | nextCard = Tuple.first model.myCards, myCards = ( model.nextCard, Tuple.second model.myCards ) }
                                    |> movefigure position
                                , Cmd.none
                                )

                            ( False, True ) ->
                                ( { model | nextCard = Tuple.second model.myCards, myCards = ( Tuple.first model.myCards, model.nextCard ) }
                                    |> movefigure position
                                , Cmd.none
                                )

                            ( False, False ) ->
                                ( model, Cmd.none )

                    Out ->
                        -- none of my own figures was selected before
                        ( { model | prevPosition = Out }, Cmd.none )


movefigure : ( Int, Int ) -> Model -> Model
movefigure newposition model =
    { model
        | prevPosition = Out
        , myFigures =
            model.myFigures
                |> List.map
                    (\f ->
                        if f.pos == model.prevPosition then
                            -- move figure from prev to newposition
                            { f | pos = On newposition }

                        else
                            f
                    )
        , opFigures =
            model.opFigures
                |> List.map
                    (\f ->
                        if f.pos == On newposition then
                            -- eat opponents figure
                            { f | pos = Out }

                        else
                            f
                    )
    }
        |> nextPlayer


nextPlayer : Model -> Model
nextPlayer model =
    { model
        | opFigures =
            model.myFigures
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
            model.opFigures
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
        , myCards = model.opCards
        , opCards = model.myCards
        , myColor = invert model.myColor
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Onitama"
        [ Html.div [ HtmlA.style "display" "flex", HtmlA.style "height" "100vh", HtmlA.style "flex-direction" "column", HtmlA.style "padding" "8px", HtmlA.style "box-sizing" "border-box" ]
            [ Svg.svg [ SvgA.id "game-board", SvgA.viewBox "-1 -1 152 152" ]
                [ predefinedSymbols
                , Svg.g [ SvgA.transform <| "translate(0, " ++ (String.fromInt <| gridsize + 5) ++ ")" ]
                    [ Svg.g [ SvgA.class "pieces" ]
                        (drawFigures model.myColor model.myFigures
                            ++ drawFigures (invert model.myColor) model.opFigures
                        )
                    , Svg.g [ SvgA.class "grid-lines" ] <|
                        -- the grid is drawn here
                        List.map (Cell.draw NormalCell Clicked) grid
                            ++ (case model.prevPosition of
                                    On ( u, v ) ->
                                        -- draw the highlighted cells for next possible moves
                                        List.append [ Cell.draw MoveToCell Clicked ( u, v ) ]
                                            ((List.Extra.unique ((Tuple.first model.myCards).moves ++ (Tuple.second model.myCards).moves)
                                                |> List.map (\( x, y ) -> ( x + u, y + v ))
                                                |> List.filter (\move -> grid |> List.member move)
                                                |> List.Extra.filterNot
                                                    (\move ->
                                                        model.myFigures
                                                            |> figurePositions
                                                            |> List.member move
                                                    )
                                             )
                                                |> List.map (Cell.draw SelectedCell Clicked)
                                            )

                                    Out ->
                                        []
                               )
                    ]
                , Svg.g [ SvgA.class "cards-group" ]
                    (drawAllCards (Tuple.first model.myCards)
                        (Tuple.second model.myCards)
                        (Tuple.first model.opCards)
                        (Tuple.second model.opCards)
                        model.nextCard
                    )
                , Svg.g
                    [ SvgA.class "card-prompt"
                    , SvgA.display
                        (case model.chooseCard of
                            Just _ ->
                                "block"

                            Nothing ->
                                "none"
                        )
                    ]
                    (drawCardPrompt model.myCards ChoosedCard)
                , Svg.text_ [ SvgA.class "status-line", SvgA.x "145", SvgA.y "2", SvgA.fontSize "3", SvgA.textAnchor "end" ]
                    [ Svg.text <|
                        case model.myColor of
                            White ->
                                "white to move"

                            Black ->
                                "black to move"
                    ]
                ]
            ]
        ]
