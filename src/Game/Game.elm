module Game.Game exposing (..)

import Game.Card exposing (..)
import Game.Cell exposing (..)
import Game.Figure exposing (..)
import Global exposing (gridsize)
import Html exposing (Html)
import List.Extra
import Random
import Svg
import Svg.Attributes as SvgA
import Task



-- MODEL


type alias Game =
    { myColor : Color
    , nextColor : Color
    , myFigures : List Figure
    , opFigures : List Figure
    , myCards : ( Card, Card )
    , opCards : ( Card, Card )
    , commonCard : Card
    , lastClickedCell : Maybe ( Int, Int )
    , chooseCard : Maybe ( ( Int, Int ), ( Int, Int ) )
    }


{-| GameMove

    - The from field is the start position and is always seen from the perspective of the White player
    - The move field is independent from the player color. See it from the perspectiv of a non rotated card

-}
type alias GameMove =
    { color : Color
    , card : Card
    , from : ( Int, Int )
    , move : ( Int, Int )
    }



-- VIEW


view : Game -> List (Html Msg)
view game =
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
                    ++ (case game.lastClickedCell of
                            Just ( u, v ) ->
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

                            Nothing ->
                                []
                       )
            ]
        , Svg.g [ SvgA.class "cards-group" ]
            (drawAllCards (Tuple.first game.myCards)
                (Tuple.second game.myCards)
                (Tuple.first game.opCards)
                (Tuple.second game.opCards)
                game.commonCard
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
                case game.nextColor of
                    White ->
                        "white to move"

                    Black ->
                        "black to move"
            ]
        ]
    ]



-- UPDATE


type Msg
    = UserClickedOnCell Position
    | UserChoseOneCard Card
    | GotNewCards ( List Card, List Card )
    | NewGameMove GameMove


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        GotNewCards ( shuffledCards, _ ) ->
            ( game
                |> newCards shuffledCards
            , Cmd.none
            )

        NewGameMove gameMove ->
            if gameMove.color == game.nextColor then
                case ( game.myColor, gameMove.color ) of
                    ( White, White ) ->
                        ( game
                            |> executeGameMove gameMove
                        , Cmd.none
                        )

                    ( White, Black ) ->
                        ( game
                            |> flipFigures
                            |> flipCards
                            |> executeGameMove (gameMove |> rotateCardMove)
                            |> flipFigures
                            |> flipCards
                        , Cmd.none
                        )

                    ( Black, White ) ->
                        ( game
                            |> flipFigures
                            |> flipCards
                            |> executeGameMove (gameMove |> rotateCardMove >> rotateFromMove)
                            |> flipFigures
                            |> flipCards
                        , Cmd.none
                        )

                    ( Black, Black ) ->
                        ( game
                            |> executeGameMove (gameMove |> rotateFromMove)
                        , Cmd.none
                        )

            else
                ( game, Cmd.none )

        UserChoseOneCard card ->
            let
                ( previousPosition, move ) =
                    case game.chooseCard of
                        Just ( prev, move_ ) ->
                            ( prev, move_ )

                        Nothing ->
                            -- cannot happen but compiler needs it
                            ( ( 0, 0 ), ( 0, 0 ) )
            in
            ( game
            , sendNewGameMove { color = game.myColor, card = card, from = previousPosition, move = move }
            )

        UserClickedOnCell Out ->
            ( { game | lastClickedCell = Nothing }, Cmd.none )

        UserClickedOnCell (On position) ->
            if game.myColor == game.nextColor then
                if game.myFigures |> figurePositions |> List.member position then
                    -- There is one of my own figures on this position
                    case game.lastClickedCell of
                        Just previousPosition ->
                            if position == previousPosition then
                                -- my figure was allready selected -> Toggle it
                                ( { game | lastClickedCell = Nothing }, Cmd.none )

                            else
                                -- select another figure
                                ( { game | lastClickedCell = Just position }, Cmd.none )

                        Nothing ->
                            -- none of my fugures was selected -> select this one
                            ( { game | lastClickedCell = Just position }, Cmd.none )

                else
                    -- There is none of my own figures on this position -> move and/or eat opponents figures
                    case game.lastClickedCell of
                        Just previousPosition ->
                            -- One of my figures was selected before
                            let
                                move =
                                    ( Tuple.first position - Tuple.first previousPosition, Tuple.second position - Tuple.second previousPosition )
                            in
                            case
                                ( (Tuple.first game.myCards).moves
                                    |> List.member move
                                , (Tuple.second game.myCards).moves
                                    |> List.member move
                                )
                            of
                                ( True, True ) ->
                                    ( { game | chooseCard = Just ( previousPosition, move ) }, Cmd.none )

                                ( True, False ) ->
                                    ( game
                                    , sendNewGameMove { color = game.myColor, card = Tuple.first game.myCards, from = previousPosition, move = move }
                                    )

                                ( False, True ) ->
                                    ( game
                                    , sendNewGameMove { color = game.myColor, card = Tuple.second game.myCards, from = previousPosition, move = move }
                                    )

                                ( False, False ) ->
                                    ( game, Cmd.none )

                        Nothing ->
                            -- none of my own figures was selected before
                            ( { game | lastClickedCell = Nothing }, Cmd.none )

            else
                ( game, Cmd.none )


newCards : List Card -> Game -> Game
newCards shuffledCards game =
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
        , commonCard =
            case List.head <| List.drop 4 <| shuffledCards of
                Just c ->
                    c

                Nothing ->
                    dummyCard
    }
        |> (if game.myColor == Black then
                flipCards

            else
                identity
           )


sendNewGameMove : GameMove -> Cmd Msg
sendNewGameMove gameMove =
    (gameMove
        |> (if gameMove.color == Black then
                rotateFromMove

            else
                identity
           )
        |> NewGameMove
    )
        |> send


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


rotateCardMove : GameMove -> GameMove
rotateCardMove { color, from, move, card } =
    let
        ( move_x, move_y ) =
            move
    in
    { color = color
    , from = from
    , move = ( negate move_x, negate move_y )
    , card = card
    }


rotateFromMove : GameMove -> GameMove
rotateFromMove { color, from, move, card } =
    let
        ( from_x, from_y ) =
            from
    in
    { color = color
    , from = ( 4 - from_x, 4 - from_y )
    , move = move
    , card = card
    }


{-| The Move happens here

    - check if there is one of my figures on `from`
    - check if the move is possible (cannt move outside the grid and can't move to `Out`)
    - check if I have the card
    -> If all testes pass, then move my figure to the new pos and swap the cards
       Eat opponents figures if they are on `from + move`
    -> Do nothing if one test fails (all or nothing)

-}
executeGameMove : GameMove -> Game -> Game
executeGameMove { color, from, move, card } game =
    let
        ( from_x, from_y ) =
            from

        ( move_x, move_y ) =
            move

        to =
            ( from_x + move_x, from_y + move_y )

        myNewFigures =
            game.myFigures
                |> List.map
                    (\f ->
                        if f.pos == On ( from_x, from_y ) then
                            -- move figure from from to newposition
                            { f | pos = On to }

                        else
                            f
                    )

        opNewFigures =
            game.opFigures
                |> List.map
                    (\f ->
                        if f.pos == On to then
                            -- eat opponents figure
                            { f | pos = Out }

                        else
                            f
                    )

        ( myFirstCard, mySecondCard ) =
            ( Tuple.first game.myCards, Tuple.second game.myCards )
    in
    if
        List.member from (figurePositions game.myFigures)
            && List.member to grid
            && (myFirstCard == card || mySecondCard == card)
    then
        { game
            | lastClickedCell = Nothing
            , myFigures = myNewFigures
            , opFigures = opNewFigures
            , chooseCard = Nothing
            , commonCard = card
            , myCards =
                if Tuple.first game.myCards == card then
                    ( game.commonCard, mySecondCard )

                else
                    ( myFirstCard, game.commonCard )
            , nextColor = invert color
        }

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



-- INIT




setupNewGame : Color -> Color -> Game
setupNewGame playerColor nextColor =
    Game playerColor
        nextColor
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
        Nothing
        Nothing


giveNewCards : Cmd Msg
giveNewCards =
    Random.generate GotNewCards chooseFiveCards

