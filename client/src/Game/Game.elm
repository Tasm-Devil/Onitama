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



-- MODEL


type GameState
    = Thinking
    | FigureSelected ( Int, Int )
    | ChosingCard ( ( Int, Int ), ( Int, Int ) )
    | MoveDone GameMove


type alias Game =
    { myColor : Color
    , nextColor : Color
    , myFigures : List Figure
    , opFigures : List Figure
    , myCards : ( Card, Card )
    , opCards : ( Card, Card )
    , commonCard : Card
    , state : GameState
    }


{-| GameMove

    - The from field is the start position and is always seen from the perspective of the grid. Bottom left cell is (0,0) for white and black.
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
                    ++ (case game.state of
                            FigureSelected ( u, v ) ->
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

                            _ ->
                                []
                       )
            ]
        , Svg.g [ SvgA.class "cards-group" ]
            (drawAllCards (Tuple.first game.myCards)
                (Tuple.second game.myCards)
                (Tuple.first game.opCards)
                (Tuple.second game.opCards)
                game.commonCard
                (game.nextColor /= game.myColor)
            )
        , Svg.g
            [ SvgA.class "card-prompt"
            , SvgA.display
                (case game.state of
                    ChosingCard ( _, _ ) ->
                        "block"

                    _ ->
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
    = UserChoseOneCard Card
    | UserClickedOnCell ( Int, Int )
    | NewGameMove GameMove


update : Msg -> Game -> Game
update msg game =
    case msg of
        UserChoseOneCard card ->
            case game.state of
                ChosingCard ( from, move ) ->
                    { game | state = MoveDone { color = game.myColor, card = card, from = from, move = move } }

                _ ->
                    game

        UserClickedOnCell ( x, y ) ->
            if game.myColor == game.nextColor then
                handleClick ( x, y ) game

            else
                game

        NewGameMove gm ->
            { game | state = MoveDone gm }
                |> execGameMove


handleClick : ( Int, Int ) -> Game -> Game
handleClick ( x, y ) game =
    if game.myFigures |> figurePositions |> List.member ( x, y ) then
        -- There is one of my own figures on this position
        case game.state of
            FigureSelected ( u, v ) ->
                if ( x, y ) == ( u, v ) then
                    -- my figure was allready selected -> Toggle it
                    { game | state = Thinking }

                else
                    -- select another figure
                    { game | state = FigureSelected ( x, y ) }

            Thinking ->
                -- none of my fugures was selected -> select this one
                { game | state = FigureSelected ( x, y ) }

            _ ->
                game

    else
        -- There is none of my own figures on this position -> move and/or eat opponents figures
        case game.state of
            FigureSelected ( u, v ) ->
                -- One of my figures was selected before
                let
                    move =
                        ( x - u, y - v )
                in
                case
                    ( (Tuple.first game.myCards).moves
                        |> List.member move
                    , (Tuple.second game.myCards).moves
                        |> List.member move
                    )
                of
                    ( True, True ) ->
                        { game | state = ChosingCard ( ( u, v ), move ) }

                    ( True, False ) ->
                        { game | state = MoveDone { color = game.myColor, card = Tuple.first game.myCards, from = ( u, v ), move = move } }

                    ( False, True ) ->
                        { game | state = MoveDone { color = game.myColor, card = Tuple.second game.myCards, from = ( u, v ), move = move } }

                    ( False, False ) ->
                        game

            _ ->
                game


rotateGameMove : GameMove -> GameMove
rotateGameMove { color, from, move, card } =
    let
        ( move_x, move_y ) =
            move

        ( from_x, from_y ) =
            from
    in
    { color = color
    , from = ( 4 - from_x, 4 - from_y )
    , move = ( negate move_x, negate move_y )
    , card = card
    }


execGameMove : Game -> Game
execGameMove game =
    case game.state of
        MoveDone gameMove ->
            if gameMove.color == game.nextColor then
                case ( game.myColor, gameMove.color ) of
                    ( White, White ) ->
                        game
                            |> moveFigures gameMove

                    ( White, Black ) ->
                        game
                            |> flipFigures
                            |> flipCards
                            |> moveFigures (gameMove |> rotateGameMove)
                            |> flipFigures
                            |> flipCards

                    ( Black, White ) ->
                        game
                            |> flipFigures
                            |> flipCards
                            |> moveFigures (gameMove |> rotateGameMove)
                            |> flipFigures
                            |> flipCards

                    ( Black, Black ) ->
                        game
                            |> moveFigures gameMove

            else
                { game | state = Thinking }

        _ ->
            game


{-| The Move happens here

    - check if there is one of my figures on `from`
    - check if the move is possible (cannt move outside the grid and can't move to `Out`)
    - check if I have the card
    -> If all testes pass, then move my figure to the new pos and swap the cards
       Eat opponents figures if they are on `from + move`
    -> Do nothing if one test fails (all or nothing)

-}
moveFigures : GameMove -> Game -> Game
moveFigures { color, from, move, card } game =
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
                        if f.pos == ( from_x, from_y ) then
                            -- move figure from from to newposition
                            { f | pos = to }

                        else
                            f
                    )

        opNewFigures =
            game.opFigures
                |> List.filter
                    (\f -> f.pos /= to)

        ( myFirstCard, mySecondCard ) =
            ( Tuple.first game.myCards, Tuple.second game.myCards )
    in
    if
        List.member from (figurePositions game.myFigures)
            && List.member to grid
            && (myFirstCard == card || mySecondCard == card)
    then
        { game
            | myFigures = myNewFigures
            , opFigures = opNewFigures
            , commonCard = card
            , myCards =
                if Tuple.first game.myCards == card then
                    ( game.commonCard, mySecondCard )

                else
                    ( myFirstCard, game.commonCard )
            , nextColor = invert color
            , state = Thinking
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


setupNewGame : List Card -> Color -> Color -> Game
setupNewGame cards playerColor nextColor =
    Game playerColor
        nextColor
        [ { kind = Pawn 1, pos = ( 0, 0 ) }
        , { kind = Pawn 2, pos = ( 1, 0 ) }
        , { kind = King, pos = ( 2, 0 ) }
        , { kind = Pawn 3, pos = ( 3, 0 ) }
        , { kind = Pawn 4, pos = ( 4, 0 ) }
        ]
        [ { kind = Pawn 4, pos = ( 0, 4 ) }
        , { kind = Pawn 3, pos = ( 1, 4 ) }
        , { kind = King, pos = ( 2, 4 ) }
        , { kind = Pawn 2, pos = ( 3, 4 ) }
        , { kind = Pawn 1, pos = ( 4, 4 ) }
        ]
        ( Maybe.withDefault dummyCard (List.head <| cards), Maybe.withDefault dummyCard (List.head <| List.drop 1 <| cards) )
        ( Maybe.withDefault dummyCard (List.head <| List.drop 2 <| cards), Maybe.withDefault dummyCard (List.head <| List.drop 3 <| cards) )
        (Maybe.withDefault dummyCard (List.head <| List.drop 4 <| cards))
        Thinking
        |> (if playerColor == Black then
                flipCards

            else
                identity
           )
