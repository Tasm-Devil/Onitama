module Api exposing (JoinGameResponse, JoinError, Msg(..), ServerGame, getGameFromServer, getGameIdFromServer, getGameSummariesFromServer, joinGame, postNewGameMove, concede, gameMoveToString, stringToGameMove, joinErrorToString)

import Game.Card exposing (Card, cardByName)
import Game.Figure exposing (Color(..))
import Game.Game as Game exposing (GameState(..))
import Http
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Lobby exposing (GameId, Status(..),GameSummary, GameStatus(..))


-- MOVE STRING CONVERSION
-- Format: "<color>:<from><to>:<card>" e.g., "w:c1c3:tiger"


posToChess : ( Int, Int ) -> String
posToChess ( x, y ) =
    let
        col = String.fromChar (Char.fromCode (Char.toCode 'a' + x))
        row = String.fromInt (y + 1)
    in
    col ++ row


chessToPos : String -> Maybe ( Int, Int )
chessToPos str =
    case String.toList str of
        [ colChar, rowChar ] ->
            let
                x = Char.toCode colChar - Char.toCode 'a'
                y = Char.toCode rowChar - Char.toCode '1'
            in
            if x >= 0 && x <= 4 && y >= 0 && y <= 4 then
                Just ( x, y )
            else
                Nothing
        _ ->
            Nothing


gameMoveToString : Game.GameMove -> String
gameMoveToString { color, card, from, move } =
    let
        colorStr = case color of
            White -> "w"
            Black -> "b"
        fromStr = posToChess from
        toPos = ( Tuple.first from + Tuple.first move, Tuple.second from + Tuple.second move )
        toStr = posToChess toPos
        cardStr = String.toLower card.name
    in
    colorStr ++ ":" ++ fromStr ++ toStr ++ ":" ++ cardStr


stringToGameMove : String -> Maybe Game.GameMove
stringToGameMove str =
    case String.split ":" str of
        [ colorStr, positions, cardStr ] ->
            let
                maybeColor = case colorStr of
                    "w" -> Just White
                    "b" -> Just Black
                    _ -> Nothing

                maybeFromTo =
                    if String.length positions == 4 then
                        let
                            fromStr = String.left 2 positions
                            toStr = String.right 2 positions
                        in
                        Maybe.map2 Tuple.pair (chessToPos fromStr) (chessToPos toStr)
                    else
                        Nothing

                card = cardByName (capitalizeFirst cardStr)
            in
            case ( maybeColor, maybeFromTo ) of
                ( Just color, Just ( from, to ) ) ->
                    let
                        move = ( Tuple.first to - Tuple.first from, Tuple.second to - Tuple.second from )
                    in
                    Just { color = color, card = card, from = from, move = move }

                _ ->
                    Nothing

        _ ->
            Nothing


capitalizeFirst : String -> String
capitalizeFirst str =
    case String.uncons str of
        Just ( first, rest ) ->
            String.cons (Char.toUpper first) rest

        Nothing ->
            str


type alias ServerGame =
    { gameWhiteName : String
    , gameBlackName : String
    , gameCards : List Card
    , gameHistory : List Game.GameMove
    , gameWinner : Maybe Color
    }


type alias PlayerToken =
    String


type alias JoinGameResponse =
    { responseGame : ServerGame
    , responseToken : PlayerToken
    , responsePlayerName : String  -- Server explicitly tells us who we are
    }


type JoinError
    = JEGameNotFound
    | JEGameFull
    | JEInvalidToken
    | JENameTaken
    | JEInvalidName
    | JENetworkError String


joinErrorToString : JoinError -> String
joinErrorToString error =
    case error of
        JEGameNotFound ->
            "Game not found. It may have been deleted."

        JEGameFull ->
            "This game is full. Both players have already joined."

        JEInvalidToken ->
            "Your session expired. Please try again."

        JENameTaken ->
            "This name is already taken. Please choose another name."

        JEInvalidName ->
            "Please enter a valid name (at least 1 character)."

        JENetworkError msg ->
            "Network error: " ++ msg


type Msg
    = ReceivedGameIdFromServer (Result Http.Error GameId) -- the game id of the new game
    | ReceivedJoinGameResponse (Result Http.Error (Result JoinError JoinGameResponse))
    | ReceivedGameFromServer (Result Http.Error ServerGame)
    | ReceivedPostCreatedFromServer (Result Http.Error Game.GameMove)
    | ReceivedGameSummariesFromServer (Result Http.Error (List GameSummary)) -- lightweight game summaries
    | ReceivedConcedeResponse (Result Http.Error (Maybe Color))



-- HTTP


getGameSummariesFromServer : Cmd Msg
getGameSummariesFromServer =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Cache-Control" "no-cache, no-store, must-revalidate"
            , Http.header "Pragma" "no-cache"
            ]
        , url = "/1/onitama/summary"
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedGameSummariesFromServer (Decode.list decodeGameSummary)
        , timeout = Nothing
        , tracker = Nothing
        }


getGameIdFromServer : Cmd Msg
getGameIdFromServer =
    Http.post
        { url = "/1/onitama/new"
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedGameIdFromServer Decode.int
        }


joinGame : GameId -> String -> Maybe PlayerToken -> Cmd Msg
joinGame gameid name maybeToken =
    let
        tokenHeader =
            case maybeToken of
                Just token ->
                    [ Http.header "X-Session-Token" token ]

                Nothing ->
                    []

        -- Decode Either JoinError JoinGameResponse
        -- Server returns: {"Left": "JENameTaken"} or {"Right": {...}}
        eitherDecoder =
            Decode.oneOf
                [ Decode.field "Right" decodeJoinGameResponse |> Decode.map Ok
                , Decode.field "Left" decodeJoinError |> Decode.map Err
                ]
    in
    Http.request
        { method = "PUT"
        , headers = tokenHeader
        , url = "/1/onitama?table=" ++ String.fromInt gameid ++ "&name=" ++ name
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedJoinGameResponse eitherDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getGameFromServer : GameId -> Cmd Msg
getGameFromServer gameid =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Cache-Control" "no-cache, no-store, must-revalidate"
            , Http.header "Pragma" "no-cache"
            ]
        , url = "/1/onitama?table=" ++ String.fromInt gameid
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedGameFromServer decodeGame
        , timeout = Nothing
        , tracker = Nothing
        }


postNewGameMove : GameId -> PlayerToken -> Game.GameMove -> Cmd Msg
postNewGameMove gameid token gameMove =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Session-Token" token ]
        , url = "/1/onitama?table=" ++ String.fromInt gameid
        , body = Http.jsonBody (encodeGameMove gameMove)
        , expect = Http.expectJson ReceivedPostCreatedFromServer decodeGameMove
        , timeout = Nothing
        , tracker = Nothing
        }


concede : GameId -> PlayerToken -> Cmd Msg
concede gameid token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Session-Token" token ]
        , url = "/1/onitama/concede?table=" ++ String.fromInt gameid
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedConcedeResponse (Decode.nullable decodeColor)
        , timeout = Nothing
        , tracker = Nothing
        }


-- DECODERS


decodeJoinError : Decoder JoinError
decodeJoinError =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "JEGameNotFound" ->
                        Decode.succeed JEGameNotFound

                    "JEGameFull" ->
                        Decode.succeed JEGameFull

                    "JEInvalidToken" ->
                        Decode.succeed JEInvalidToken

                    "JENameTaken" ->
                        Decode.succeed JENameTaken

                    "JEInvalidName" ->
                        Decode.succeed JEInvalidName

                    _ ->
                        Decode.fail ("Unknown join error: " ++ str)
            )


decodeColor : Decoder Color
decodeColor =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "White" ->
                        Decode.succeed White

                    "Black" ->
                        Decode.succeed Black

                    _ ->
                        Decode.fail ("Unknown color: " ++ str)
            )


decodeGameStatus : Decoder GameStatus
decodeGameStatus =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "WaitingForPlayers" ->
                        Decode.succeed WaitingForPlayers

                    "InProgress" ->
                        Decode.succeed InProgress

                    "Completed" ->
                        Decode.succeed Completed

                    _ ->
                        Decode.fail ("Unknown game status: " ++ str)
            )


decodeGameSummary : Decoder GameSummary
decodeGameSummary =
    Decode.succeed GameSummary
        |> required "summaryId" Decode.int
        |> required "summaryPlayer1" Decode.string
        |> required "summaryPlayer2" Decode.string
        |> required "summaryMoveCount" Decode.int
        |> required "summaryStatus" decodeGameStatus


decodeGameMove : Decoder Game.GameMove
decodeGameMove =
    Decode.string
        |> Decode.andThen
            (\str ->
                case stringToGameMove str of
                    Just gameMove ->
                        Decode.succeed gameMove

                    Nothing ->
                        Decode.fail ("Invalid game move format: " ++ str)
            )


decodeGame : Decoder ServerGame
decodeGame =
    Decode.succeed ServerGame
        |> required "gameWhiteName" Decode.string
        |> required "gameBlackName" Decode.string
        |> required "gameCards" (Decode.list (Decode.map Game.Card.cardByName Decode.string))
        |> required "gameHistory" (Decode.list decodeGameMove)
        |> required "gameWinner" (Decode.nullable decodeColor)


decodeJoinGameResponse : Decoder JoinGameResponse
decodeJoinGameResponse =
    Decode.succeed JoinGameResponse
        |> required "responseGame" decodeGame
        |> required "responseToken" Decode.string
        |> required "responsePlayerName" Decode.string


encodeGameMove : Game.GameMove -> Encode.Value
encodeGameMove gameMove =
    Encode.string (gameMoveToString gameMove)
