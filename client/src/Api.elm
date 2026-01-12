module Api exposing (JoinGameResponse, Msg(..), ServerGame, getGameFromServer, getGameIdFromServer, getGameSummariesFromServer, joinGame, postNewGameMove, concede, gameMoveToString, stringToGameMove)

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
    { player_white : String
    , player_black : String
    , cards : List Card
    , history : List Game.GameMove
    }


type alias SessionToken =
    String


type alias JoinGameResponse =
    { responseGame : ServerGame
    , responseToken : SessionToken
    }



type Msg
    = ReceivedGameIdFromServer (Result Http.Error GameId) -- the game id of the new game
    | ReceivedJoinGameResponse (Result Http.Error JoinGameResponse)
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


joinGame : GameId -> String -> Maybe SessionToken -> Cmd Msg
joinGame gameid name maybeToken =
    let
        tokenParam =
            case maybeToken of
                Just token ->
                    "&token=" ++ token
                
                Nothing ->
                    ""
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/1/onitama?table=" ++ String.fromInt gameid ++ "&name=" ++ name ++ tokenParam
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedJoinGameResponse (Decode.nullable decodeJoinGameResponse |> Decode.andThen (\maybeResponse -> 
            case maybeResponse of
                Just response -> Decode.succeed response
                Nothing -> Decode.fail "Server returned null - game not found or full"
            ))
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


postNewGameMove : GameId -> SessionToken -> Game.GameMove -> Cmd Msg
postNewGameMove gameid token gameMove =
    Http.post
        { url = "/1/onitama?table=" ++ String.fromInt gameid ++ "&token=" ++ token
        , body = Http.jsonBody (encodeGameMove gameMove)
        , expect = Http.expectJson ReceivedPostCreatedFromServer decodeGameMove
        }


concede : GameId -> SessionToken -> Cmd Msg
concede gameid token =
    Http.post
        { url = "/1/onitama/concede?table=" ++ String.fromInt gameid ++ "&token=" ++ token
        , body = Http.emptyBody
        , expect = Http.expectJson ReceivedConcedeResponse (Decode.nullable decodeColor)
        }


-- DECODERS


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
        |> required "player_white" Decode.string
        |> required "player_black" Decode.string
        |> required "cards" (Decode.list (Decode.map Game.Card.cardByName Decode.string))
        |> required "history" (Decode.list decodeGameMove)


decodeJoinGameResponse : Decoder JoinGameResponse
decodeJoinGameResponse =
    Decode.succeed JoinGameResponse
        |> required "responseGame" decodeGame
        |> required "responseToken" Decode.string


encodeGameMove : Game.GameMove -> Encode.Value
encodeGameMove gameMove =
    Encode.string (gameMoveToString gameMove)
