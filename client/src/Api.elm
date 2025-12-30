module Api exposing (Msg(..), ServerGame, getGameFromServer, getGameIdFromServer, getGameSummariesFromServer, joinGame, postNewGameMove)

import Game.Card exposing (Card)
import Game.Figure exposing (Color(..))
import Game.Game as Game exposing (GameState(..))
import Http
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Lobby exposing (GameId, Status(..),GameSummary, GameStatus(..))


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



-- HTTP


getGameSummariesFromServer : Cmd Msg
getGameSummariesFromServer =
    Http.get
        { url = "/1/onitama/summary"
        , expect = Http.expectJson ReceivedGameSummariesFromServer (Decode.list decodeGameSummary)
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
    Http.get
        { url = "/1/onitama?table=" ++ String.fromInt gameid
        , expect =
            Http.expectJson ReceivedGameFromServer decodeGame
        }


postNewGameMove : GameId -> SessionToken -> Game.GameMove -> Cmd Msg
postNewGameMove gameid token gameMove =
    Http.post
        { url = "/1/onitama?table=" ++ String.fromInt gameid ++ "&token=" ++ token
        , body = Http.jsonBody (enecodergameMove gameMove)
        , expect = Http.expectJson ReceivedPostCreatedFromServer decodeGameMove
        }



-- DECODERS


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


-- DECODERS


decodeTuple : Decoder ( Int, Int )
decodeTuple =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)


decodeGameMove : Decoder Game.GameMove
decodeGameMove =
    Decode.succeed Game.GameMove
        |> required "color" (Decode.map Game.Figure.colorFromString Decode.string)
        |> required "card" (Decode.map Game.Card.cardByName Decode.string)
        |> required "from" decodeTuple
        |> required "move" decodeTuple


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


enecodergameMove : Game.GameMove -> Encode.Value
enecodergameMove gameMove =
    Encode.object
        [ ( "color", Encode.string (Game.Figure.colorToString gameMove.color) )
        , ( "card", Encode.string gameMove.card.name )
        , ( "from", Encode.list Encode.int [ Tuple.first gameMove.from, Tuple.second gameMove.from ] )
        , ( "move", Encode.list Encode.int [ Tuple.first gameMove.move, Tuple.second gameMove.move ] )
        ]
