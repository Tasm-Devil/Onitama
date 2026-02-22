port module Ports exposing
    ( closeGameStream
    , closeLobbyStream
    , gameEventReceived
    , lobbyEventReceived
    , openGameStream
    , openLobbyStream
    , playSound
    )

import Json.Encode as Encode



-- SSE PORTS (Server-Sent Events)
-- Open SSE connection for lobby updates


port openLobbyStream : () -> Cmd msg



-- Close lobby SSE connection


port closeLobbyStream : () -> Cmd msg



-- Receive lobby events from SSE
-- Events: gameCreated, playerJoined, gameStarted, gameEnded


port lobbyEventReceived : (Encode.Value -> msg) -> Sub msg



-- Open SSE connection for game updates (pass gameId)


port openGameStream : Int -> Cmd msg



-- Close game SSE connection


port closeGameStream : () -> Cmd msg



-- Receive game events from SSE
-- Events: move, concede


port gameEventReceived : (Encode.Value -> msg) -> Sub msg



-- SOUND PORTS


port playSound : String -> Cmd msg
