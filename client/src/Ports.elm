port module Ports exposing (saveSession, loadSession, clearSession)

import Json.Encode as Encode


-- Save session to localStorage
-- Expects: { gameId: Int, playerName: String, token: String }
port saveSession : Encode.Value -> Cmd msg


-- Load session from localStorage on startup
-- This is called automatically by JavaScript on page load
port loadSession : (Encode.Value -> msg) -> Sub msg


-- Clear a specific session
port clearSession : Encode.Value -> Cmd msg
