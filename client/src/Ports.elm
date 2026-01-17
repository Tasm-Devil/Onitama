port module Ports exposing (loadPlayers, savePlayer)

import Json.Encode as Encode



-- Save player identity to localStorage (adds or updates)
-- Expects: { playerName: String, token: String }


port savePlayer : Encode.Value -> Cmd msg



-- Load all player identities from localStorage on startup
-- Receives: [{ playerName: String, token: String }, ...]
-- This is called automatically by JavaScript on page load


port loadPlayers : (Encode.Value -> msg) -> Sub msg
