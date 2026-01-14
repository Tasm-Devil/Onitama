port module Ports exposing (savePlayer, loadPlayer)

import Json.Encode as Encode


-- Save player identity to localStorage
-- Expects: { playerName: String, token: String }
port savePlayer : Encode.Value -> Cmd msg


-- Load player identity from localStorage on startup
-- This is called automatically by JavaScript on page load
port loadPlayer : (Encode.Value -> msg) -> Sub msg
