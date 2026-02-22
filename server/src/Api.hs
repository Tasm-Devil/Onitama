{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.ByteString.Lazy as Lazy (ByteString)
import Network.HTTP.Media ((//), (/:))
import Servant (Accept (contentType), Capture, Get, Header, JSON, MimeRender (..), Post, Proxy (..), ReqBody, type (:<|>), type (:>))
import Servant.API (Accept (..), Raw)
import Types

-- SSE Content Type
data EventStream = EventStream

instance Accept EventStream where
  contentType _ = "text" // "event-stream"

-- RESTful API structure: /1/onitama/games/...
type NewGame = "1" :> "onitama" :> "games" :> Header "X-Session-Token" SessionToken :> ReqBody '[JSON] NewGameRequest :> Post '[JSON] (Either JoinError NewGameResponse)

type GetGameSummaries = "1" :> "onitama" :> "games" :> Get '[JSON] [GameSummary]

type JoinGame = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> "players" :> Header "X-Session-Token" SessionToken :> ReqBody '[JSON] JoinRequest :> Post '[JSON] (Either JoinError JoinGameResponse)

type GetGame = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> Get '[JSON] GameWithNames

type NewMove = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> "moves" :> Header "X-Session-Token" SessionToken :> ReqBody '[JSON] MoveNotation :> Post '[JSON] (Either MoveError MoveNotation)

type Concede = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> "concede" :> Header "X-Session-Token" SessionToken :> Post '[JSON] (Either ConcedeError Color)

-- SSE Streaming endpoints (using Raw for WAI-level streaming)
type LobbyStream = "1" :> "onitama" :> "games" :> "stream" :> Raw

type GameStream = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> "stream" :> Raw

type Index = Capture "gameid" GameId :> Get '[HTML] RawHtml

type API = NewGame :<|> GetGameSummaries :<|> JoinGame :<|> GetGame :<|> NewMove :<|> Concede :<|> LobbyStream :<|> GameStream :<|> Index

api :: Proxy API
api = Proxy

type APIWithAssets = API :<|> Raw

apiWithAssets :: Proxy APIWithAssets
apiWithAssets = Proxy

-- https://mmhaskell.com/blog/2020/3/23/serving-html-with-servant
data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw
