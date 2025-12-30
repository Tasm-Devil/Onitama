{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Game (Game, GameMove)
import Network.HTTP.Media ((//), (/:))
import Servant
  ( Accept (contentType),
    Capture,
    FromHttpApiData,
    Get,
    JSON,
    MimeRender (..),
    Post,
    Proxy (..),
    Put,
    QueryParam,
    ReqBody,
    ToHttpApiData,
    type (:<|>),
    type (:>),
  )
import Servant.API (Accept (..), Raw)

newtype GameId = GameId UUID
  deriving (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- Lightweight game summary for listing games
data GameSummary = GameSummary
  { summaryId :: GameId
  , summaryPlayer1 :: String
  , summaryPlayer2 :: String
  , summaryMoveCount :: Int
  , summaryStatus :: GameStatus
  } deriving (Show, Eq, Generic)

data GameStatus = WaitingForPlayers | InProgress | Completed
  deriving (Show, Eq, Generic)

instance ToJSON GameSummary
instance FromJSON GameSummary
instance ToJSON GameStatus
instance FromJSON GameStatus

type Games = Map GameId Game

type NewGame = "game" :> Post '[JSON] GameId -- Creat new Game with shuffle Cards and return gameId

type GetGameSummaries = "games" :> "summary" :> Get '[JSON] [GameSummary] -- Get lightweight game summaries

type JoinGame = "game" :> Capture "gameid" GameId :> QueryParam "name" String :> Put '[JSON] (Maybe Game)

type GetGame = "game" :> Capture "gameid" GameId :> Get '[JSON] (Maybe Game) -- Get Current Game from gameId

type NewMove = "game" :> Capture "gameid" GameId :> ReqBody '[JSON] GameMove :> Post '[JSON] (Maybe GameMove)

type Index = Capture "gameid" GameId :> Get '[HTML] RawHtml

type API = NewGame :<|> GetGameSummaries :<|> JoinGame :<|> GetGame :<|> NewMove :<|> Index

api :: Proxy API
api = Proxy

type APIWithAssets = API :<|> Raw

apiWithAssets :: Proxy APIWithAssets
apiWithAssets = Proxy

--https://mmhaskell.com/blog/2020/3/23/serving-html-with-servant
data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw