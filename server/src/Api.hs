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
import Data.Text (Text)
import GHC.Generics (Generic)
import Game (Game (..), GameMove, Color)
import Network.HTTP.Media ((//), (/:))
import Servant
  ( Accept (contentType),
    Capture,
    FromHttpApiData,
    Get,
    Header,
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

newtype GameId = GameId Int
  deriving (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Num)

newtype SessionToken = SessionToken Text
  deriving (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, Generic, ToJSON, FromJSON)

-- Response when joining a game includes both game state and session token
data JoinGameResponse = JoinGameResponse
  { responseGame :: Game
  , responseToken :: SessionToken
  } deriving (Show, Generic)

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

instance ToJSON JoinGameResponse
instance FromJSON JoinGameResponse
instance ToJSON GameSummary
instance FromJSON GameSummary
instance ToJSON GameStatus
instance FromJSON GameStatus

-- Create a game summary from a full game
gameToSummary :: GameId -> Game -> GameSummary
gameToSummary gameId (Game p1 p2 _ history maybeWinner) =
  GameSummary
    { summaryId = gameId,
      summaryPlayer1 = p1,
      summaryPlayer2 = p2,
      summaryMoveCount = Prelude.length history,
      summaryStatus = determineStatus p1 p2 maybeWinner
    }
  where
    determineStatus _ _ (Just _) = Completed
    determineStatus "" "" Nothing = WaitingForPlayers
    determineStatus "" _ Nothing = WaitingForPlayers
    determineStatus _ "" Nothing = WaitingForPlayers
    determineStatus _ _ Nothing = InProgress

type Games = Map GameId Game

-- New API structure: /1/onitama/...
type NewGame = "1" :> "onitama" :> "new" :> Post '[JSON] GameId

type GetGameSummaries = "1" :> "onitama" :> "summary" :> Get '[JSON] [GameSummary]

type JoinGame = "1" :> "onitama" :> QueryParam "table" GameId :> QueryParam "name" String :> Header "X-Session-Token" SessionToken :> Put '[JSON] (Maybe JoinGameResponse)

type GetGame = "1" :> "onitama" :> QueryParam "table" GameId :> Get '[JSON] (Maybe Game)

type NewMove = "1" :> "onitama" :> QueryParam "table" GameId :> Header "X-Session-Token" SessionToken :> ReqBody '[JSON] GameMove :> Post '[JSON] (Maybe GameMove)

type Concede = "1" :> "onitama" :> "concede" :> QueryParam "table" GameId :> Header "X-Session-Token" SessionToken :> Post '[JSON] (Maybe Color)

type Index = Capture "gameid" GameId :> Get '[HTML] RawHtml

type API = NewGame :<|> GetGameSummaries :<|> JoinGame :<|> GetGame :<|> NewMove :<|> Concede :<|> Index

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