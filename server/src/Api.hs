{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Game (Card, Color, Game (..), GameMove)
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

-- SSE Content Type
data EventStream = EventStream

instance Accept EventStream where
  contentType _ = "text" // "event-stream"

type GameId = Int

newtype SessionToken = SessionToken Text
  deriving (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, Generic, ToJSON, FromJSON)

-- Game state with player names (for client display)
-- This is what clients receive, not the internal Game type with PlayerIds
data GameWithNames = GameWithNames
  { gameWhiteName :: Text,
    gameBlackName :: Text,
    gameCards :: [Card],
    gameHistory :: [GameMove],
    gameWinner :: Maybe Color,
    gameCreatedAt :: UTCTime,
    gameLastActivity :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON GameWithNames

instance FromJSON GameWithNames

-- Response when joining a game includes game data, session token, and player name
data JoinGameResponse = JoinGameResponse
  { responseGame :: GameWithNames,
    responseToken :: SessionToken,
    responsePlayerName :: Text -- Server explicitly tells client which player they are
  }
  deriving (Show, Generic)

-- Errors that can occur when joining a game
data JoinError
  = JEGameNotFound
  | JEGameFull
  | JEInvalidToken
  | JENameTaken
  | JEInvalidName
  deriving (Eq, Show, Generic)

-- Errors that can occur when submitting a move
data MoveError
  = MEInvalidToken
  | MENotYourTurn
  | MEGameNotFound
  | MEGameOver
  | MEInvalidMove
  deriving (Eq, Show, Generic)

-- Errors that can occur when conceding
data ConcedeError
  = CEInvalidToken
  | CEGameNotFound
  | CEAlreadyEnded
  deriving (Eq, Show, Generic)

-- Lightweight game summary for listing games
data GameSummary = GameSummary
  { summaryId :: GameId,
    summaryPlayer1 :: String,
    summaryPlayer2 :: String,
    summaryMoveCount :: Int,
    summaryStatus :: GameStatus,
    summaryCreatedAt :: UTCTime,
    summaryLastActivity :: UTCTime
  }
  deriving (Show, Eq, Generic)

data GameStatus = WaitingForPlayers | InProgress | Completed
  deriving (Show, Eq, Generic)

instance ToJSON JoinGameResponse

instance FromJSON JoinGameResponse

instance ToJSON JoinError

instance FromJSON JoinError

instance ToJSON MoveError

instance FromJSON MoveError

instance ToJSON ConcedeError

instance FromJSON ConcedeError

instance ToJSON GameSummary

instance FromJSON GameSummary

instance ToJSON GameStatus

instance FromJSON GameStatus

-- Create a game summary from game data with player names
gameToSummary :: GameId -> Text -> Text -> Game -> GameSummary
gameToSummary gameId whiteName blackName (Game maybeWhiteId maybeBlackId _ history maybeWinner created lastAct) =
  GameSummary
    { summaryId = gameId,
      summaryPlayer1 = if isNothing maybeWhiteId then "" else T.unpack whiteName,
      summaryPlayer2 = if isNothing maybeBlackId then "" else T.unpack blackName,
      summaryMoveCount = Prelude.length history,
      summaryStatus = determineStatus maybeWhiteId maybeBlackId maybeWinner,
      summaryCreatedAt = created,
      summaryLastActivity = lastAct
    }
  where
    determineStatus _ _ (Just _) = Completed
    determineStatus Nothing Nothing Nothing = WaitingForPlayers
    determineStatus Nothing _ Nothing = WaitingForPlayers
    determineStatus _ Nothing Nothing = WaitingForPlayers
    determineStatus _ _ Nothing = InProgress

-- Request body for joining a game
newtype JoinRequest
  = JoinRequest {joinPlayerName :: String}
  deriving (Show, Generic)

instance ToJSON JoinRequest

instance FromJSON JoinRequest

-- RESTful API structure: /1/onitama/games/...
type NewGame = "1" :> "onitama" :> "games" :> Post '[JSON] GameId

type GetGameSummaries = "1" :> "onitama" :> "games" :> Get '[JSON] [GameSummary]

type JoinGame = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> "players" :> Header "X-Session-Token" SessionToken :> ReqBody '[JSON] JoinRequest :> Post '[JSON] (Either JoinError JoinGameResponse)

type GetGame = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> Get '[JSON] GameWithNames

type NewMove = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> "moves" :> Header "X-Session-Token" SessionToken :> ReqBody '[JSON] GameMove :> Post '[JSON] (Either MoveError GameMove)

type Concede = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> "concede" :> Header "X-Session-Token" SessionToken :> Post '[JSON] (Either ConcedeError Color)

-- SSE Streaming endpoints (using Raw for WAI-level streaming)
type LobbyStream = "1" :> "onitama" :> "games" :> "stream" :> Raw

type GameStream = "1" :> "onitama" :> "games" :> Capture "gameId" GameId :> "stream" :> Raw

type NewGamePage = "newgame" :> Get '[HTML] RawHtml

type Index = Capture "gameid" GameId :> Get '[HTML] RawHtml

type API = NewGame :<|> GetGameSummaries :<|> JoinGame :<|> GetGame :<|> NewMove :<|> Concede :<|> LobbyStream :<|> GameStream :<|> NewGamePage :<|> Index

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