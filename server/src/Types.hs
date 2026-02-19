{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON (..), ToJSON, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData)

data Color
  = White
  | Black
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

-- Player slot identifier: which position in the game
data PlayerSlot = PlayerWhite | PlayerBlack
  deriving (Show, Eq, Ord, Generic)

instance ToJSON PlayerSlot

instance FromJSON PlayerSlot

-- PlayerId is an internal database key (foreign key to dbPlayers)
type PlayerId = Int

type Card = String

-- MoveNotation format: "<color>:<from><to>:<card>" e.g. "w:c1c3:tiger"
type MoveNotation = String

type GameId = Int

newtype SessionToken = SessionToken Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON)

-- Persistence record for a game
data Game = Game
  { player_white :: Maybe PlayerId,
    player_black :: Maybe PlayerId,
    cards :: [Card],
    history :: [(MoveNotation, UTCTime)],
    winner :: Maybe Color,
    createdAt :: UTCTime,
    lastActivity :: UTCTime,
    aiDifficulty :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON Game where
  parseJSON = Aeson.withObject "Game" $ \v ->
    Game
      <$> v .: "player_white"
      <*> v .: "player_black"
      <*> v .: "cards"
      <*> v .: "history"
      <*> v .: "winner"
      <*> v .: "createdAt"
      <*> v .: "lastActivity"
      <*> v .:? "aiDifficulty"

addMoveToGame :: MoveNotation -> UTCTime -> Maybe Color -> Game -> Game
addMoveToGame move now maybeWinner g =
  g
    { history = (move, now) : history g,
      winner = maybeWinner,
      lastActivity = now
    }

-- Game state with player names (for client display)
-- This is what clients receive, not the internal Game type with PlayerIds
data GameWithNames = GameWithNames
  { gameWhiteName :: Text,
    gameBlackName :: Text,
    gameCards :: [Card],
    gameHistory :: [MoveNotation],
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

instance ToJSON JoinGameResponse

instance FromJSON JoinGameResponse

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
  | MEInvalidMove
  deriving (Eq, Show, Generic)

-- Errors that can occur when conceding
data ConcedeError
  = CEInvalidToken
  | CEGameNotFound
  | CEAlreadyEnded
  deriving (Eq, Show, Generic)

instance ToJSON JoinError

instance FromJSON JoinError

instance ToJSON MoveError

instance FromJSON MoveError

instance ToJSON ConcedeError

instance FromJSON ConcedeError

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

instance ToJSON GameSummary

instance FromJSON GameSummary

instance ToJSON GameStatus

instance FromJSON GameStatus

-- Create a game summary from game data with player names
gameToSummary :: GameId -> Text -> Text -> Game -> GameSummary
gameToSummary gameId whiteName blackName (Game maybeWhiteId maybeBlackId _ history maybeWinner created lastAct _) =
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

-- Request body for creating a new game
data NewGameRequest = NewGameRequest
  { newGamePlayerName :: String,
    newGameVsAI :: Bool
  }
  deriving (Show, Generic)

instance ToJSON NewGameRequest

instance FromJSON NewGameRequest

-- Response for creating a new game
data NewGameResponse = NewGameResponse
  { newGameId :: GameId,
    newGameJoinResponse :: JoinGameResponse
  }
  deriving (Show, Generic)

instance ToJSON NewGameResponse

instance FromJSON NewGameResponse
