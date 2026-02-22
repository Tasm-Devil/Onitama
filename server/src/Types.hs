{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData)

data Color
  = White
  | Black
  deriving (Eq, Read, Show, Generic, NFData, ToJSON, FromJSON)

type Card = String

-- MoveNotation format: "<color>:<from><to>:<card>" e.g. "w:c1c3:tiger"
type MoveNotation = String

type GameId = Int

-- OIDC user identity from Authelia headers
newtype OidcUserId = OidcUserId Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON)

data AuthUser = AuthUser
  { authUserId :: OidcUserId,
    authUserName :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuthUser

instance FromJSON AuthUser

-- Persistence record for a game
data Game = Game
  { player_white :: Maybe OidcUserId,
    player_black :: Maybe OidcUserId,
    player_white_name :: Text,
    player_black_name :: Text,
    cards :: [Card],
    history :: [(MoveNotation, UTCTime)],
    winner :: Maybe Color,
    createdAt :: UTCTime,
    lastActivity :: UTCTime,
    aiDifficulty :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

addMoveToGame :: MoveNotation -> UTCTime -> Maybe Color -> Game -> Game
addMoveToGame move now maybeWinner g =
  g
    { history = (move, now) : history g,
      winner = maybeWinner,
      lastActivity = now
    }

-- Game state with player names (for client display)
-- This is what clients receive, not the internal Game type
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

-- Response when joining a game
data JoinGameResponse = JoinGameResponse
  { responseGame :: GameWithNames,
    responsePlayerName :: Text
  }
  deriving (Show, Generic)

instance ToJSON JoinGameResponse

instance FromJSON JoinGameResponse

-- Errors that can occur when joining a game
data JoinError
  = JEGameNotFound
  | JEGameFull
  | JENotAuthenticated
  deriving (Eq, Show, Generic)

-- Errors that can occur when submitting a move
data MoveError
  = MENotYourTurn
  | MEGameNotFound
  | MEInvalidMove
  | MENotAuthenticated
  deriving (Eq, Show, Generic)

-- Errors that can occur when conceding
data ConcedeError
  = CEGameNotFound
  | CEAlreadyEnded
  | CENotAuthenticated
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

-- Create a game summary from game data (names from Game record directly)
gameToSummary :: GameId -> Game -> GameSummary
gameToSummary gameId game =
  GameSummary
    { summaryId = gameId,
      summaryPlayer1 = if isNothing (player_white game) then "" else T.unpack (player_white_name game),
      summaryPlayer2 = if isNothing (player_black game) then "" else T.unpack (player_black_name game),
      summaryMoveCount = Prelude.length (history game),
      summaryStatus = determineStatus (player_white game) (player_black game) (winner game),
      summaryCreatedAt = createdAt game,
      summaryLastActivity = lastActivity game
    }
  where
    determineStatus _ _ (Just _) = Completed
    determineStatus Nothing Nothing Nothing = WaitingForPlayers
    determineStatus Nothing _ Nothing = WaitingForPlayers
    determineStatus _ Nothing Nothing = WaitingForPlayers
    determineStatus _ _ Nothing = InProgress

-- Pure projection from Game to GameWithNames (no DB lookup needed)
gameToGameWithNames :: Game -> GameWithNames
gameToGameWithNames game =
  GameWithNames
    { gameWhiteName = player_white_name game,
      gameBlackName = player_black_name game,
      gameCards = cards game,
      gameHistory = map fst (history game),
      gameWinner = winner game,
      gameCreatedAt = createdAt game,
      gameLastActivity = lastActivity game
    }

data CardSet = BaseOnly | WithExpansion
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Request body for creating a new game
data NewGameRequest = NewGameRequest
  { newGameVsAI :: Bool,
    newGameCardSet :: CardSet
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
