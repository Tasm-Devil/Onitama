{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Data.Aeson (FromJSON (..), ToJSON, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

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
