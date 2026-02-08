{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game where

import Data.Aeson (FromJSON, ToJSON)
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

-- GameMove format: "<color>:<from><to>:<card>" e.g. "w:c1c3:tiger"
type GameMove = String

data Game = Game
  { player_white :: Maybe PlayerId,
    player_black :: Maybe PlayerId,
    cards :: [Card],
    history :: [(GameMove, UTCTime)],
    winner :: Maybe Color,
    createdAt :: UTCTime,
    lastActivity :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
