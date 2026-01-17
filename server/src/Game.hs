{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Conc (TVar)
import GHC.Generics (Generic)
import System.Random (StdGen, newStdGen)
import System.Random.Shuffle (shuffle')

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

-- GameMove is now a simple string like "w:c1c3:tiger"
-- Format: <color>:<from><to>:<card>
-- This makes the server game-agnostic
type GameMove = String

data Game = Game
  { player_white :: Maybe PlayerId,
    player_black :: Maybe PlayerId,
    cards :: [Card],
    history :: [GameMove],
    winner :: Maybe Color
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

validCards :: [Card]
validCards =
  [ "Boar",
    "Cobra",
    "Crab",
    "Crane",
    "Dragon",
    "Eel",
    "Elephant",
    "Frog",
    "Goose",
    "Horse",
    "Mantis",
    "Monkey",
    "Ox",
    "Rabbit",
    "Rooster",
    "Tiger"
  ]

-- Which player starts when this card is the common card
cardStartPlayer :: Card -> Color
cardStartPlayer card = case card of
  "Boar" -> White
  "Cobra" -> White
  "Crab" -> Black
  "Crane" -> Black
  "Dragon" -> White
  "Eel" -> Black
  "Elephant" -> White
  "Frog" -> White
  "Goose" -> Black
  "Horse" -> White
  "Mantis" -> White
  "Monkey" -> Black
  "Ox" -> Black
  "Rabbit" -> Black
  "Rooster" -> White
  "Tiger" -> Black
  _ -> White -- default fallback

-- Determine which player slot should make the next move based on game history and common card
getCurrentPlayerSlot :: Game -> PlayerSlot
getCurrentPlayerSlot (Game _ _ cards history _) =
  let commonCard = if length cards >= 5 then cards !! 4 else ""
      startPlayer = cardStartPlayer commonCard
      moveCount = length history
   in case startPlayer of
        White -> if even moveCount then PlayerWhite else PlayerBlack
        Black -> if even moveCount then PlayerBlack else PlayerWhite

{-
moreCards :: [Card] -- Senseis Path
moreCards =
  [ "bear",
    "dog",
    "fox",
    "giraffe",
    "iguana",
    "kirin",
    "mouse",
    "otter",
    "panda",
    "phoenix",
    "rat",
    "sable",
    "sea_snake",
    "tanuki",
    "turtle",
    "viper"
  ]
-}

give5Cards :: IO [Card]
give5Cards = do
  rng <- newStdGen
  return . take 5 . shuffle' validCards (length validCards) $ rng

main :: IO ()
main = do
  a <- give5Cards
  print a
