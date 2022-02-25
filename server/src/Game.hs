{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Game

where

import GHC.Generics (Generic)
import System.Random (StdGen, newStdGen)
import System.Random.Shuffle (shuffle')
import Data.Aeson.TH (defaultOptions, deriveJSON)
import GHC.Conc (TVar)


data Color
  = White
  | Black
  deriving (Eq, Read, Show, Generic)

$(deriveJSON defaultOptions ''Color)

type Card = String

data GameMove = GameMove
  { color :: Color,
    card :: Card,
    from :: (Integer, Integer),
    move :: (Integer, Integer)
  }
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''GameMove)

data Game = Game
  { cards :: [Card],
    history :: [GameMove]
  }
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Game)


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

give5Cards :: IO [Card]
give5Cards = do
  rng <- newStdGen
  return . take 5 . shuffle' validCards (length validCards) $ rng

main :: IO ()
main = do
  a <- give5Cards
  print a
