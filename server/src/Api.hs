{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Api where

import Game ( GameMove, Game )
import Servant
import qualified Data.Map.Strict as Map
import Data.Map ( Map )
import Data.Aeson.TH (defaultOptions, deriveJSON)


type NewGame = Post '[JSON] GameId -- Creat new Game with shuffle Cards and return gameId

type GetGame = Capture "gameid" GameId :> Get '[JSON] Game -- Get Current Game from gameId

type NewMove = Capture "gameid" GameId :> ReqBody '[JSON] GameMove :> Post '[JSON] GameMove

type API = "game" :> (NewGame :<|> GetGame :<|> NewMove)

api :: Proxy API
api = Proxy

newtype GameId = GameId Int
  deriving (Num, Show, Eq, Ord, Enum, FromHttpApiData, ToHttpApiData)

$(deriveJSON defaultOptions ''GameId)

