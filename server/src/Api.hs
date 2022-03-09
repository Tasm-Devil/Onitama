{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api where

import Game ( GameMove, Game )
import GHC.Generics ( Generic )
import Servant
    ( Put,
      QueryParam,
      Post,
      Get,
      type (:>),
      ReqBody,
      JSON,
      Capture,
      type (:<|>),
      ToHttpApiData,
      FromHttpApiData,
      Proxy(..) )
import qualified Data.Map.Strict as Map
import Data.Aeson ( FromJSON, ToJSON )
import Data.Map ( Map )
import Data.UUID (UUID)


type NewGame = Post '[JSON] GameId -- Creat new Game with shuffle Cards and return gameId

type GetGames = Get '[JSON] [GameId] -- Get all GameIds

type JoinGame = Capture "gameid" GameId :> QueryParam "name" String :> Put '[JSON] (Maybe Game)

type GetGame = Capture "gameid" GameId :> Get '[JSON] (Maybe Game) -- Get Current Game from gameId

type NewMove = Capture "gameid" GameId :> ReqBody '[JSON] GameMove :> Post '[JSON] (Maybe GameMove)

type API = "game" :> (NewGame :<|> GetGames :<|> JoinGame :<|> GetGame :<|> NewMove)

api :: Proxy API
api = Proxy

newtype GameId = GameId UUID
  deriving (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, Generic, ToJSON, FromJSON )


