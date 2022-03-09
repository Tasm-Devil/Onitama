{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Game (Game, GameMove)
import Network.HTTP.Media ((//), (/:))
import Servant
  ( Accept (contentType),
    Capture,
    FromHttpApiData,
    Get,
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

newtype GameId = GameId UUID
  deriving (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, Generic, ToJSON, FromJSON)

type NewGame = "game" :> Post '[JSON] GameId -- Creat new Game with shuffle Cards and return gameId

type GetGames = "game" :> Get '[JSON] [GameId] -- Get all GameIds

type JoinGame = "game" :> Capture "gameid" GameId :> QueryParam "name" String :> Put '[JSON] (Maybe Game)

type GetGame = "game" :> Capture "gameid" GameId :> Get '[JSON] (Maybe Game) -- Get Current Game from gameId

type NewMove = "game" :> Capture "gameid" GameId :> ReqBody '[JSON] GameMove :> Post '[JSON] (Maybe GameMove)

type Index = Capture "gameid" GameId :> Get '[HTML] RawHtml

type API = NewGame :<|> GetGames :<|> JoinGame :<|> GetGame :<|> NewMove :<|> Index

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