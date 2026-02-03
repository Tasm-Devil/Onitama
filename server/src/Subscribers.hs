{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscribers
  ( SubscriberStore,
    LobbyEvent (..),
    GameEvent (..),
    newSubscriberStore,
    subscribeLobby,
    unsubscribeLobby,
    broadcastLobby,
    subscribeGame,
    unsubscribeGame,
    broadcastGame,
  )
where

import Api (GameId, GameSummary)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Game (Color, GameMove)

-- | Events broadcast to lobby subscribers
data LobbyEvent
  = GameCreated GameId GameSummary
  | PlayerJoined GameId Text Color -- gameId, playerName, color
  | GameStarted GameId
  | GameEnded GameId (Maybe Color) -- winner color, Nothing if draw/abandoned
  deriving (Show, Generic)

instance ToJSON LobbyEvent where
  toJSON (GameCreated gid summary) =
    object
      [ "event" .= ("gameCreated" :: Text),
        "gameId" .= gid,
        "summary" .= summary
      ]
  toJSON (PlayerJoined gid name color) =
    object
      [ "event" .= ("playerJoined" :: Text),
        "gameId" .= gid,
        "player" .= name,
        "color" .= color
      ]
  toJSON (GameStarted gid) =
    object
      [ "event" .= ("gameStarted" :: Text),
        "gameId" .= gid
      ]
  toJSON (GameEnded gid winner) =
    object
      [ "event" .= ("gameEnded" :: Text),
        "gameId" .= gid,
        "winner" .= winner
      ]

-- | Events broadcast to game subscribers
data GameEvent
  = MoveEvent GameMove UTCTime
  | ConcedeEvent Color -- winner color
  deriving (Show, Generic)

instance ToJSON GameEvent where
  toJSON (MoveEvent move timestamp) =
    object
      [ "event" .= ("move" :: Text),
        "move" .= move,
        "timestamp" .= timestamp
      ]
  toJSON (ConcedeEvent winner) =
    object
      [ "event" .= ("concede" :: Text),
        "winner" .= winner
      ]

-- | Central store for all SSE subscribers
data SubscriberStore = SubscriberStore
  { lobbySubscribers :: TVar [TQueue LobbyEvent],
    gameSubscribers :: TVar (Map GameId [TQueue GameEvent])
  }

-- | Create a new empty subscriber store
newSubscriberStore :: IO SubscriberStore
newSubscriberStore = do
  lobbySubs <- newTVarIO []
  gameSubs <- newTVarIO Map.empty
  return $ SubscriberStore lobbySubs gameSubs

-- | Subscribe to lobby events, returns a queue to read from
subscribeLobby :: SubscriberStore -> IO (TQueue LobbyEvent)
subscribeLobby store = do
  queue <- newTQueueIO
  atomically $ modifyTVar' (lobbySubscribers store) (queue :)
  return queue

-- | Unsubscribe from lobby events
unsubscribeLobby :: SubscriberStore -> TQueue LobbyEvent -> IO ()
unsubscribeLobby store queue =
  atomically $ modifyTVar' (lobbySubscribers store) (filter (/= queue))

-- | Broadcast an event to all lobby subscribers
broadcastLobby :: SubscriberStore -> LobbyEvent -> IO ()
broadcastLobby store event = do
  queues <- readTVarIO (lobbySubscribers store)
  forM_ queues $ \queue ->
    atomically $ writeTQueue queue event

-- | Subscribe to events for a specific game
subscribeGame :: SubscriberStore -> GameId -> IO (TQueue GameEvent)
subscribeGame store gameId = do
  queue <- newTQueueIO
  atomically $ modifyTVar' (gameSubscribers store) $ \m ->
    Map.insertWith (++) gameId [queue] m
  return queue

-- | Unsubscribe from game events
unsubscribeGame :: SubscriberStore -> GameId -> TQueue GameEvent -> IO ()
unsubscribeGame store gameId queue =
  atomically $ modifyTVar' (gameSubscribers store) $ \m ->
    Map.adjust (filter (/= queue)) gameId m

-- | Broadcast an event to all subscribers of a specific game
broadcastGame :: SubscriberStore -> GameId -> GameEvent -> IO ()
broadcastGame store gameId event = do
  subscribers <- readTVarIO (gameSubscribers store)
  case Map.lookup gameId subscribers of
    Nothing -> return ()
    Just queues -> forM_ queues $ \queue ->
      atomically $ writeTQueue queue event
