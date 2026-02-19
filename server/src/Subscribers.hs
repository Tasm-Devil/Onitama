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

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    modifyTVar',
    newTQueueIO,
    newTVarIO,
    readTVarIO,
    writeTQueue,
  )
import Control.Monad (forM_)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Types (Color, GameId, MoveNotation)

-- | Notification broadcast to lobby subscribers (client refetches on receive)
data LobbyEvent = LobbyChanged
  deriving (Show, Generic)

instance ToJSON LobbyEvent where
  toJSON LobbyChanged =
    object ["event" .= ("lobbyChanged" :: Text)]

-- | Events broadcast to game subscribers
data GameEvent
  = MoveEvent MoveNotation UTCTime (Maybe Color)
  | ConcedeEvent Color -- winner color
  | PlayerJoinedEvent Text Color -- player name, assigned color
  deriving (Show, Generic)

instance ToJSON GameEvent where
  toJSON (MoveEvent move timestamp maybeWinner) =
    object $
      [ "event" .= ("move" :: Text),
        "move" .= move,
        "timestamp" .= timestamp
      ]
        ++ maybe [] (\w -> ["winner" .= w]) maybeWinner
  toJSON (ConcedeEvent winner) =
    object
      [ "event" .= ("concede" :: Text),
        "winner" .= winner
      ]
  toJSON (PlayerJoinedEvent name color) =
    object
      [ "event" .= ("playerJoined" :: Text),
        "name" .= name,
        "color" .= color
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
  return $ SubscriberStore {lobbySubscribers = lobbySubs, gameSubscribers = gameSubs}

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
