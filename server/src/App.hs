{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Api
  ( API,
    APIWithAssets,
    ConcedeError (..),
    GameId (..),
    GameSummary,
    GameWithNames,
    JoinError (..),
    JoinGameResponse (..),
    JoinRequest (..),
    MoveError (..),
    RawHtml (RawHtml),
    SessionToken (..),
    api,
    apiWithAssets,
  )
import Control.Concurrent.STM (atomically, readTQueue)
import Control.Exception (bracket, finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Data.Aeson (encode)
import Data.ByteString.Builder (byteString, lazyByteString)
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database
  ( CleanupConfig (..),
    DB,
    concedeGame,
    getAllGameSummaries,
    getGameWithNames,
    initDB,
    insertGameWithNewId,
    joinGameWithToken,
    logDBState,
    updateGame,
    validateTokenForMove,
  )
import Game (Color (..), Game (..), GameMove, PlayerSlot (..), getCurrentPlayerSlot, give5Cards)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseStream)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Options (cleanupOptionsToConfig)
import qualified Options
import Servant
  ( Application,
    Handler,
    HasServer (ServerT),
    Proxy (..),
    Raw,
    Server,
    Tagged (Tagged),
    err404,
    hoistServer,
    serve,
    throwError,
    unTagged,
    type (:<|>) (..),
  )
import Subscribers
  ( GameEvent (..),
    LobbyEvent (..),
    SubscriberStore,
    broadcastGame,
    broadcastLobby,
    newSubscriberStore,
    subscribeGame,
    subscribeLobby,
    unsubscribeGame,
    unsubscribeLobby,
  )
import System.Directory (doesFileExist)
import WaiAppStatic.Types (MaxAge (..), ssMaxAge)

-- | Application environment with DB and subscriber store
data AppEnv = AppEnv
  { appDB :: DB,
    appSubscribers :: SubscriberStore
  }

-- | WAI Application with configuration
appWithConfig :: Options.ServerOptions -> IO Application
appWithConfig opts =
  let cleanupCfg = cleanupOptionsToConfig (Options.optCleanup opts)
      saveIntervalMins = Options.optSaveInterval opts
      cleanupIntervalMins = Options.cleanupInterval (Options.optCleanup opts)
      cleanupEnabled = Options.cleanupEnabled (Options.optCleanup opts)
   in serve apiWithAssets <$> makeServer (Options.optDatabase opts) (Options.optResetDB opts) cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled

-- | Custom monad for handlers: gives access to AppEnv via ReaderT
type AppM = ReaderT AppEnv Handler

-- | Build the complete server: typed API routes + static file serving
makeServer :: FilePath -> Bool -> CleanupConfig -> Int -> Int -> Bool -> IO (Server APIWithAssets)
makeServer dbPath resetDB cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled = do
  db <- initDB dbPath resetDB cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled
  subscriberStore <- newSubscriberStore
  let env = AppEnv {appDB = db, appSubscribers = subscriberStore}
  putStrLn "Server initialized successfully"

  let staticSettings = (defaultFileServerSettings "assets/") {ssMaxAge = NoMaxAge}
      staticFileServer = staticApp staticSettings
      apiHandlers = hoistServer api (runAppM env) (handlers env)

  -- Combine: try API routes first, fall back to static files
  return (apiHandlers :<|> Tagged {unTagged = staticFileServer})

-- | Convert our AppM monad to Servant's Handler monad
runAppM :: AppEnv -> AppM a -> Handler a
runAppM env action = runReaderT action env

-- | All API route handlers
handlers :: AppEnv -> ServerT API AppM
handlers env =
  newGame
    :<|> getGameSummaries
    :<|> joinGame
    :<|> getGame
    :<|> newMove
    :<|> concede
    :<|> lobbyStreamHandler env
    :<|> gameStreamHandler env
    :<|> getNewGamePageHtml
    :<|> getIndexHtml

newGame :: AppM GameId
newGame = do
  env <- ask
  let db = appDB env
      store = appSubscribers env
  gameId <- liftIO $ insertGameWithNewId db
  liftIO $ do
    putStrLn $ "Creating new game with ID: " ++ show gameId
    logDBState "After creating game" db
    broadcastLobby store LobbyChanged
  return gameId

getGameSummaries :: AppM [GameSummary]
getGameSummaries = do
  db <- asks appDB
  liftIO $ getAllGameSummaries db

joinGame :: GameId -> Maybe SessionToken -> JoinRequest -> AppM (Either JoinError JoinGameResponse)
joinGame gameId maybeToken (JoinRequest name) = do
  env <- ask
  let db = appDB env
      store = appSubscribers env
      playerName = T.pack name
  liftIO $ putStrLn $ "Player '" ++ name ++ "' attempting to join game " ++ show gameId

  result <- liftIO $ joinGameWithToken db gameId playerName maybeToken
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Join failed: " ++ show err
      return $ Left err
    Right joinResponse -> do
      liftIO $ do
        putStrLn "Join successful"
        broadcastLobby store LobbyChanged
      return $ Right joinResponse

getGame :: GameId -> AppM GameWithNames
getGame gameId = do
  db <- asks appDB
  maybeGame <- liftIO $ getGameWithNames db gameId
  case maybeGame of
    Nothing -> throwError err404
    Just game -> return game

newMove :: GameId -> Maybe SessionToken -> GameMove -> AppM (Either MoveError GameMove)
newMove gameId maybeToken move = do
  case maybeToken of
    Nothing -> return $ Left MEInvalidToken
    Just token -> do
      env <- ask
      let db = appDB env
          store = appSubscribers env
      -- Validate token and check if it's the player's turn
      isValid <- liftIO $ validateTokenForMove db gameId token

      if not isValid
        then do
          liftIO $ putStrLn "Move rejected: invalid token or not your turn"
          return $ Left MENotYourTurn
        else do
          -- Token is valid, process the move
          now <- liftIO getCurrentTime
          let updateGameFn (Game p1 p2 cards history w created _) =
                Just $
                  Game
                    { player_white = p1,
                      player_black = p2,
                      cards = cards,
                      history = (move, now) : history,
                      winner = w,
                      createdAt = created,
                      lastActivity = now
                    }
          success <- liftIO $ updateGame db gameId updateGameFn
          if success
            then do
              liftIO $ do
                putStrLn "Move accepted"
                broadcastGame store gameId (MoveEvent move now)
                broadcastLobby store LobbyChanged
              return $ Right move
            else return $ Left MEGameNotFound

concede :: GameId -> Maybe SessionToken -> AppM (Either ConcedeError Color)
concede gameId maybeToken = do
  case maybeToken of
    Nothing -> return $ Left CEInvalidToken
    Just token -> do
      env <- ask
      let db = appDB env
          store = appSubscribers env
      result <- liftIO $ concedeGame db gameId token
      case result of
        Nothing -> return $ Left CEGameNotFound
        Just winnerColor -> do
          liftIO $ do
            broadcastGame store gameId (ConcedeEvent winnerColor)
            broadcastLobby store LobbyChanged
          return $ Right winnerColor

-- | SSE handler for lobby stream
lobbyStreamHandler :: AppEnv -> Tagged AppM Application
lobbyStreamHandler env = Tagged $ \req respond -> do
  let store = appSubscribers env
  -- Subscribe to lobby events
  queue <- subscribeLobby store
  -- Send SSE response
  respond $
    responseStream status200 [("Content-Type", "text/event-stream"), ("Cache-Control", "no-cache"), ("Connection", "keep-alive")] $ \write flush -> do
      -- Send initial comment to establish connection
      write (byteString ": connected\n\n")
      flush
      -- Loop forever sending events
      let loop = do
            event <- atomically $ readTQueue queue
            let eventData = "data: " <> lazyByteString (encode event) <> "\n\n"
            write eventData
            flush
            loop
      loop `finally` unsubscribeLobby store queue

-- | SSE handler for game stream
gameStreamHandler :: AppEnv -> GameId -> Tagged AppM Application
gameStreamHandler env gameId = Tagged $ \req respond -> do
  let store = appSubscribers env
  -- Subscribe to game events
  queue <- subscribeGame store gameId
  -- Send SSE response
  respond $
    responseStream status200 [("Content-Type", "text/event-stream"), ("Cache-Control", "no-cache"), ("Connection", "keep-alive")] $ \write flush -> do
      -- Send initial comment to establish connection
      write (byteString ": connected\n\n")
      flush
      -- Loop forever sending events
      let loop = do
            event <- atomically $ readTQueue queue
            let eventData = "data: " <> lazyByteString (encode event) <> "\n\n"
            write eventData
            flush
            loop
      loop `finally` unsubscribeGame store gameId queue

getNewGamePageHtml :: AppM RawHtml
getNewGamePageHtml = do
  let path = "assets/index.html"
  exists <- liftIO $ doesFileExist path
  if exists
    then RawHtml <$> liftIO (Lazy.readFile path)
    else throwError err404

getIndexHtml :: GameId -> AppM RawHtml
getIndexHtml _ = do
  let path = "assets/index.html"
  exists <- liftIO $ doesFileExist path
  if exists
    then RawHtml <$> liftIO (Lazy.readFile path)
    else throwError err404
