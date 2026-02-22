{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Api (API, APIWithAssets, RawHtml (RawHtml), api, apiWithAssets)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (atomically, readTQueue)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate, finally)
import Control.Monad (guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Data.Aeson (encode)
import Data.ByteString.Builder (byteString, lazyByteString)
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Database
import qualified Minimax
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseStream)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import qualified Onitama
import qualified Options
import Servant (Application, Handler, HasServer (ServerT), Server, Tagged (Tagged), err404, hoistServer, serve, throwError, unTagged, type (:<|>) (..))
import Subscribers (GameEvent (..), LobbyEvent (..))
import qualified Subscribers
import System.Directory (doesFileExist)
import Types
import WaiAppStatic.Types (MaxAge (..), ssMaxAge)

-- | Application environment with DB and subscriber store
data AppEnv = AppEnv
  { appDB :: Database.DB,
    appSubscribers :: Subscribers.SubscriberStore
  }

-- | WAI Application with configuration
appWithConfig :: Options.ServerOptions -> IO Application
appWithConfig opts =
  let cleanupCfg = Options.cleanupOptionsToConfig (Options.optCleanup opts)
      saveIntervalMins = Options.optSaveInterval opts
      cleanupIntervalMins = Options.cleanupInterval (Options.optCleanup opts)
      cleanupEnabled = Options.cleanupEnabled (Options.optCleanup opts)
   in serve apiWithAssets <$> makeServer (Options.optDatabase opts) (Options.optResetDB opts) cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled

-- | Custom monad for handlers: gives access to AppEnv via ReaderT
type AppM = ReaderT AppEnv Handler

-- | Convert Maybe to ExceptT, throwing the given error on Nothing.
noteE :: (Monad m) => e -> Maybe a -> ExceptT e m a
noteE err = maybe (throwE err) return

-- | Convert Either to ExceptT.
hoistEither :: (Monad m) => Either e a -> ExceptT e m a
hoistEither = either throwE return

-- | Time a pure computation, forcing full evaluation via NFData.
timed :: (NFData a) => String -> a -> IO a
timed label val = do
  start <- getCurrentTime
  result <- evaluate (force val)
  end <- getCurrentTime
  let ms = realToFrac (diffUTCTime end start) * (1000 :: Double)
  putStrLn $ label ++ " took " ++ show (round ms :: Int) ++ "ms"
  return result

-- | Build the complete server: typed API routes + static file serving
makeServer :: Maybe FilePath -> Bool -> Database.CleanupConfig -> Int -> Int -> Bool -> IO (Server APIWithAssets)
makeServer dbPath resetDB cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled = do
  subscriberStore <- Subscribers.newSubscriberStore
  let onCleanup = Subscribers.broadcastLobby subscriberStore LobbyChanged
  db <- Database.initDB dbPath resetDB cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled onCleanup
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
    :<|> getIndexHtml

newGame :: Maybe SessionToken -> NewGameRequest -> AppM (Either JoinError NewGameResponse)
newGame maybeToken (NewGameRequest name vsAI cardSet) = do
  let trimmedName = T.strip (T.pack name)
  if T.null trimmedName
    then return $ Left JEInvalidName
    else
      if vsAI
        then newAIGame maybeToken trimmedName cardSet
        else newMultiplayerGame maybeToken trimmedName cardSet

newMultiplayerGame :: Maybe SessionToken -> T.Text -> CardSet -> AppM (Either JoinError NewGameResponse)
newMultiplayerGame maybeToken playerName cardSet = runExceptT $ do
  env <- lift ask
  let db = appDB env
      store = appSubscribers env

  -- Resolve player identity first (can fail with JENameTaken/JEInvalidName)
  player <- liftIO $ Database.resolvePlayerForGame db playerName maybeToken
  hoistEither player >>= \p -> do
    let pid = Database.playerId p

    -- Create game with the creator already assigned
    newCards <- liftIO $ Onitama.give5Cards cardSet
    now <- liftIO getCurrentTime
    let game =
          Game
            { player_white = Just pid,
              player_black = Nothing,
              cards = newCards,
              history = [],
              winner = Nothing,
              createdAt = now,
              lastActivity = now,
              aiDifficulty = Nothing
            }
    gameId <- liftIO $ Database.insertGameWithNewId db game
    liftIO $ putStrLn $ "Creating new game with ID: " ++ show gameId

    gameWithNames <- liftIO $ Database.gameToGameWithNames db game
    let joinResponse = JoinGameResponse
          { responseGame = gameWithNames,
            responseToken = Database.playerToken p,
            responsePlayerName = Database.playerName p
          }
    liftIO $ do
      Database.logDBState "After creating game" db
      Subscribers.broadcastLobby store LobbyChanged
      Subscribers.broadcastGame store gameId (PlayerJoinedEvent playerName White)
    return $ NewGameResponse {newGameId = gameId, newGameJoinResponse = joinResponse}

newAIGame :: Maybe SessionToken -> T.Text -> CardSet -> AppM (Either JoinError NewGameResponse)
newAIGame maybeToken playerName cardSet = runExceptT $ do
  env <- lift ask
  let db = appDB env
      store = appSubscribers env
      aiName = T.pack "Computer"
      depth = 5

  -- Get or create AI player
  maybeAI <- liftIO $ Database.getPlayerByName db aiName
  aiPlayer <- maybe (liftIO $ Database.createPlayer db aiName) return maybeAI

  -- Create game with AI as Black
  newCards <- liftIO $ Onitama.give5Cards cardSet
  now <- liftIO getCurrentTime
  let game =
        Game
          { player_white = Nothing,
            player_black = Just (Database.playerId aiPlayer),
            cards = newCards,
            history = [],
            winner = Nothing,
            createdAt = now,
            lastActivity = now,
            aiDifficulty = Just depth
          }
  gameId <- liftIO $ Database.insertGameWithNewId db game
  liftIO $ putStrLn $ "Creating AI game with ID: " ++ show gameId

  -- If Black (AI) starts, apply AI first move
  currentGame <- noteE JEGameNotFound =<< liftIO (Database.getGameById db gameId)
  let gs = Onitama.initGameState (cards currentGame)
  gameAfterAI <- case Onitama.gsNextColor gs of
    Black -> applyAIOpening db gameId depth gs currentGame
    White -> return currentGame

  -- Join human as White
  joinResponse <- hoistEither =<< liftIO (Database.joinGameWithToken db gameId playerName maybeToken)
  liftIO $ do
    Subscribers.broadcastLobby store LobbyChanged
    Subscribers.broadcastGame store gameId (PlayerJoinedEvent playerName White)
  unless (null $ history gameAfterAI) $ do
    let (aiMove, aiTime) = head (history gameAfterAI)
    liftIO $ Subscribers.broadcastGame store gameId (MoveEvent aiMove aiTime (winner gameAfterAI))
  return $ NewGameResponse {newGameId = gameId, newGameJoinResponse = joinResponse}

-- | Apply AI opening move if possible, otherwise return game unchanged.
applyAIOpening :: Database.DB -> GameId -> Int -> Onitama.GameState -> Game -> ExceptT JoinError AppM Game
applyAIOpening db gameId depth gs currentGame = do
  maybePm <- liftIO $ timed "AI opening" (Minimax.bestMove depth gs)
  case maybePm of
    Nothing -> return currentGame
    Just pm -> do
      let moveStr = Onitama.formatMove pm
      case Onitama.replayGame (cards currentGame) [moveStr] of
        Nothing -> return currentGame
        Just finalState -> do
          aiNow <- liftIO getCurrentTime
          _ <- liftIO $ Database.updateGame db gameId (Types.addMoveToGame moveStr aiNow (Onitama.gsWinner finalState))
          liftIO $ putStrLn $ "AI made opening move: " ++ moveStr
          noteE JEGameNotFound =<< liftIO (Database.getGameById db gameId)

getGameSummaries :: AppM [GameSummary]
getGameSummaries = do
  db <- asks appDB
  liftIO $ Database.getAllGameSummaries db

joinGame :: GameId -> Maybe SessionToken -> JoinRequest -> AppM (Either JoinError JoinGameResponse)
joinGame gameId maybeToken (JoinRequest name) = runExceptT $ do
  env <- lift ask
  let db = appDB env
      store = appSubscribers env
      playerName = T.pack name
  liftIO $ putStrLn $ "Player '" ++ name ++ "' attempting to join game " ++ show gameId

  -- Get game state before join to detect new joins vs rejoins
  maybeGameBefore <- liftIO $ Database.getGameById db gameId

  joinResponse <- hoistEither =<< liftIO (Database.joinGameWithToken db gameId playerName maybeToken)

  -- Broadcast PlayerJoinedEvent only for new joins (not rejoins)
  let respGame = responseGame joinResponse
      joinedName = responsePlayerName joinResponse
      joinedColor = if joinedName == gameWhiteName respGame then White else Black
  case maybeGameBefore of
    Just gameBefore -> do
      let slotWasEmpty = case joinedColor of
            White -> isNothing (player_white gameBefore)
            Black -> isNothing (player_black gameBefore)
      when slotWasEmpty $
        liftIO $
          Subscribers.broadcastGame store gameId (PlayerJoinedEvent joinedName joinedColor)
    Nothing -> return ()
  liftIO $ Subscribers.broadcastLobby store LobbyChanged
  return joinResponse

getGame :: GameId -> AppM GameWithNames
getGame gameId = do
  db <- asks appDB
  maybeGame <- liftIO $ Database.getGameWithNames db gameId
  case maybeGame of
    Nothing -> throwError err404
    Just game -> return game

newMove :: GameId -> Maybe SessionToken -> Types.MoveNotation -> AppM (Either MoveError Types.MoveNotation)
newMove gameId maybeToken move = runExceptT $ do
  token <- noteE MEInvalidToken maybeToken
  env <- lift ask
  let db = appDB env
      store = appSubscribers env

  -- Get game and validate player
  game <- noteE MEGameNotFound =<< liftIO (Database.getGameById db gameId)
  maybePlayer <- liftIO $ Database.getPlayerByToken db token
  let isInGame = case maybePlayer of
        Just p ->
          let pid = Database.playerId p
           in player_white game == Just pid || player_black game == Just pid
        Nothing -> False
  unless isInGame $ do
    liftIO $ putStrLn "Move rejected: player not in game"
    throwE MENotYourTurn

  -- Validate move
  let historyMoves = move : map fst (history game)
  finalState <- case Onitama.replayGame (cards game) historyMoves of
    Nothing -> do
      liftIO $ putStrLn "Move rejected: invalid move"
      throwE MEInvalidMove
    Just gs -> return gs
  let maybeWinner = Onitama.gsWinner finalState

  -- Apply move
  now <- liftIO getCurrentTime
  success <- liftIO $ Database.updateGame db gameId (Types.addMoveToGame move now maybeWinner)
  unless success $ throwE MEGameNotFound
  liftIO $ do
    putStrLn "Move accepted"
    Subscribers.broadcastGame store gameId (MoveEvent move now maybeWinner)
    Subscribers.broadcastLobby store LobbyChanged

  -- Trigger AI response if applicable
  when (isNothing maybeWinner) $ lift $ triggerAIMove gameId
  return move

concede :: GameId -> Maybe SessionToken -> AppM (Either ConcedeError Color)
concede gameId maybeToken = runExceptT $ do
  token <- noteE CEInvalidToken maybeToken
  env <- lift ask
  let db = appDB env
      store = appSubscribers env
  winnerColor <- noteE CEGameNotFound =<< liftIO (Database.concedeGame db gameId token)
  liftIO $ do
    Subscribers.broadcastGame store gameId (ConcedeEvent winnerColor)
    Subscribers.broadcastLobby store LobbyChanged
  return winnerColor

triggerAIMove :: GameId -> AppM ()
triggerAIMove gameId = void $ runMaybeT $ do
  env <- lift ask
  let db = appDB env
      store = appSubscribers env

  game <- MaybeT $ liftIO $ Database.getGameById db gameId
  depth <- MaybeT $ return $ aiDifficulty game
  gs <- MaybeT $ return $ Onitama.replayGame (cards game) (map fst $ history game)
  guard $ isNothing (Onitama.gsWinner gs)
  guard $ Onitama.gsNextColor gs == Black
  maybePm <- liftIO $ timed "AI move" (Minimax.bestMove depth gs)
  pm <- MaybeT $ return maybePm

  let moveStr = Onitama.formatMove pm
      historyMoves = moveStr : map fst (history game)
  finalState <- MaybeT $ return $ Onitama.replayGame (cards game) historyMoves

  now <- liftIO getCurrentTime
  success <- liftIO $ Database.updateGame db gameId (Types.addMoveToGame moveStr now (Onitama.gsWinner finalState))
  guard success
  liftIO $ do
    putStrLn $ "AI move: " ++ moveStr
    Subscribers.broadcastGame store gameId (MoveEvent moveStr now (Onitama.gsWinner finalState))
    Subscribers.broadcastLobby store LobbyChanged

-- | SSE handler for lobby stream
lobbyStreamHandler :: AppEnv -> Tagged AppM Application
lobbyStreamHandler env = Tagged $ \req respond -> do
  let store = appSubscribers env
  -- Subscribe to lobby events
  queue <- Subscribers.subscribeLobby store
  -- Send SSE response
  respond $
    responseStream status200 [("Content-Type", "text/event-stream"), ("Cache-Control", "no-cache"), ("Connection", "keep-alive")] $ \write flush -> do
      -- Loop forever, sending tick every 15s (doubles as keepalive for proxies)
      let loop = do
            result <- race (threadDelay 15000000) (atomically $ readTQueue queue)
            case result of
              Left () -> do
                write (byteString "data: {\"event\":\"tick\"}\n\n")
                flush
                loop
              Right event -> do
                write ("data: " <> lazyByteString (encode event) <> "\n\n")
                flush
                loop
      loop `finally` Subscribers.unsubscribeLobby store queue

-- | SSE handler for game stream
gameStreamHandler :: AppEnv -> GameId -> Tagged AppM Application
gameStreamHandler env gameId = Tagged $ \req respond -> do
  let store = appSubscribers env
  -- Subscribe to game events
  queue <- Subscribers.subscribeGame store gameId
  -- Send SSE response
  respond $
    responseStream status200 [("Content-Type", "text/event-stream"), ("Cache-Control", "no-cache"), ("Connection", "keep-alive")] $ \write flush -> do
      -- Send initial comment to establish connection
      write (byteString ": connected\n\n")
      flush
      -- Loop forever, sending keepalive every 15s to prevent proxy timeouts
      let loop = do
            result <- race (threadDelay 15000000) (atomically $ readTQueue queue)
            case result of
              Left () -> do
                write (byteString ": keepalive\n\n")
                flush
                loop
              Right event -> do
                write ("data: " <> lazyByteString (encode event) <> "\n\n")
                flush
                loop
      loop `finally` Subscribers.unsubscribeGame store gameId queue

getIndexHtml :: GameId -> AppM RawHtml
getIndexHtml _ = do
  let path = "assets/index.html"
  exists <- liftIO $ doesFileExist path
  if exists
    then RawHtml <$> liftIO (Lazy.readFile path)
    else throwError err404
