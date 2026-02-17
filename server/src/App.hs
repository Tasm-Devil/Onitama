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
    GameWithNames (..),
    JoinError (..),
    JoinGameResponse (..),
    JoinRequest (..),
    MoveError (..),
    NewGameRequest (..),
    NewGameResponse (..),
    RawHtml (RawHtml),
    SessionToken (..),
    api,
    apiWithAssets,
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (atomically, readTQueue)
import Control.Exception (finally)
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
import Data.Time.Clock (getCurrentTime)
import Database
  ( CleanupConfig (..),
    DB,
    Player (..),
    concedeGame,
    createPlayer,
    getAllGameSummaries,
    getGameById,
    getGameWithNames,
    getPlayerByName,
    getPlayerByToken,
    initDB,
    insertGameWithNewId,
    joinGameWithToken,
    logDBState,
    updateGame,
  )
import Game (Color (..), Game (..), GameMove, addMoveToGame)
import qualified Minimax
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseStream)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Onitama (formatMove, give5Cards, replayGame, validateMove)
import qualified Onitama
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

-- | Convert Maybe to ExceptT, throwing the given error on Nothing.
noteE :: (Monad m) => e -> Maybe a -> ExceptT e m a
noteE err = maybe (throwE err) return

-- | Convert Either to ExceptT.
hoistEither :: (Monad m) => Either e a -> ExceptT e m a
hoistEither = either throwE return

-- | Build the complete server: typed API routes + static file serving
makeServer :: Maybe FilePath -> Bool -> CleanupConfig -> Int -> Int -> Bool -> IO (Server APIWithAssets)
makeServer dbPath resetDB cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled = do
  subscriberStore <- newSubscriberStore
  let onCleanup = broadcastLobby subscriberStore LobbyChanged
  db <- initDB dbPath resetDB cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled onCleanup
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
    :<|> getNewGameAIPageHtml
    :<|> getIndexHtml

newGame :: Maybe SessionToken -> NewGameRequest -> AppM (Either JoinError NewGameResponse)
newGame maybeToken (NewGameRequest name vsAI) = do
  let trimmedName = T.strip (T.pack name)
  if T.null trimmedName
    then return $ Left JEInvalidName
    else
      if vsAI
        then newAIGame maybeToken trimmedName
        else newMultiplayerGame maybeToken trimmedName

newMultiplayerGame :: Maybe SessionToken -> T.Text -> AppM (Either JoinError NewGameResponse)
newMultiplayerGame maybeToken playerName = do
  env <- ask
  let db = appDB env
      store = appSubscribers env
  newCards <- liftIO give5Cards
  now <- liftIO getCurrentTime
  let game =
        Game
          { player_white = Nothing,
            player_black = Nothing,
            cards = newCards,
            history = [],
            winner = Nothing,
            createdAt = now,
            lastActivity = now,
            aiDifficulty = Nothing
          }
  gameId <- liftIO $ insertGameWithNewId db game
  liftIO $ putStrLn $ "Creating new game with ID: " ++ show gameId
  result <- liftIO $ joinGameWithToken db gameId playerName maybeToken
  case result of
    Left err -> return $ Left err
    Right joinResponse -> do
      liftIO $ do
        logDBState "After creating game" db
        broadcastLobby store LobbyChanged
        broadcastGame store gameId (PlayerJoinedEvent playerName White)
      return $ Right (NewGameResponse { newGameId = gameId, newGameJoinResponse = joinResponse })

newAIGame :: Maybe SessionToken -> T.Text -> AppM (Either JoinError NewGameResponse)
newAIGame maybeToken playerName = runExceptT $ do
  env <- lift ask
  let db = appDB env
      store = appSubscribers env
      aiName = T.pack "Computer"
      depth = 5

  -- Get or create AI player
  maybeAI <- liftIO $ getPlayerByName db aiName
  aiPlayer <- case maybeAI of
    Just p -> return p
    Nothing -> liftIO $ createPlayer db aiName

  -- Create game with AI as Black
  newCards <- liftIO give5Cards
  now <- liftIO getCurrentTime
  let game =
        Game
          { player_white = Nothing,
            player_black = Just (playerId aiPlayer),
            cards = newCards,
            history = [],
            winner = Nothing,
            createdAt = now,
            lastActivity = now,
            aiDifficulty = Just depth
          }
  gameId <- liftIO $ insertGameWithNewId db game
  liftIO $ putStrLn $ "Creating AI game with ID: " ++ show gameId

  -- If Black (AI) starts, apply AI first move
  currentGame <- noteE JEGameNotFound =<< liftIO (getGameById db gameId)
  let gs = Onitama.initGameState (cards currentGame)
  gameAfterAI <- case Onitama.gsNextColor gs of
    Black -> applyAIOpening db gameId depth gs currentGame
    White -> return currentGame

  -- Join human as White
  joinResponse <- hoistEither =<< liftIO (joinGameWithToken db gameId playerName maybeToken)
  liftIO $ do
    broadcastLobby store LobbyChanged
    broadcastGame store gameId (PlayerJoinedEvent playerName White)
  unless (null $ history gameAfterAI) $ do
    let (aiMove, aiTime) = head (history gameAfterAI)
    liftIO $ broadcastGame store gameId (MoveEvent aiMove aiTime (winner gameAfterAI))
  return $ NewGameResponse { newGameId = gameId, newGameJoinResponse = joinResponse }

-- | Apply AI opening move if possible, otherwise return game unchanged.
applyAIOpening :: DB -> GameId -> Int -> Onitama.GameState -> Game -> ExceptT JoinError AppM Game
applyAIOpening db gameId depth gs currentGame =
  case Minimax.bestMove depth gs of
    Nothing -> return currentGame
    Just pm -> do
      let moveStr = formatMove pm
      case validateMove (cards currentGame) [moveStr] of
        Left _ -> return currentGame
        Right maybeWinner -> do
          aiNow <- liftIO getCurrentTime
          _ <- liftIO $ updateGame db gameId (addMoveToGame moveStr aiNow maybeWinner)
          liftIO $ putStrLn $ "AI made opening move: " ++ moveStr
          noteE JEGameNotFound =<< liftIO (getGameById db gameId)

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

  -- Get game state before join to detect new joins vs rejoins
  maybeGameBefore <- liftIO $ getGameById db gameId

  result <- liftIO $ joinGameWithToken db gameId playerName maybeToken
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Join failed: " ++ show err
      return $ Left err
    Right joinResponse -> do
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
              broadcastGame store gameId (PlayerJoinedEvent joinedName joinedColor)
        Nothing -> return ()
      liftIO $ broadcastLobby store LobbyChanged
      return $ Right joinResponse

getGame :: GameId -> AppM GameWithNames
getGame gameId = do
  db <- asks appDB
  maybeGame <- liftIO $ getGameWithNames db gameId
  case maybeGame of
    Nothing -> throwError err404
    Just game -> return game

newMove :: GameId -> Maybe SessionToken -> GameMove -> AppM (Either MoveError GameMove)
newMove gameId maybeToken move = runExceptT $ do
  token <- noteE MEInvalidToken maybeToken
  env <- lift ask
  let db = appDB env
      store = appSubscribers env

  -- Get game and validate player
  game <- noteE MEGameNotFound =<< liftIO (getGameById db gameId)
  maybePlayer <- liftIO $ getPlayerByToken db token
  let isInGame = case maybePlayer of
        Just p ->
          let pid = playerId p
           in player_white game == Just pid || player_black game == Just pid
        Nothing -> False
  unless isInGame $ do
    liftIO $ putStrLn "Move rejected: player not in game"
    throwE MENotYourTurn

  -- Validate move
  let historyMoves = move : map fst (history game)
  maybeWinner <- case validateMove (cards game) historyMoves of
    Left _ -> do
      liftIO $ putStrLn "Move rejected: invalid move"
      throwE MEInvalidMove
    Right mw -> return mw

  -- Apply move
  now <- liftIO getCurrentTime
  success <- liftIO $ updateGame db gameId (addMoveToGame move now maybeWinner)
  unless success $ throwE MEGameNotFound
  liftIO $ do
    putStrLn "Move accepted"
    broadcastGame store gameId (MoveEvent move now maybeWinner)
    broadcastLobby store LobbyChanged

  -- Trigger AI response if applicable
  when (isNothing maybeWinner) $ lift $ triggerAIMove gameId
  return move

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

triggerAIMove :: GameId -> AppM ()
triggerAIMove gameId = void $ runMaybeT $ do
  env <- lift ask
  let db = appDB env
      store = appSubscribers env

  game <- MaybeT $ liftIO $ getGameById db gameId
  depth <- MaybeT $ return $ aiDifficulty game
  gs <- MaybeT $ return $ replayGame (cards game) (map fst $ history game)
  guard $ isNothing (Onitama.gsWinner gs)
  guard $ Onitama.gsNextColor gs == Black
  pm <- MaybeT $ return $ Minimax.bestMove depth gs

  let moveStr = formatMove pm
      historyMoves = moveStr : map fst (history game)
  maybeWinner <- MaybeT $ return $ either (const Nothing) Just $ validateMove (cards game) historyMoves

  now <- liftIO getCurrentTime
  success <- liftIO $ updateGame db gameId (addMoveToGame moveStr now maybeWinner)
  guard success
  liftIO $ do
    putStrLn $ "AI move: " ++ moveStr
    broadcastGame store gameId (MoveEvent moveStr now maybeWinner)
    broadcastLobby store LobbyChanged

-- | SSE handler for lobby stream
lobbyStreamHandler :: AppEnv -> Tagged AppM Application
lobbyStreamHandler env = Tagged $ \req respond -> do
  let store = appSubscribers env
  -- Subscribe to lobby events
  queue <- subscribeLobby store
  -- Send SSE response
  respond $
    responseStream status200 [("Content-Type", "text/event-stream"), ("Cache-Control", "no-cache"), ("Connection", "keep-alive")] $ \write flush -> do
      -- Send initial lobbyChanged so client fetches current state
      write ("data: " <> lazyByteString (encode LobbyChanged) <> "\n\n")
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
      loop `finally` unsubscribeGame store gameId queue

getNewGamePageHtml :: AppM RawHtml
getNewGamePageHtml = do
  let path = "assets/index.html"
  exists <- liftIO $ doesFileExist path
  if exists
    then RawHtml <$> liftIO (Lazy.readFile path)
    else throwError err404

getNewGameAIPageHtml :: AppM RawHtml
getNewGameAIPageHtml = getNewGamePageHtml

getIndexHtml :: GameId -> AppM RawHtml
getIndexHtml _ = do
  let path = "assets/index.html"
  exists <- liftIO $ doesFileExist path
  if exists
    then RawHtml <$> liftIO (Lazy.readFile path)
    else throwError err404
