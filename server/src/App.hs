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
import Servant (Application, Handler, HasServer (ServerT), Server, Tagged (Tagged), err401, err404, hoistServer, serve, throwError, unTagged, type (:<|>) (..))
import Subscribers (GameEvent (..), LobbyEvent (..))
import qualified Subscribers
import System.Directory (doesFileExist)
import Types
    ( NewGameResponse(..),
      NewGameRequest(NewGameRequest),
      CardSet,
      GameSummary,
      ConcedeError(CEGameNotFound, CENotAuthenticated),
      MoveError(..),
      JoinError(JENotAuthenticated, JEGameNotFound),
      JoinGameResponse(..),
      GameWithNames(gameWhiteName),
      Game(..),
      AuthUser(..),
      OidcUserId(..),
      GameId,
      MoveNotation,
      Color(..),
      addMoveToGame,
      gameToGameWithNames )
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

-- | Extract authenticated user from OIDC headers, or throw error.
requireAuth :: Maybe OidcUserId -> Maybe T.Text -> Either e AuthUser -> Either e AuthUser
requireAuth maybeUserId maybeName _ =
  case (maybeUserId, maybeName) of
    (Just uid, Just name) -> Right (AuthUser { authUserId = uid, authUserName = name })
    (Just uid, Nothing) -> Right (AuthUser { authUserId = uid, authUserName = let OidcUserId t = uid in t })
    _ -> Left undefined -- will be handled per-endpoint

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
    :<|> whoAmI
    :<|> getIndexHtml

-- | Extract AuthUser from headers or return an error
extractAuth :: Maybe OidcUserId -> Maybe T.Text -> Maybe AuthUser
extractAuth (Just uid) (Just name) = Just (AuthUser { authUserId = uid, authUserName = name })
extractAuth (Just uid) Nothing = Just (AuthUser { authUserId = uid, authUserName = let OidcUserId t = uid in t })
extractAuth _ _ = Nothing

newGame :: Maybe OidcUserId -> Maybe T.Text -> NewGameRequest -> AppM (Either JoinError NewGameResponse)
newGame maybeUserId maybeName (NewGameRequest vsAI cardSet) =
  case extractAuth maybeUserId maybeName of
    Nothing -> return $ Left JENotAuthenticated
    Just user ->
      if vsAI
        then newAIGame user cardSet
        else newMultiplayerGame user cardSet

newMultiplayerGame :: AuthUser -> CardSet -> AppM (Either JoinError NewGameResponse)
newMultiplayerGame user cardSet = runExceptT $ do
  env <- lift ask
  let db = appDB env
      store = appSubscribers env
      userId = authUserId user
      displayName = authUserName user

  -- Create game with the creator already assigned
  newCards <- liftIO $ Onitama.give5Cards cardSet
  now <- liftIO getCurrentTime
  let game =
        Game
          { player_white = Just userId,
            player_black = Nothing,
            player_white_name = displayName,
            player_black_name = T.empty,
            cards = newCards,
            history = [],
            winner = Nothing,
            createdAt = now,
            lastActivity = now,
            aiDifficulty = Nothing
          }
  gameId <- liftIO $ Database.insertGameWithNewId db game
  liftIO $ putStrLn $ "Creating new game with ID: " ++ show gameId

  let gameWithNames = gameToGameWithNames game
      joinResponse = JoinGameResponse
        { responseGame = gameWithNames,
          responsePlayerName = displayName
        }
  liftIO $ do
    Database.logDBState "After creating game" db
    Subscribers.broadcastLobby store LobbyChanged
    Subscribers.broadcastGame store gameId (PlayerJoinedEvent displayName White)
  return $ NewGameResponse {newGameId = gameId, newGameJoinResponse = joinResponse}

newAIGame :: AuthUser -> CardSet -> AppM (Either JoinError NewGameResponse)
newAIGame user cardSet = runExceptT $ do
  env <- lift ask
  let db = appDB env
      store = appSubscribers env
      userId = authUserId user
      displayName = authUserName user
      aiUserId = OidcUserId "__ai__"
      aiName = T.pack "Computer"
      depth = 5

  -- Create game with AI as Black
  newCards <- liftIO $ Onitama.give5Cards cardSet
  now <- liftIO getCurrentTime
  let game =
        Game
          { player_white = Nothing,
            player_black = Just aiUserId,
            player_white_name = T.empty,
            player_black_name = aiName,
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
  joinResponse <- hoistEither =<< liftIO (Database.joinGame db gameId userId displayName)
  liftIO $ do
    Subscribers.broadcastLobby store LobbyChanged
    Subscribers.broadcastGame store gameId (PlayerJoinedEvent displayName White)
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

joinGame :: GameId -> Maybe OidcUserId -> Maybe T.Text -> AppM (Either JoinError JoinGameResponse)
joinGame gameId maybeUserId maybeName = runExceptT $ do
  case extractAuth maybeUserId maybeName of
    Nothing -> throwE JENotAuthenticated
    Just user -> do
      env <- lift ask
      let db = appDB env
          store = appSubscribers env
          userId = authUserId user
          displayName = authUserName user
      liftIO $ putStrLn $ "Player '" ++ T.unpack displayName ++ "' attempting to join game " ++ show gameId

      -- Get game state before join to detect new joins vs rejoins
      maybeGameBefore <- liftIO $ Database.getGameById db gameId

      joinResponse <- hoistEither =<< liftIO (Database.joinGame db gameId userId displayName)

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

newMove :: GameId -> Maybe OidcUserId -> Maybe T.Text -> Types.MoveNotation -> AppM (Either MoveError Types.MoveNotation)
newMove gameId maybeUserId maybeName move = runExceptT $ do
  user <- case extractAuth maybeUserId maybeName of
    Nothing -> throwE MENotAuthenticated
    Just u -> return u
  env <- lift ask
  let db = appDB env
      store = appSubscribers env
      userId = authUserId user

  -- Atomically validate and apply the move (prevents TOCTOU race conditions)
  now <- liftIO getCurrentTime
  result <- liftIO $ Database.atomicUpdateGame db gameId $ \game ->
        let isInGame = player_white game == Just userId || player_black game == Just userId
        in if not isInGame
           then Left MENotYourTurn
           else let historyMoves = move : map fst (history game)
                in case Onitama.replayGame (cards game) historyMoves of
                     Nothing -> Left MEInvalidMove
                     Just finalState ->
                       let maybeWinner = Onitama.gsWinner finalState
                       in Right (Types.addMoveToGame move now maybeWinner game, maybeWinner)

  maybeWinner <- case result of
    Nothing -> throwE MEGameNotFound
    Just (Left err) -> do
      liftIO $ putStrLn $ case err of
        MENotYourTurn -> "Move rejected: player not in game"
        MEInvalidMove -> "Move rejected: invalid move"
        _ -> "Move rejected"
      throwE err
    Just (Right w) -> do
      liftIO $ putStrLn "Move accepted"
      return w

  liftIO $ do
    Subscribers.broadcastGame store gameId (MoveEvent move now maybeWinner)
    Subscribers.broadcastLobby store LobbyChanged

  -- Trigger AI response if applicable
  when (isNothing maybeWinner) $ lift $ triggerAIMove gameId
  return move

concede :: GameId -> Maybe OidcUserId -> Maybe T.Text -> AppM (Either ConcedeError Color)
concede gameId maybeUserId maybeName = runExceptT $ do
  user <- case extractAuth maybeUserId maybeName of
    Nothing -> throwE CENotAuthenticated
    Just u -> return u
  env <- lift ask
  let db = appDB env
      store = appSubscribers env
      userId = authUserId user
  winnerColor <- noteE CEGameNotFound =<< liftIO (Database.concedeGame db gameId userId)
  liftIO $ do
    Subscribers.broadcastGame store gameId (ConcedeEvent winnerColor)
    Subscribers.broadcastLobby store LobbyChanged
  return winnerColor

whoAmI :: Maybe OidcUserId -> Maybe T.Text -> AppM AuthUser
whoAmI maybeUserId maybeName =
  case extractAuth maybeUserId maybeName of
    Nothing -> throwError err401
    Just user -> return user

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
