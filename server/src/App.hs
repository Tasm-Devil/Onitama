{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Data.Maybe (fromJust, isNothing)
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
import Game (Color, Game (..), GameMove, PlayerSlot (..), getCurrentPlayerSlot, give5Cards)
import Network.Wai (Application)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import WaiAppStatic.Types (MaxAge (..), ssMaxAge)
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
import System.Directory (doesFileExist)

-- | WAI Application with configuration
appWithConfig :: Options.ServerOptions -> IO Application
appWithConfig opts =
  let cleanupCfg = cleanupOptionsToConfig (Options.optCleanup opts)
      saveIntervalMins = Options.optSaveInterval opts
      cleanupIntervalMins = Options.cleanupInterval (Options.optCleanup opts)
      cleanupEnabled = Options.cleanupEnabled (Options.optCleanup opts)
   in serve apiWithAssets <$> makeServer (Options.optDatabase opts) (Options.optResetDB opts) cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled

-- | Custom monad for handlers: gives access to DB via ReaderT
type AppM = ReaderT DB Handler

-- | Build the complete server: typed API routes + static file serving
makeServer :: FilePath -> Bool -> CleanupConfig -> Int -> Int -> Bool -> IO (Server APIWithAssets)
makeServer dbPath resetDB cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled = do
  db <- initDB dbPath resetDB cleanupCfg saveIntervalMins cleanupIntervalMins cleanupEnabled
  putStrLn "Server initialized successfully"

  let staticSettings = (defaultFileServerSettings "assets/") {ssMaxAge = NoMaxAge}
      staticFileServer = staticApp staticSettings
      apiHandlers = hoistServer api (runAppM db) handlers

  -- Combine: try API routes first, fall back to static files
  return (apiHandlers :<|> Tagged {unTagged = staticFileServer})

-- | Convert our AppM monad to Servant's Handler monad
runAppM :: DB -> AppM a -> Handler a
runAppM db action = runReaderT action db

-- | All API route handlers
handlers :: ServerT API AppM
handlers = newGame :<|> getGameSummaries :<|> joinGame :<|> getGame :<|> newMove :<|> concede :<|> getIndexHtml

newGame :: AppM GameId
newGame = do
  db <- ask
  gameId <- liftIO $ insertGameWithNewId db
  liftIO $ do
    putStrLn $ "Creating new game with ID: " ++ show gameId
    logDBState "After creating game" db
  return gameId

getGameSummaries :: AppM [GameSummary]
getGameSummaries = do
  db <- ask
  liftIO $ getAllGameSummaries db

joinGame :: GameId -> Maybe SessionToken -> JoinRequest -> AppM (Either JoinError JoinGameResponse)
joinGame gameId maybeToken (JoinRequest name) = do
  db <- ask
  let playerName = T.pack name
  liftIO $ putStrLn $ "Player '" ++ name ++ "' attempting to join game " ++ show gameId

  result <- liftIO $ joinGameWithToken db gameId playerName maybeToken
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Join failed: " ++ show err
      return $ Left err
    Right joinResponse -> do
      liftIO $ putStrLn "Join successful"
      return $ Right joinResponse

getGame :: GameId -> AppM GameWithNames
getGame gameId = do
  db <- ask
  maybeGame <- liftIO $ getGameWithNames db gameId
  case maybeGame of
    Nothing -> throwError err404
    Just game -> return game

newMove :: GameId -> Maybe SessionToken -> GameMove -> AppM (Either MoveError GameMove)
newMove gameId maybeToken move = do
  case maybeToken of
    Nothing -> return $ Left MEInvalidToken
    Just token -> do
      db <- ask
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
              liftIO $ putStrLn "Move accepted"
              return $ Right move
            else return $ Left MEGameNotFound

concede :: GameId -> Maybe SessionToken -> AppM (Either ConcedeError Color)
concede gameId maybeToken = do
  case maybeToken of
    Nothing -> return $ Left CEInvalidToken
    Just token -> do
      db <- ask
      result <- liftIO $ concedeGame db gameId token
      case result of
        Nothing -> return $ Left CEGameNotFound
        Just color -> return $ Right color

getIndexHtml :: GameId -> AppM RawHtml
getIndexHtml _ = do
  let path = "assets/index.html"
  exists <- liftIO $ doesFileExist path
  if exists
    then RawHtml <$> liftIO (Lazy.readFile path)
    else throwError err404
