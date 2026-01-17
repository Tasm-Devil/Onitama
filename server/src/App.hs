{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module App where

import Api (API, APIWithAssets, GameId (..), GameSummary, GameWithNames, JoinError (..), JoinGameResponse (..), RawHtml (RawHtml), SessionToken (..), api, apiWithAssets)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Database
  ( DB,
    concedeGame,
    forceSave,
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

-- | WAI Application entry point
app :: IO Application
app = serve apiWithAssets <$> makeServer

-- | Custom monad for handlers: gives access to DB via ReaderT
type AppM = ReaderT DB Handler

-- | Build the complete server: typed API routes + static file serving
makeServer :: IO (Server APIWithAssets)
makeServer = do
  putStrLn "Starting server..."
  db <- initDB
  putStrLn "Server initialized successfully"

  let staticFileServer = staticApp $ defaultFileServerSettings "assets/"
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
    -- Force an immediate save for testing
    forceSave db

  return gameId

getGameSummaries :: AppM [GameSummary]
getGameSummaries = do
  db <- ask
  liftIO $ getAllGameSummaries db

joinGame :: Maybe GameId -> Maybe String -> Maybe SessionToken -> AppM (Either JoinError JoinGameResponse)
joinGame maybeGameId maybeName maybeToken = do
  db <- ask
  case (maybeGameId, maybeName) of
    (Just gameId, Just name) -> do
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
    _ -> return $ Left JEInvalidName -- Missing gameId or name

getGame :: Maybe GameId -> AppM (Maybe GameWithNames)
getGame maybeGameId = do
  case maybeGameId of
    Nothing -> return Nothing
    Just gameId -> do
      db <- ask
      liftIO $ getGameWithNames db gameId

newMove :: Maybe GameId -> Maybe SessionToken -> GameMove -> AppM (Maybe GameMove)
newMove maybeGameId maybeToken move = do
  case (maybeGameId, maybeToken) of
    (Just gameId, Just token) -> do
      db <- ask
      -- Validate token and check if it's the player's turn
      isValid <- liftIO $ validateTokenForMove db gameId token

      if not isValid
        then do
          liftIO $ putStrLn "Move rejected: invalid token or not your turn"
          return Nothing
        else do
          -- Token is valid, process the move
          let updateGameFn (Game p1 p2 cards history w) = Just $ Game {player_white = p1, player_black = p2, cards = cards, history = move : history, winner = w}
          success <- liftIO $ updateGame db gameId updateGameFn
          if success
            then do
              liftIO $ putStrLn "Move accepted"
              return (Just move)
            else return Nothing
    _ -> return Nothing

concede :: Maybe GameId -> Maybe SessionToken -> AppM (Maybe Color)
concede maybeGameId maybeToken = do
  case (maybeGameId, maybeToken) of
    (Just gameId, Just token) -> do
      db <- ask
      liftIO $ concedeGame db gameId token
    _ -> return Nothing

getIndexHtml :: GameId -> AppM RawHtml
getIndexHtml _ = do
  let path = "assets/index.html"
  exists <- liftIO $ doesFileExist path
  if exists
    then RawHtml <$> liftIO (Lazy.readFile path)
    else throwError err404
