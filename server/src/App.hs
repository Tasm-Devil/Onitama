{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module App where

import Api (API, GameId (..), GameSummary, SessionToken (..), JoinGameResponse (..), api, RawHtml (RawHtml), APIWithAssets, apiWithAssets, GameWithNames)
import Game (Game (..), GameMove, give5Cards, Color, PlayerSlot(..), getCurrentPlayerSlot)
import qualified Data.Text as T
import Database
  ( DB,
    initDB,
    markDBChanged,
    logDBState,
    getGameById,
    insertGame,
    insertGameWithNewId,
    updateGame,
    getAllGameSummaries,
    forceSave,
    joinGameWithToken,
    validateTokenForMove,
    concedeGame,
    getGameWithNames
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Data.Maybe (fromJust, isNothing)
import Network.Wai (Application)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Servant
  ( Application,
    Handler,
    HasServer (ServerT),
    Proxy (..),
    Raw,
    Server,
    Tagged (Tagged),
    hoistServer,
    serve,
    type (:<|>) (..),
    throwError,
    err404, unTagged,
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
      apiHandlers      = hoistServer api (runAppM db) handlers

  -- Combine: try API routes first, fall back to static files
  return (apiHandlers :<|> Tagged { unTagged = staticFileServer })

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

joinGame :: Maybe GameId -> Maybe String -> Maybe SessionToken -> AppM (Maybe JoinGameResponse)
joinGame maybeGameId name maybeToken = do
  db <- ask
  if isNothing name || isNothing maybeGameId
    then return Nothing
    else do
      let gameId = fromJust maybeGameId
      let playerName = T.pack (fromJust name)

      liftIO $ putStrLn $ "Player " ++ T.unpack playerName ++ " attempting to join game " ++ show gameId

      -- Join game and get token (with optional existing token for rejoin)
      result <- liftIO $ joinGameWithToken db gameId playerName maybeToken
      case result of
        Nothing -> do
          liftIO $ putStrLn "Join failed: game not found, full, or invalid token for rejoin"
          return Nothing
        Just joingameresponse -> do
          liftIO $ putStrLn "Join successful, token generated/retrieved"
          return $ Just joingameresponse

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

      if not isValid then do
        liftIO $ putStrLn "Move rejected: invalid token or not your turn"
        return Nothing
      else do
        -- Token is valid, process the move
        let updateGameFn (Game p1 p2 cards history w) = Just $ Game { player_white = p1, player_black = p2, cards = cards, history = move : history, winner = w }
        success <- liftIO $ updateGame db gameId updateGameFn
        if success then do
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



