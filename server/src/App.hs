{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module App where

import Api (API, GameId (..), GameSummary, api, RawHtml (RawHtml), APIWithAssets, apiWithAssets)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Maybe (fromJust, isNothing)
import Data.UUID.V4 (nextRandom)
import Game (Game (Game), GameMove, give5Cards)
import Network.Wai (Application)
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
  )
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

-- Import our new Database module
import Database
  ( DB,
    initDB,
    markDBChanged,
    logDBState,
    getGameById,
    insertGame,
    updateGame,
    getAllGameSummaries,
    forceSave
  )

app :: IO Application
app = serve apiWithAssets <$> server

type AppM = ReaderT DB Handler

server :: IO (Server APIWithAssets)
server = do
  putStrLn "Starting server..."
  let assets = staticApp $ defaultFileServerSettings "assets/"
  db <- initDB
  putStrLn "Server initialized successfully"
  return (readerServer db :<|> Tagged assets)
  where
    readerToHandler :: DB -> AppM a -> Handler a
    readerToHandler db appM = runReaderT appM db
    readerServer :: DB -> ServerT API Handler
    readerServer db = hoistServer api (readerToHandler db) apiServer

apiServer :: ServerT API AppM
apiServer = newGame :<|> getGameSummaries :<|> joinGame :<|> getGame :<|> newMove :<|> getIndexHtml

newGame :: AppM GameId
newGame = do
  db <- ask
  newCards <- liftIO give5Cards
  newUuid <- liftIO nextRandom
  let gameId = GameId newUuid
  liftIO $ putStrLn $ "Creating new game with ID: " ++ show gameId
  
  -- Create a new game and insert it into the database
  liftIO $ insertGame db gameId (Game "" "" newCards [])
  
  liftIO $ do
    putStrLn "Game created, logging state"
    logDBState "After creating game" db
    -- Force an immediate save for testing
    forceSave db
  
  return gameId
getGameSummaries :: AppM [GameSummary]
getGameSummaries = do
  db <- ask
  liftIO $ getAllGameSummaries db

joinGame :: GameId -> Maybe String -> AppM (Maybe Game)
joinGame gameId name = do
  db <- ask
  if isNothing name
    then return Nothing
    else do
      -- Get the current game
      maybeGame <- liftIO $ getGameById db gameId
      case maybeGame of
        Nothing -> return Nothing
        Just game -> do
          -- Update the game with the player's name
          let playerName = fromJust name
          let updateGameFn :: Game -> Maybe Game
              updateGameFn (Game "" "" cards history) = Just $ Game playerName "" cards history
              updateGameFn (Game "" p2 cards history) = Just $ Game playerName p2 cards history
              updateGameFn (Game p1 "" cards history) = Just $ Game p1 playerName cards history
              updateGameFn (Game p1 p2 cards history) = Just $ Game p1 p2 cards history
          
          success <- liftIO $ updateGame db gameId updateGameFn
          if success
            then liftIO $ getGameById db gameId
            else return Nothing

getGame :: GameId -> AppM (Maybe Game)
getGame gameId = do
  db <- ask
  liftIO $ getGameById db gameId

newMove :: GameId -> GameMove -> AppM (Maybe GameMove)
newMove gameId move = do
  db <- ask
  -- Check if the game exists
  maybeGame <- liftIO $ getGameById db gameId
  case maybeGame of
    Nothing -> return Nothing
    Just game -> do
      -- Update the game with the new move
      let updateGameFn (Game p1 p2 cards history) = Just $ Game p1 p2 cards (move : history)
      success <- liftIO $ updateGame db gameId updateGameFn
      if success
        then return (Just move)
        else return Nothing

getIndexHtml :: GameId -> AppM RawHtml
getIndexHtml gameId = do
  bs <- liftIO $ Lazy.readFile "assets/index.html"
  return $ RawHtml bs



