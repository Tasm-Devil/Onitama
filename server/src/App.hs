{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module App where

import Api (API, GameId (..), GameSummary, SessionToken (..), JoinGameResponse (..), api, RawHtml (RawHtml), APIWithAssets, apiWithAssets)
import Game (Game (Game), GameMove, give5Cards, Color, PlayerSlot(..), getCurrentPlayerSlot)
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
    validateToken,
    concedeGame
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
apiServer = newGame :<|> getGameSummaries :<|> joinGame :<|> getGame :<|> newMove :<|> concede :<|> getIndexHtml

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
      let playerName = fromJust name
      
      liftIO $ putStrLn $ "Player " ++ playerName ++ " attempting to join game " ++ show gameId
      
      -- Join game and get token (with optional existing token for rejoin)
      result <- liftIO $ joinGameWithToken db gameId playerName maybeToken
      case result of
        Nothing -> do
          liftIO $ putStrLn "Join failed: game not found, full, or invalid token for rejoin"
          return Nothing
        Just joingameresponse -> do
          liftIO $ putStrLn "Join successful, token generated/retrieved"
          return $ Just joingameresponse

getGame :: Maybe GameId -> AppM (Maybe Game)
getGame maybeGameId = do
  case maybeGameId of
    Nothing -> return Nothing
    Just gameId -> do
      db <- ask
      liftIO $ getGameById db gameId

newMove :: Maybe GameId -> Maybe SessionToken -> GameMove -> AppM (Maybe GameMove)
newMove maybeGameId maybeToken move = do
  case (maybeGameId, maybeToken) of
    (Just gameId, Just token) -> do
      db <- ask
      -- Check if the game exists
      maybeGame <- liftIO $ getGameById db gameId
      case maybeGame of
        Nothing -> do
          liftIO $ putStrLn "Move rejected: game not found"
          return Nothing
        Just game -> do
          -- Determine whose turn it is
          let currentSlot = getCurrentPlayerSlot game
          
          -- Validate token
          isValid <- liftIO $ validateToken db gameId token currentSlot
          
          if not isValid then do
            liftIO $ putStrLn $ "Move rejected: invalid token or not your turn (expected " ++ show currentSlot ++ ")"
            return Nothing
          else do
            -- Token is valid, process the move
            let updateGameFn (Game p1 p2 cards history w) = Just $ Game p1 p2 cards (move : history) w
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
getIndexHtml gameId = do
  bs <- liftIO $ Lazy.readFile "assets/index.html"
  return $ RawHtml bs



