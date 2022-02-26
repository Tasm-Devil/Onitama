{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module App where

import Api ( api, API, GameId )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Map ( Map, empty )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Conc
    ( readTVarIO, writeTVar, readTVar, atomically, newTVarIO, TVar )
import GHC.Generics ()
import Game (Game (Game), GameMove, give5Cards)
import MakeAssets ( serveAssets, Default(def) )
import Network.Wai (Application)
import Servant
    ( hoistServer,
      serve,
      Raw,
      HasServer(ServerT),
      Server,
      Tagged(Tagged),
      type (:<|>)(..),
      Handler,
      Application,
      Proxy(..) )

type Games = Map GameId Game

newtype DB = DB (TVar Games)

mkDB :: IO DB
mkDB = DB <$> newTVarIO empty

type WithAssets = API :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app :: IO Application
app = serve withAssets <$> server

type AppM = ReaderT DB Handler

server :: IO (Server WithAssets)
server = do
  assets <- serveAssets def
  db <- mkDB
  return (readerServer db :<|> Tagged assets)
  where
    readerToHandler :: DB -> AppM a -> Handler a
    readerToHandler db x = runReaderT x db
    readerServer :: DB -> ServerT API Handler
    readerServer db = hoistServer api (readerToHandler db) apiServer

apiServer :: ServerT API AppM
apiServer = newGame :<|> getAllGames :<|> getGame :<|> newMove

newGame :: AppM GameId
newGame = do
  DB db <- ask
  newCards <- liftIO give5Cards
  let newgame = Game newCards []
  liftIO $
    atomically $ do
      games <- readTVar db
      writeTVar db $ insertNewGameToDb newgame games
  games <- liftIO $ readTVarIO db
  return $ fst $ Map.findMax games
  where
    insertNewGameToDb :: Game -> Games -> Games
    insertNewGameToDb newgame games =
      let newGameId = 1 + maybe 0 fst (Map.lookupMax games)
       in Map.insert newGameId newgame games

getAllGames :: AppM [GameId]
getAllGames = do
  DB db <- ask
  games <- liftIO $ readTVarIO db
  return $ Map.keys games

getGame :: GameId -> AppM (Maybe Game)
getGame gameId = do
  DB db <- ask
  games <- liftIO $ readTVarIO db
  return $ Map.lookup gameId games

newMove :: GameId -> GameMove -> AppM (Maybe GameMove)
newMove gameId move = do
  DB db <- ask
  games <- liftIO $ readTVarIO db
  if Map.notMember gameId games
    then do return Nothing
    else do
      liftIO $
        atomically $ do
          games <- readTVar db
          writeTVar db $ updateGameInDb move gameId games
      return (Just move)
  where
    updateGameInDb :: GameMove -> GameId -> Games -> Games
    updateGameInDb move =
      let insertMoveToGame m (Game cards history) = Just $ Game cards (m : history)
       in Map.update (insertMoveToGame move)
