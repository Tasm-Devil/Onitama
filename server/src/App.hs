{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module App where

import Api (API, GameId, api)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Map (Map, empty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, fromJust, isNothing)
import GHC.Generics ()
import Game (Game (Game), GameMove, give5Cards)
import MakeAssets (Default (def), serveAssets)
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
    readerToHandler db appM = runReaderT appM db
    readerServer :: DB -> ServerT API Handler
    readerServer db = hoistServer api (readerToHandler db) apiServer

apiServer :: ServerT API AppM
apiServer = newGame :<|> getAllGames :<|> joinGame :<|> getGame :<|> newMove

newGame :: AppM GameId
newGame = do
  DB db <- ask
  newCards <- liftIO give5Cards
  liftIO . atomically $ do
    modifyTVar db . insertNewGameToDb $ Game "" "" newCards []
  games <- liftIO $ readTVarIO db
  return . fst $ Map.findMax games
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

joinGame :: GameId -> Maybe String -> AppM (Maybe Game) -- adds playername to game of id
joinGame gameId name = do
  DB db <- ask
  games <- liftIO $ readTVarIO db
  if Map.notMember gameId games || isNothing name
    then do return Nothing
    else do
      liftIO . atomically $ do
        modifyTVar db $ updateDb (fromJust name) gameId
      games <- liftIO $ readTVarIO db
      return $ Map.lookup gameId games
  where
    updateDb :: String -> GameId -> Games -> Games
    updateDb name =
      let 
        insertPlayNameToGame name (Game "" "" cards history) = Just $ Game name "" cards history
        insertPlayNameToGame name (Game "" p2 cards history) = Just $ Game name p2 cards history
        insertPlayNameToGame name (Game p1 "" cards history) = Just $ Game p1 name cards history
        insertPlayNameToGame name (Game p1 p2 cards history) = Just $ Game p1 p2 cards history
       in Map.update (insertPlayNameToGame name)

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
      liftIO . atomically $ do
        modifyTVar db $ updateDb move gameId
      return (Just move)
  where
    updateDb :: GameMove -> GameId -> Games -> Games
    updateDb move =
      let insertMoveToGame m (Game p1 p2 cards history) = Just $ Game p1 p2 cards (m : history)
       in Map.update (insertMoveToGame move)
