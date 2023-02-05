{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Api (API, GameId (..), api, RawHtml (RawHtml), APIWithAssets, apiWithAssets)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Map (Map, empty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics ()
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
    serveDirectoryWebApp,
    type (:<|>) (..),
  )
import Data.ByteString.Lazy as Lazy ( ByteString, readFile )
import Network.Wai.Application.Static
    ( staticApp, defaultFileServerSettings )

type Games = Map GameId Game

newtype DB = DB (TVar Games)

app :: IO Application
app = serve apiWithAssets <$> server

type AppM = ReaderT DB Handler

server :: IO (Server APIWithAssets)
server = do
  let assets = staticApp $ defaultFileServerSettings "assets/"
  db <- DB <$> newTVarIO empty
  return (readerServer db :<|> Tagged assets)
  where
    readerToHandler :: DB -> AppM a -> Handler a
    readerToHandler db appM = runReaderT appM db
    readerServer :: DB -> ServerT API Handler
    readerServer db = hoistServer api (readerToHandler db) apiServer

apiServer :: ServerT API AppM
apiServer = newGame :<|> getAllGames :<|> joinGame :<|> getGame :<|> newMove :<|> getIndexHtml

newGame :: AppM GameId
newGame = do
  DB db <- ask
  newCards <- liftIO give5Cards
  newUuid <- liftIO nextRandom
  let insNewGame = Map.insert (GameId newUuid) (Game "" "" newCards [])
  liftIO . atomically $ do
    modifyTVar db insNewGame
  games <- liftIO $ readTVarIO db
  return $ GameId newUuid

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
      let insertPlayNameToGame name (Game "" "" cards history) = Just $ Game name "" cards history
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

getIndexHtml :: GameId -> AppM RawHtml
getIndexHtml gameId = do
  bs <- liftIO $ Lazy.readFile "assets/index.html"
  return $ RawHtml bs