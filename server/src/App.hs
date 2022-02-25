{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE TypeOperators #-}

module App

where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Maybe (fromMaybe)
import GHC.Conc
import GHC.Generics ()
import Game ( give5Cards, GameMove, Game(Game) )
import Network.Wai (Application)
import Servant
import Api
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import MakeAssets

newtype Games = Games (TVar (Map GameId Game))

mkDB :: IO Games
mkDB = Games <$> newTVarIO (Map.singleton 0 (Game [] []))

type WithAssets = API :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app :: IO Application
app = serve withAssets <$> server

type AppM = ReaderT Games Handler

server :: IO (Server WithAssets)
server = do
  assets <- serveAssets def
  games  <- mkDB
  return (app' games :<|> Tagged assets)
  where
    nt :: Games -> AppM a -> Handler a
    nt s x = runReaderT x s
    app' :: Games -> ServerT API Handler
    app' s = hoistServer api (nt s) apiServer

apiServer :: ServerT API AppM
apiServer = newGame :<|> getGame :<|> newMove
  where
    newGame :: AppM GameId
    newGame = do
      Games p <- ask
      newCards <- liftIO give5Cards
      let newgame = Game newCards []
      liftIO $ atomically (readTVar p >>= writeTVar p . insertNewGame newgame)
      games <- liftIO $ readTVarIO p
      return $ fst $ Map.findMax games
      where
        insertNewGame :: Game -> Map GameId Game -> Map GameId Game
        insertNewGame newgame g =
          let newGameId = 1 + fst (Map.findMax g)
           in Map.insert newGameId newgame g
    getGame :: GameId -> AppM Game
    getGame gameId = do
      Games p <- ask
      games <- liftIO $ readTVarIO p
      return $ Map.findWithDefault (Game [] []) gameId games
    newMove :: GameId -> GameMove -> AppM GameMove
    newMove gameId move = do
      Games p <- ask
      games <- liftIO $ readTVarIO p
      when (Map.notMember gameId games) $ return () -- ???
      liftIO $ atomically $ readTVar p >>= writeTVar p . Map.update (insertMoveToGame move) gameId
      return move
      where
        insertMoveToGame :: GameMove -> Game -> Maybe Game
        insertMoveToGame move' (Game cards history) = Just $ Game cards (move':history)
