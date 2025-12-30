{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DeriveGeneric #-}

module Database where

import Api (GameId (..), Games, GameSummary (..), GameStatus (..))
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map, empty)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID, toString, fromString)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Game (Game (Game))
import Data.ByteString.Lazy as Lazy (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as Lazy (length)
import Data.Aeson (FromJSON, ToJSON, decode, encode, FromJSONKey(..), ToJSONKey(..))
import Data.Aeson.Types (toJSONKeyText, FromJSONKeyFunction(..))
import qualified Data.Aeson.Encode.Pretty as Pretty
import System.Directory (doesFileExist)
import qualified Data.Text as T


data DBState = DBState
  { dbGames :: Games
  , dbHasChanged :: Bool
  } deriving (Generic)

instance FromJSON DBState
instance ToJSON DBState

newtype DB = DB (TVar DBState)

dbFilePath :: FilePath
dbFilePath = "gamedb.json"

-- Load database from file or create a new one if file doesn't exist
loadDB :: IO (TVar DBState)
loadDB = do
  putStrLn $ "Looking for database file at: " ++ dbFilePath
  fileExists <- doesFileExist dbFilePath
  initialDB <- if fileExists
    then do
      putStrLn "Database file found, loading..."
      fileContent <- Lazy.readFile dbFilePath
      let maybeDB = decode fileContent
      case maybeDB of
        Just db -> do
          let gameCount = Map.size (dbGames db)
          putStrLn $ "Successfully loaded database with " ++ show gameCount ++ " games"
          return db
        Nothing -> do
          putStrLn "Failed to parse database file, starting with empty DB"
          return $ DBState empty False
    else do
      putStrLn "Database file not found, starting with empty DB"
      return $ DBState empty False
  newTVarIO initialDB

-- Save database to file with pretty printing
saveDB :: TVar DBState -> IO ()
saveDB dbVar = do
  putStrLn "Checking if database needs to be saved..."
  dbState <- atomically $ do
    state <- readTVar dbVar
    let shouldSave = dbHasChanged state
    when shouldSave $
      writeTVar dbVar (state { dbHasChanged = False })
    return state
  
  let gameCount = Map.size (dbGames dbState)
  
  if dbHasChanged dbState
    then do
      putStrLn $ "Saving database with " ++ show gameCount ++ " games to " ++ dbFilePath
      -- Use pretty printing for human-readable JSON
      let encoderConfig = Pretty.defConfig { 
                            Pretty.confIndent = Pretty.Spaces 2,
                            Pretty.confCompare = compare
                          }
      let encodedDB = Pretty.encodePretty' encoderConfig dbState
      putStrLn $ "JSON size: " ++ show (Lazy.length encodedDB) ++ " bytes"
      Lazy.writeFile dbFilePath encodedDB
      putStrLn "Database saved to file successfully (pretty-printed)"
    else putStrLn $ "No changes to save. Current games: " ++ show gameCount

-- Start periodic saving of database
startPeriodicSave :: TVar DBState -> IO ()
startPeriodicSave dbVar = do
  putStrLn "Starting periodic database save thread"
  -- Fork a thread that will save the database every 30 seconds for testing
  _ <- forkIO $ forever $ do
    putStrLn "Periodic save triggered"
    saveDB dbVar
    threadDelay (30 * 1000000)  -- 30 seconds for testing (instead of 10 minutes)
  putStrLn "Periodic save thread started"
  return ()

-- Log current database state
logDBState :: String -> DB -> IO ()
logDBState prefix (DB dbVar) = do
  dbState <- readTVarIO dbVar
  let gameCount = Map.size (dbGames dbState)
  let changeStatus = if dbHasChanged dbState then "changed" else "unchanged"
  putStrLn $ prefix ++ ": " ++ show gameCount ++ " games, status: " ++ changeStatus

-- Mark database as changed
markDBChanged :: DB -> IO ()
markDBChanged db@(DB dbVar) = do
  putStrLn "Marking database as changed"
  atomically $ do
    modifyTVar dbVar $ \state -> state { dbHasChanged = True }
  logDBState "After marking changed" db

-- Initialize database with TVar
initDB :: IO DB
initDB = do
  dbVar <- loadDB
  startPeriodicSave dbVar
  return $ DB dbVar

-- Helper functions for accessing and modifying the database
getGames :: DB -> IO Games
getGames (DB dbVar) = do
  state <- readTVarIO dbVar
  return $ dbGames state

getGameById :: DB -> GameId -> IO (Maybe Game)
getGameById (DB dbVar) gameId = do
  state <- readTVarIO dbVar
  return $ Map.lookup gameId (dbGames state)

insertGame :: DB -> GameId -> Game -> IO ()
insertGame (DB dbVar) gameId game = do
  atomically $ modifyTVar dbVar $ \state ->
    state { dbGames = Map.insert gameId game (dbGames state), dbHasChanged = True }
  markDBChanged (DB dbVar)

updateGame :: DB -> GameId -> (Game -> Maybe Game) -> IO Bool
updateGame (DB dbVar) gameId updateFn = do
  result <- atomically $ do
    state <- readTVar dbVar
    let games = dbGames state
    case Map.lookup gameId games of
      Nothing -> return False
      Just game -> case updateFn game of
        Nothing -> return False
        Just updatedGame -> do
          writeTVar dbVar $ state { 
            dbGames = Map.insert gameId updatedGame games, 
            dbHasChanged = True 
          }
          return True
  when result $ markDBChanged (DB dbVar)
  return result

forceSave :: DB -> IO ()
forceSave (DB dbVar) = do
  putStrLn "Forcing immediate database save"
  saveDB dbVar

-- Create a game summary from a full game
gameToSummary :: GameId -> Game -> GameSummary
gameToSummary gameId (Game p1 p2 cards history) =
  GameSummary
    { summaryId = gameId
    , summaryPlayer1 = p1
    , summaryPlayer2 = p2
    , summaryMoveCount = Prelude.length history
    , summaryStatus = determineStatus p1 p2
    }
  where
    determineStatus "" "" = WaitingForPlayers
    determineStatus "" _  = WaitingForPlayers
    determineStatus _  "" = WaitingForPlayers
    determineStatus _  _  = InProgress  -- Could add more logic for completed games

-- Get all game summaries
getAllGameSummaries :: DB -> IO [GameSummary]
getAllGameSummaries (DB dbVar) = do
  state <- readTVarIO dbVar
  let games = dbGames state
  return $ map (uncurry gameToSummary) (Map.toList games)