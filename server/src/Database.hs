{-# LANGUAGE DeriveGeneric #-}

module Database where

import Api (GameId (..), GameStatus (..), GameSummary (..), Games, SessionToken (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.ByteString.Lazy as Lazy (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as Lazy (length)
import Data.Map (Map, empty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Game (Game (Game))
import System.Directory (doesFileExist)

-- Player slot identifier: which position in the game
data PlayerSlot = Player1 | Player2
  deriving (Show, Eq, Ord, Generic)

instance ToJSON PlayerSlot

instance FromJSON PlayerSlot

-- Session key: (GameId, PlayerSlot)
type SessionKey = (GameId, PlayerSlot)

data DBState = DBState
  { dbGames :: Games,
    dbNextId :: Int,
    dbSessions :: Map SessionKey SessionToken,
    dbHasChanged :: Bool
  }
  deriving (Generic)

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
  initialDB <-
    if fileExists
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
            return $ DBState empty 1 empty False
      else do
        putStrLn "Database file not found, starting with empty DB"
        return $ DBState empty 1 empty False
  newTVarIO initialDB

-- Save database to file with pretty printing
saveDB :: TVar DBState -> IO ()
saveDB dbVar = do
  putStrLn "Checking if database needs to be saved..."
  dbState <- atomically $ do
    state <- readTVar dbVar
    let shouldSave = dbHasChanged state
    when shouldSave $
      writeTVar dbVar (state {dbHasChanged = False})
    return state

  let gameCount = Map.size (dbGames dbState)

  if dbHasChanged dbState
    then do
      putStrLn $ "Saving database with " ++ show gameCount ++ " games to " ++ dbFilePath
      -- Use pretty printing for human-readable JSON
      let encoderConfig =
            Pretty.defConfig
              { Pretty.confIndent = Pretty.Spaces 2,
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
    threadDelay (30 * 1000000) -- 30 seconds for testing (instead of 10 minutes)
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
    modifyTVar dbVar $ \state -> state {dbHasChanged = True}
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

-- Generate next game ID and insert game
insertGameWithNewId :: DB -> Game -> IO GameId
insertGameWithNewId (DB dbVar) game = do
  gameId <- atomically $ do
    state <- readTVar dbVar
    let newId = GameId (dbNextId state)
    modifyTVar dbVar $ \s ->
      s
        { dbGames = Map.insert newId game (dbGames s),
          dbNextId = dbNextId s + 1,
          dbHasChanged = True
        }
    return newId
  markDBChanged (DB dbVar)
  return gameId

insertGame :: DB -> GameId -> Game -> IO ()
insertGame (DB dbVar) gameId game = do
  atomically $ modifyTVar dbVar $ \state ->
    state {dbGames = Map.insert gameId game (dbGames state), dbHasChanged = True}
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
          writeTVar dbVar $
            state
              { dbGames = Map.insert gameId updatedGame games,
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
    { summaryId = gameId,
      summaryPlayer1 = p1,
      summaryPlayer2 = p2,
      summaryMoveCount = Prelude.length history,
      summaryStatus = determineStatus p1 p2
    }
  where
    determineStatus "" "" = WaitingForPlayers
    determineStatus "" _ = WaitingForPlayers
    determineStatus _ "" = WaitingForPlayers
    determineStatus _ _ = InProgress -- Could add more logic for completed games

-- Get all game summaries
getAllGameSummaries :: DB -> IO [GameSummary]
getAllGameSummaries (DB dbVar) = do
  state <- readTVarIO dbVar
  let games = dbGames state
  return $ map (uncurry gameToSummary) (Map.toList games)

-- Generate a new session token
generateToken :: IO SessionToken
generateToken = do
  SessionToken . toText <$> nextRandom

-- Create a session for a player joining a game
createSession :: DB -> GameId -> PlayerSlot -> IO SessionToken
createSession (DB dbVar) gameId slot = do
  token <- generateToken
  atomically $ modifyTVar dbVar $ \state ->
    state
      { dbSessions = Map.insert (gameId, slot) token (dbSessions state),
        dbHasChanged = True
      }
  markDBChanged (DB dbVar)
  putStrLn $ "Created session for game " ++ show gameId ++ ", slot " ++ show slot
  return token

-- Validate that a token is valid for making a move in a game
validateToken :: DB -> GameId -> SessionToken -> PlayerSlot -> IO Bool
validateToken (DB dbVar) gameId token expectedSlot = do
  state <- readTVarIO dbVar
  let sessions = dbSessions state
  case Map.lookup (gameId, expectedSlot) sessions of
    Just storedToken -> return $ storedToken == token
    Nothing -> return False

-- Determine which player slot should make the next move based on game history
getCurrentPlayerSlot :: Game -> PlayerSlot
getCurrentPlayerSlot (Game _ _ _ history) =
  if even (Prelude.length history) then Player1 else Player2

-- Join a game and get a session token (or retrieve existing session with validation)
joinGameWithToken :: DB -> GameId -> String -> Maybe SessionToken -> IO (Maybe (Game, SessionToken))
joinGameWithToken db@(DB dbVar) gameId playerName maybeProvidedToken = do
  maybeGame <- getGameById db gameId
  case maybeGame of
    Nothing -> return Nothing
    Just game@(Game p1 p2 cards history) -> do
      state <- readTVarIO dbVar
      let sessions = dbSessions state

      -- Check if player is already in the game
      if p1 == playerName
        then do
          -- Player1 slot is taken by this name
          case Map.lookup (gameId, Player1) sessions of
            Just existingToken -> do
              -- Token exists for Player1
              case maybeProvidedToken of
                Just providedToken | providedToken == existingToken -> do
                  -- Valid token provided, allow rejoin
                  putStrLn $ "Player " ++ playerName ++ " rejoining as Player1 with valid token"
                  return $ Just (game, existingToken)
                _ -> do
                  -- No token or wrong token - reject to prevent impersonation
                  putStrLn $ "Rejecting join: " ++ playerName ++ " already exists as Player1 but wrong/no token provided"
                  return Nothing
            Nothing -> do
              -- No token exists yet (shouldn't happen, but handle it)
              putStrLn "Warning: Player1 exists but no token found, creating new session"
              token <- createSession db gameId Player1
              return $ Just (game, token)
        else
          if p2 == playerName
            then do
              -- Player2 slot is taken by this name
              case Map.lookup (gameId, Player2) sessions of
                Just existingToken -> do
                  case maybeProvidedToken of
                    Just providedToken | providedToken == existingToken -> do
                      putStrLn $ "Player " ++ playerName ++ " rejoining as Player2 with valid token"
                      return $ Just (game, existingToken)
                    _ -> do
                      putStrLn $ "Rejecting join: " ++ playerName ++ " already exists as Player2 but wrong/no token provided"
                      return Nothing
                Nothing -> do
                  putStrLn "Warning: Player2 exists but no token found, creating new session"
                  token <- createSession db gameId Player2
                  return $ Just (game, token)
            else do
              -- New player, find empty slot
              let (slot, updatedGame)
                    | null p1 && null p2 = (Player1, Game playerName "" cards history)
                    | null p1 = (Player1, Game playerName p2 cards history)
                    | null p2 = (Player2, Game p1 playerName cards history)
                    | otherwise = (Player1, game) -- dummy, will return Nothing
              if game == updatedGame
                then do
                  -- Game was full, couldn't join
                  putStrLn $ "Game is full, rejecting join for " ++ playerName
                  return Nothing
                else do
                  -- Update game and create session
                  atomically $ modifyTVar dbVar $ \state ->
                    state
                      { dbGames = Map.insert gameId updatedGame (dbGames state),
                        dbHasChanged = True
                      }
                  markDBChanged db
                  token <- createSession db gameId slot
                  putStrLn $ "New player " ++ playerName ++ " joined as " ++ show slot
                  return $ Just (updatedGame, token)