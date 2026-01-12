{-# LANGUAGE DeriveGeneric #-}

module Database where

import Api (GameId (..), GameStatus (..), GameSummary (..), Games, JoinGameResponse (..), SessionToken (..), gameToSummary)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
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
import Game (Color (..), Game (..), PlayerSlot (..), getCurrentPlayerSlot, give5Cards)
import System.Directory (doesFileExist)

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
  fileExists <- doesFileExist dbFilePath
  initialDB <-
    if fileExists
      then do
        fileContent <- Lazy.readFile dbFilePath
        let maybeDB = decode fileContent
        case maybeDB of
          Just db -> do
            let gameCount = Map.size (dbGames db)
            putStrLn $ "Successfully loaded database at " ++ dbFilePath ++ " with " ++ show gameCount ++ " games"
            return db
          Nothing -> do
            putStrLn $ "Failed to parse database file at " ++ dbFilePath ++ " , starting with empty DB"
            return $ DBState { dbGames = empty, dbNextId = 1, dbSessions = empty, dbHasChanged = False }
      else do
        putStrLn $ "Database file at " ++ dbFilePath ++ " not found, starting with empty DB"
        return $ DBState { dbGames = empty, dbNextId = 1, dbSessions = empty, dbHasChanged = False }
  newTVarIO initialDB

-- Save database to file with pretty printing
saveDB :: TVar DBState -> IO ()
saveDB dbVar = do
  dbState <- atomically $ do
    state <- readTVar dbVar
    let shouldSave = dbHasChanged state
    when shouldSave $
      writeTVar dbVar (state {dbHasChanged = False})
    return state

  let gameCount = Map.size (dbGames dbState)

  when (dbHasChanged dbState) $ do
    -- Use pretty printing for human-readable JSON
    let encoderConfig =
          Pretty.defConfig
            { Pretty.confIndent = Pretty.Spaces 2,
              Pretty.confCompare = compare
            }
    let encodedDB = Pretty.encodePretty' encoderConfig dbState
    Lazy.writeFile dbFilePath encodedDB
    putStrLn $ "Database with " ++ show gameCount ++ " games ( " ++ show (Lazy.length encodedDB) ++ " bytes) saved to file successfully."

-- Start periodic saving of database
startPeriodicSave :: TVar DBState -> IO ()
startPeriodicSave dbVar = do
  putStrLn "Starting periodic database save thread"
  -- Fork a thread that will save the database every 30 seconds
  _ <- forkIO $ forever $ do
    saveDB dbVar
    threadDelay (30 * 1000000) -- 30 seconds for testing (instead of 10 minutes)
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
insertGameWithNewId :: DB -> IO GameId
insertGameWithNewId (DB dbVar) = do
  newCards <- liftIO give5Cards
  gameId <- atomically $ do
    state <- readTVar dbVar
    let newId = GameId (dbNextId state)
    modifyTVar dbVar $ \s ->
      s
        { dbGames = Map.insert newId (Game { player_white = "", player_black = "", cards = newCards, history = [], winner = Nothing }) (dbGames s),
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

-- Join a game and get a session token (or retrieve existing session with validation)
joinGameWithToken :: DB -> GameId -> String -> Maybe SessionToken -> IO (Maybe JoinGameResponse)
joinGameWithToken db@(DB dbVar) gameId playerName maybeProvidedToken = do
  maybeGame <- getGameById db gameId
  case maybeGame of
    Nothing -> return Nothing
    Just game@(Game p1 p2 cards history _) -> do
      state <- readTVarIO dbVar
      let sessions = dbSessions state

      -- Check if player is already in the game
      if p1 == playerName
        then do
          -- Player1 slot is taken by this name
          case Map.lookup (gameId, PlayerWhite) sessions of
            Just existingToken -> do
              -- Token exists for Player1
              case maybeProvidedToken of
                Just providedToken | providedToken == existingToken -> do
                  -- Valid token provided, allow rejoin
                  putStrLn $ "Player " ++ playerName ++ " rejoining as Player1 with valid token"
                  return $ Just (JoinGameResponse {responseGame = game, responseToken = existingToken})
                _ -> do
                  -- No token or wrong token - reject to prevent impersonation
                  putStrLn $ "Rejecting join: " ++ playerName ++ " already exists as Player1 but wrong/no token provided"
                  return Nothing
            Nothing -> do
              -- No token exists yet (shouldn't happen, but handle it)
              putStrLn "Warning: Player1 exists but no token found, creating new session"
              token <- createSession db gameId PlayerWhite
              return $ Just (JoinGameResponse {responseGame = game, responseToken = token})
        else
          if p2 == playerName
            then do
              -- Player2 slot is taken by this name
              case Map.lookup (gameId, PlayerBlack) sessions of
                Just existingToken -> do
                  case maybeProvidedToken of
                    Just providedToken | providedToken == existingToken -> do
                      putStrLn $ "Player " ++ playerName ++ " rejoining as Player2 with valid token"
                      return $ Just (JoinGameResponse {responseGame = game, responseToken = existingToken})
                    _ -> do
                      putStrLn $ "Rejecting join: " ++ playerName ++ " already exists as Player2 but wrong/no token provided"
                      return Nothing
                Nothing -> do
                  putStrLn "Warning: Player2 exists but no token found, creating new session"
                  token <- createSession db gameId PlayerBlack
                  return $ Just (JoinGameResponse {responseGame = game, responseToken = token})
            else do
              -- New player, find empty slot
              let (slot, updatedGame)
                    | null p1 && null p2 = (PlayerWhite, Game { player_white = playerName, player_black = "", cards = cards, history = history, winner = Nothing })
                    | null p1 = (PlayerWhite, Game { player_white = playerName, player_black = p2, cards = cards, history = history, winner = Nothing })
                    | null p2 = (PlayerBlack, Game { player_white = p1, player_black = playerName, cards = cards, history = history, winner = Nothing })
                    | otherwise = (PlayerWhite, game) -- dummy, will return Nothing
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
                  return $ Just (JoinGameResponse {responseGame = updatedGame, responseToken = token})

-- Concede a game - the player with the given token admits defeat
-- Returns the winner's color if successful
concedeGame :: DB -> GameId -> SessionToken -> IO (Maybe Color)
concedeGame db@(DB dbVar) gameId token = do
  state <- readTVarIO dbVar
  let sessions = dbSessions state

  -- Find which slot this token belongs to
  let maybeSlot = findSlotByToken gameId token sessions

  case maybeSlot of
    Nothing -> do
      putStrLn $ "Concede failed: invalid token for game " ++ show gameId
      return Nothing
    Just loserSlot -> do
      let winnerColor = case loserSlot of
            PlayerWhite -> Black -- White concedes, Black wins
            PlayerBlack -> White -- Black concedes, White wins

      -- Update the game with the winner
      success <- atomically $ do
        currentState <- readTVar dbVar
        case Map.lookup gameId (dbGames currentState) of
          Nothing -> return False
          Just (Game p1 p2 cards history _) -> do
            let updatedGame = Game { player_white = p1, player_black = p2, cards = cards, history = history, winner = Just winnerColor }
            writeTVar dbVar $
              currentState
                { dbGames = Map.insert gameId updatedGame (dbGames currentState),
                  dbHasChanged = True
                }
            return True

      if success
        then do
          markDBChanged db
          putStrLn $ "Game " ++ show gameId ++ " ended: " ++ show winnerColor ++ " wins"
          return $ Just winnerColor
        else do
          putStrLn $ "Concede failed: game " ++ show gameId ++ " not found"
          return Nothing
  where
    findSlotByToken :: GameId -> SessionToken -> Map SessionKey SessionToken -> Maybe PlayerSlot
    findSlotByToken gid tok sessions
      | Map.lookup (gid, PlayerWhite) sessions == Just tok =
          Just PlayerWhite
      | Map.lookup (gid, PlayerBlack) sessions == Just tok =
          Just PlayerBlack
      | otherwise = Nothing