{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever, guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.ByteString.Lazy as Lazy (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as Lazy (length)
import Data.Map (Map, empty)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import Types

-- Cleanup configuration: timeout durations in hours
data CleanupConfig = CleanupConfig
  { cleanupWaitingForPlayers :: NominalDiffTime, -- hours for games waiting for players
    cleanupInProgress :: NominalDiffTime, -- hours for games in progress
    cleanupCompleted :: NominalDiffTime -- hours for completed games
  }
  deriving (Generic, Show)

instance FromJSON CleanupConfig

instance ToJSON CleanupConfig

-- Pure data stored in JSON (games only)
data DBData = DBData
  { dbGames :: Map GameId Game
  }
  deriving (Generic)

instance FromJSON DBData

instance ToJSON DBData

-- Database wrapper with runtime state
data DB = DB
  { dbData :: TVar DBData,
    dbFilePath :: Maybe FilePath,
    dbNextGameId :: TVar Int,
    dbHasChanged :: TVar Bool,
    dbCleanupConfig :: CleanupConfig
  }

-- Load database from file or create a new one if file doesn't exist
loadDB :: Maybe FilePath -> Bool -> CleanupConfig -> IO (TVar DBData)
loadDB Nothing _ _ = do
  putStrLn "Running with in-memory database (no persistence)"
  newTVarIO (DBData {dbGames = empty})
loadDB (Just dbFilePath) resetDB _cleanupCfg = do
  fileExists <- doesFileExist dbFilePath

  -- If reset flag is set, skip loading and start fresh
  let shouldLoad = fileExists && not resetDB

  when resetDB $ do
    putStrLn $ "Resetting database (--reset-db flag set), ignoring existing file at " ++ dbFilePath

  let emptyDB = DBData {dbGames = empty}

  initialDB <-
    if shouldLoad
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
            return emptyDB
      else do
        -- Only print "not found" if file truly doesn't exist (not reset-db case)
        unless fileExists $
          putStrLn $
            "Database file at " ++ dbFilePath ++ " not found, starting with empty DB"
        return emptyDB
  newTVarIO initialDB

-- Save database to file with pretty printing (no-op when no file configured)
saveDB :: DB -> IO ()
saveDB db = case dbFilePath db of
  Nothing -> return ()
  Just filePath -> do
    let dataVar = dbData db
        hasChangedVar = dbHasChanged db
    (currentData, shouldSave) <- atomically $ do
      state <- readTVar dataVar
      changed <- readTVar hasChangedVar
      when changed $
        writeTVar hasChangedVar False
      return (state, changed)

    let gameCount = Map.size (dbGames currentData)

    when shouldSave $ do
      -- Use pretty printing for human-readable JSON
      let encoderConfig =
            Pretty.defConfig
              { Pretty.confIndent = Pretty.Spaces 2,
                Pretty.confCompare = compare
              }
      let encodedDB = Pretty.encodePretty' encoderConfig currentData
      Lazy.writeFile filePath encodedDB
      putStrLn $ "Database with " ++ show gameCount ++ " games ( " ++ show (Lazy.length encodedDB) ++ " bytes) saved to file successfully."

-- Cleanup old games based on their status and lastActivity
cleanupOldGames :: DB -> IO () -> IO ()
cleanupOldGames (DB dataVar _ _ hasChangedVar config) onCleanup = do
  now <- getCurrentTime
  state <- readTVarIO dataVar
  let games = dbGames state

      -- Determine timeout for each game based on status
      -- Timeout <= 0 means never clean up (disabled)
      shouldDelete gameId game =
        let status = determineGameStatus game
            timeout = case status of
              WaitingForPlayers -> cleanupWaitingForPlayers config
              InProgress -> cleanupInProgress config
              Completed -> cleanupCompleted config
            expirationTime = addUTCTime timeout (lastActivity game)
         in timeout > 0 && now > expirationTime

      gamesToDelete = Map.filterWithKey shouldDelete games
      gameCount = Map.size gamesToDelete

  when (gameCount > 0) $ do
    putStrLn $ "Cleaning up " ++ show gameCount ++ " expired games"
    atomically $ do
      modifyTVar dataVar $ \s ->
        s {dbGames = Map.difference (dbGames s) gamesToDelete}
      writeTVar hasChangedVar True
    onCleanup
  where
    determineGameStatus game
      | isJust (winner game) = Completed
      | isNothing (player_white game) || isNothing (player_black game) = WaitingForPlayers
      | otherwise = InProgress

-- Start periodic saving of database (skipped when no file configured)
startPeriodicSave :: DB -> Int -> IO ()
startPeriodicSave db saveIntervalMinutes = case dbFilePath db of
  Nothing -> putStrLn "No database file configured, skipping periodic save"
  Just _ -> do
    putStrLn $ "Starting periodic database save thread (interval: " ++ show saveIntervalMinutes ++ " minutes)"
    void $ forkIO $ forever $ do
      saveDB db
      threadDelay (saveIntervalMinutes * 60 * 1000000) -- Convert seconds to microseconds

-- Start periodic cleanup of old games
startPeriodicCleanup :: DB -> Int -> Bool -> IO () -> IO ()
startPeriodicCleanup db cleanupIntervalMinutes enabled onCleanup =
  if enabled
    then do
      putStrLn $ "Starting periodic game cleanup thread (interval: " ++ show cleanupIntervalMinutes ++ " minutes)"
      void $ forkIO $ forever $ do
        cleanupOldGames db onCleanup
        threadDelay (cleanupIntervalMinutes * 60 * 1000000) -- Convert minutes to microseconds
    else
      putStrLn "Game cleanup disabled (--no-cleanup flag set)"

-- Log current database state
logDBState :: String -> DB -> IO ()
logDBState prefix (DB dataVar _ _ hasChangedVar _) = do
  dbData' <- readTVarIO dataVar
  changed <- readTVarIO hasChangedVar
  let gameCount = Map.size (dbGames dbData')
      changeStatus = if changed then "changed" else "unchanged"
  putStrLn $ prefix ++ ": " ++ show gameCount ++ " games, status: " ++ changeStatus

-- Initialize database with TVar and configuration
initDB :: Maybe FilePath -> Bool -> CleanupConfig -> Int -> Int -> Bool -> IO () -> IO DB
initDB filePath resetDB cleanupConfig saveIntervalMinutes cleanupIntervalMinutes cleanupEnabled onCleanup = do
  dataVar <- loadDB filePath resetDB cleanupConfig

  -- Calculate next IDs from existing data
  state <- readTVarIO dataVar
  let nextGameId = calculateNextId (dbGames state)

  putStrLn $ "Calculated next game ID: " ++ show nextGameId

  -- Create runtime state TVars
  nextGameIdVar <- newTVarIO nextGameId
  hasChangedVar <- newTVarIO False

  let db =
        DB
          { dbData = dataVar,
            dbFilePath = filePath,
            dbNextGameId = nextGameIdVar,
            dbHasChanged = hasChangedVar,
            dbCleanupConfig = cleanupConfig
          }

  startPeriodicSave db saveIntervalMinutes
  startPeriodicCleanup db cleanupIntervalMinutes cleanupEnabled onCleanup
  return db
  where
    calculateNextId items
      | Map.null items = 1
      | otherwise = maximum (Map.keys items) + 1

-- Helper functions for accessing and modifying the database
getGameById :: DB -> GameId -> IO (Maybe Game)
getGameById (DB dataVar _ _ _ _) gameId = do
  state <- readTVarIO dataVar
  return $ Map.lookup gameId (dbGames state)

-- Insert a game and return its new ID
insertGameWithNewId :: DB -> Game -> IO GameId
insertGameWithNewId (DB dataVar _ nextGameIdVar hasChangedVar _) game =
  atomically $ do
    currentId <- readTVar nextGameIdVar
    modifyTVar dataVar $ \s ->
      s {dbGames = Map.insert currentId game (dbGames s)}
    writeTVar nextGameIdVar (currentId + 1)
    writeTVar hasChangedVar True
    return currentId

updateGame :: DB -> GameId -> (Game -> Game) -> IO Bool
updateGame (DB dataVar _ _ hasChangedVar _) gameId updateFn =
  atomically $ do
    state <- readTVar dataVar
    let games = dbGames state
    case Map.lookup gameId games of
      Nothing -> return False
      Just game -> do
        writeTVar dataVar $
          state {dbGames = Map.insert gameId (updateFn game) games}
        writeTVar hasChangedVar True
        return True

-- Get a game with player names for client display (pure projection)
getGameWithNames :: DB -> GameId -> IO (Maybe GameWithNames)
getGameWithNames db gameId = do
  maybeGame <- getGameById db gameId
  return $ fmap gameToGameWithNames maybeGame

-- Get all game summaries
getAllGameSummaries :: DB -> IO [GameSummary]
getAllGameSummaries (DB dataVar _ _ _ _) = do
  state <- readTVarIO dataVar
  let games = dbGames state
  return $ reverse $ map (uncurry gameToSummary) (Map.toList games)

-- | Convert Maybe to ExceptT, throwing the given error on Nothing.
noteE :: (Monad m) => e -> Maybe a -> ExceptT e m a
noteE err = maybe (throwE err) return

-- Join a game with OIDC user identity
joinGame :: DB -> GameId -> OidcUserId -> Text -> IO (Either JoinError JoinGameResponse)
joinGame db@(DB dataVar _ _ hasChangedVar _) gameId userId displayName = runExceptT $ do
  -- Look up game
  game <- noteE JEGameNotFound =<< liftIO (getGameById db gameId)

  -- Check if user is already in this game (rejoin)
  if Just userId == player_white game || Just userId == player_black game
    then do
      liftIO $ putStrLn $ "Player " ++ T.unpack displayName ++ " already in game (rejoin)"
      let gameWithNames = gameToGameWithNames game
          playerName = if Just userId == player_white game then player_white_name game else player_black_name game
      return $ JoinGameResponse {responseGame = gameWithNames, responsePlayerName = playerName}
    else do
      -- Try to join an empty slot
      now <- liftIO getCurrentTime
      let updatedGame
            | isNothing (player_white game) = Just $ game {player_white = Just userId, player_white_name = displayName, lastActivity = now}
            | isNothing (player_black game) = Just $ game {player_black = Just userId, player_black_name = displayName, lastActivity = now}
            | otherwise = Nothing -- Game is full
      newGame <- noteE JEGameFull updatedGame
      liftIO $ do
        atomically $ do
          modifyTVar dataVar $ \state ->
            state {dbGames = Map.insert gameId newGame (dbGames state)}
          writeTVar hasChangedVar True
        putStrLn $ "Player " ++ T.unpack displayName ++ " joined game"
      let gameWithNames = gameToGameWithNames newGame
      return $ JoinGameResponse {responseGame = gameWithNames, responsePlayerName = displayName}

-- Concede a game - the identified user admits defeat
concedeGame :: DB -> GameId -> OidcUserId -> IO (Maybe Color)
concedeGame db@(DB dataVar _ _ hasChangedVar _) gameId userId = runMaybeT $ do
  game <- MaybeT $ getGameById db gameId
  loserSlot <- MaybeT $ return $ playerSlotInGame game userId
  let winnerColor = case loserSlot of
        White -> Black
        Black -> White

  -- Update the game with the winner (re-read inside STM for consistency)
  success <- liftIO $ atomically $ do
    currentState <- readTVar dataVar
    case Map.lookup gameId (dbGames currentState) of
      Nothing -> return False
      Just currentGame -> do
        let updatedGame = currentGame {winner = Just winnerColor}
        writeTVar dataVar $
          currentState {dbGames = Map.insert gameId updatedGame (dbGames currentState)}
        writeTVar hasChangedVar True
        return True
  guard success
  liftIO $ putStrLn $ "Game " ++ show gameId ++ " ended: " ++ show winnerColor ++ " wins"
  return winnerColor
  where
    playerSlotInGame game uid
      | player_white game == Just uid = Just White
      | player_black game == Just uid = Just Black
      | otherwise = Nothing
