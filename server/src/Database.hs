{-# LANGUAGE DeriveGeneric #-}

module Database where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever, guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.ByteString.Lazy as Lazy (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as Lazy (length)
import Data.List (find)
import Data.Map (Map, empty)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import Types

-- Player type: stores player identity and token
data Player = Player
  { playerId :: PlayerId,
    playerName :: Text,
    playerToken :: SessionToken
  }
  deriving (Eq, Show, Generic)

instance FromJSON Player

instance ToJSON Player

-- Cleanup configuration: timeout durations in hours
data CleanupConfig = CleanupConfig
  { cleanupWaitingForPlayers :: NominalDiffTime, -- hours for games waiting for players
    cleanupInProgress :: NominalDiffTime, -- hours for games in progress
    cleanupCompleted :: NominalDiffTime -- hours for completed games
  }
  deriving (Generic, Show)

instance FromJSON CleanupConfig

instance ToJSON CleanupConfig

-- Pure data stored in JSON (games and players only)
data DBData = DBData
  { dbGames :: Map GameId Game,
    dbPlayers :: Map PlayerId Player
  }
  deriving (Generic)

instance FromJSON DBData

instance ToJSON DBData

-- Database wrapper with runtime state
data DB = DB
  { dbData :: TVar DBData,
    dbFilePath :: Maybe FilePath,
    dbNextGameId :: TVar Int,
    dbNextPlayerId :: TVar Int,
    dbHasChanged :: TVar Bool,
    dbCleanupConfig :: CleanupConfig
  }

-- Load database from file or create a new one if file doesn't exist
loadDB :: Maybe FilePath -> Bool -> CleanupConfig -> IO (TVar DBData)
loadDB Nothing _ _ = do
  putStrLn "Running with in-memory database (no persistence)"
  newTVarIO (DBData {dbGames = empty, dbPlayers = empty})
loadDB (Just dbFilePath) resetDB _cleanupCfg = do
  fileExists <- doesFileExist dbFilePath

  -- If reset flag is set, skip loading and start fresh
  let shouldLoad = fileExists && not resetDB

  when resetDB $ do
    putStrLn $ "Resetting database (--reset-db flag set), ignoring existing file at " ++ dbFilePath

  let emptyDB = DBData {dbGames = empty, dbPlayers = empty}

  initialDB <-
    if shouldLoad
      then do
        fileContent <- Lazy.readFile dbFilePath
        let maybeDB = decode fileContent
        case maybeDB of
          Just db -> do
            let gameCount = Map.size (dbGames db)
                playerCount = Map.size (dbPlayers db)
            putStrLn $ "Successfully loaded database at " ++ dbFilePath ++ " with " ++ show gameCount ++ " games and " ++ show playerCount ++ " players"
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
cleanupOldGames (DB dataVar _ _ _ hasChangedVar config) onCleanup = do
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
logDBState prefix (DB dataVar _ _ _ hasChangedVar _) = do
  dbData <- readTVarIO dataVar
  changed <- readTVarIO hasChangedVar
  let gameCount = Map.size (dbGames dbData)
      changeStatus = if changed then "changed" else "unchanged"
  putStrLn $ prefix ++ ": " ++ show gameCount ++ " games, status: " ++ changeStatus

-- Initialize database with TVar and configuration
initDB :: Maybe FilePath -> Bool -> CleanupConfig -> Int -> Int -> Bool -> IO () -> IO DB
initDB filePath resetDB cleanupConfig saveIntervalMinutes cleanupIntervalMinutes cleanupEnabled onCleanup = do
  dataVar <- loadDB filePath resetDB cleanupConfig

  -- Calculate next IDs from existing data
  state <- readTVarIO dataVar
  let nextGameId = calculateNextId (dbGames state)
      nextPlayerId = calculateNextId (dbPlayers state)

  putStrLn $ "Calculated next game ID: " ++ show nextGameId
  putStrLn $ "Calculated next player ID: " ++ show nextPlayerId

  -- Create runtime state TVars
  nextGameIdVar <- newTVarIO nextGameId
  nextPlayerIdVar <- newTVarIO nextPlayerId
  hasChangedVar <- newTVarIO False

  let db =
        DB
          { dbData = dataVar,
            dbFilePath = filePath,
            dbNextGameId = nextGameIdVar,
            dbNextPlayerId = nextPlayerIdVar,
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
getGameById (DB dataVar _ _ _ _ _) gameId = do
  state <- readTVarIO dataVar
  return $ Map.lookup gameId (dbGames state)

-- Insert a game and return its new ID
insertGameWithNewId :: DB -> Game -> IO GameId
insertGameWithNewId (DB dataVar _ nextGameIdVar _ hasChangedVar _) game =
  atomically $ do
    currentId <- readTVar nextGameIdVar
    modifyTVar dataVar $ \s ->
      s {dbGames = Map.insert currentId game (dbGames s)}
    writeTVar nextGameIdVar (currentId + 1)
    writeTVar hasChangedVar True
    return currentId

updateGame :: DB -> GameId -> (Game -> Game) -> IO Bool
updateGame (DB dataVar _ _ _ hasChangedVar _) gameId updateFn =
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

-- | Look up a player's name by ID, returning empty text if not found.
resolvePlayerName :: DB -> Maybe PlayerId -> IO Text
resolvePlayerName _ Nothing = return T.empty
resolvePlayerName db (Just pid) = maybe T.empty playerName <$> getPlayerById db pid

-- Convert a Game to GameWithNames by looking up player names
gameToGameWithNames :: DB -> Game -> IO GameWithNames
gameToGameWithNames db game = do
  whiteName <- resolvePlayerName db (player_white game)
  blackName <- resolvePlayerName db (player_black game)
  return $
    GameWithNames
      { gameWhiteName = whiteName,
        gameBlackName = blackName,
        gameCards = cards game,
        gameHistory = map fst (history game),
        gameWinner = winner game,
        gameCreatedAt = createdAt game,
        gameLastActivity = lastActivity game
      }

-- Get a game with player names for client display
getGameWithNames :: DB -> GameId -> IO (Maybe GameWithNames)
getGameWithNames db gameId = do
  maybeGame <- getGameById db gameId
  traverse (gameToGameWithNames db) maybeGame

-- Get all game summaries
getAllGameSummaries :: DB -> IO [GameSummary]
getAllGameSummaries db@(DB dataVar _ _ _ _ _) = do
  state <- readTVarIO dataVar
  let games = dbGames state
  reverse <$> mapM (uncurry $ gameIdAndGameToSummary db) (Map.toList games)
  where
    gameIdAndGameToSummary :: DB -> GameId -> Game -> IO GameSummary
    gameIdAndGameToSummary database gid game = do
      whiteName <- resolvePlayerName database (player_white game)
      blackName <- resolvePlayerName database (player_black game)
      return $ gameToSummary gid whiteName blackName game

-- Player Management Functions

-- Find player by token (search through all players)
getPlayerByToken :: DB -> SessionToken -> IO (Maybe Player)
getPlayerByToken (DB dataVar _ _ _ _ _) token = do
  state <- readTVarIO dataVar
  return $ find (\p -> playerToken p == token) (Map.elems (dbPlayers state))

-- Find player by name
getPlayerByName :: DB -> Text -> IO (Maybe Player)
getPlayerByName (DB dataVar _ _ _ _ _) name = do
  state <- readTVarIO dataVar
  return $ find (\p -> playerName p == name) (Map.elems (dbPlayers state))

-- Get player by ID
getPlayerById :: DB -> PlayerId -> IO (Maybe Player)
getPlayerById (DB dataVar _ _ _ _ _) pid = do
  state <- readTVarIO dataVar
  return $ Map.lookup pid (dbPlayers state)

-- Create a new player with unique ID and token
createPlayer :: DB -> Text -> IO Player
createPlayer (DB dataVar _ _ nextPlayerIdVar hasChangedVar _) name = do
  token <- generateToken
  atomically $ do
    pid <- readTVar nextPlayerIdVar
    let newPlayer = Player {playerId = pid, playerName = name, playerToken = token}
    modifyTVar dataVar $ \s ->
      s {dbPlayers = Map.insert pid newPlayer (dbPlayers s)}
    writeTVar nextPlayerIdVar (pid + 1)
    writeTVar hasChangedVar True
    return newPlayer
  where
    generateToken = SessionToken . toText <$> nextRandom

-- | Convert Maybe to ExceptT, throwing the given error on Nothing.
noteE :: (Monad m) => e -> Maybe a -> ExceptT e m a
noteE err = maybe (throwE err) return

-- Join a game with player token
-- If token provided: MUST be valid (rejects invalid tokens)
-- If no token: name MUST be available (rejects duplicate names)
joinGameWithToken :: DB -> GameId -> Text -> Maybe SessionToken -> IO (Either JoinError JoinGameResponse)
joinGameWithToken db@(DB dataVar _ _ _ hasChangedVar _) gameId playerNameText maybeProvidedToken = runExceptT $ do
  -- Validate name is not empty/whitespace
  let trimmedName = T.strip playerNameText
  when (T.null trimmedName) $ throwE JEInvalidName

  -- Look up game
  game <- noteE JEGameNotFound =<< liftIO (getGameById db gameId)

  -- Resolve player identity from token or name
  player <- resolvePlayer db trimmedName maybeProvidedToken
  let pid = playerId player

  -- Check if player is already in this game (rejoin)
  if Just pid == player_white game || Just pid == player_black game
    then do
      liftIO $ putStrLn $ "Player " ++ T.unpack (playerName player) ++ " already in game"
      gameWithNames <- liftIO $ gameToGameWithNames db game
      return $ JoinGameResponse {responseGame = gameWithNames, responseToken = playerToken player, responsePlayerName = playerName player}
    else do
      -- Try to join an empty slot
      now <- liftIO getCurrentTime
      let updatedGame
            | isNothing (player_white game) = Just $ game {player_white = Just pid, lastActivity = now}
            | isNothing (player_black game) = Just $ game {player_black = Just pid, lastActivity = now}
            | otherwise = Nothing -- Game is full
      newGame <- noteE JEGameFull updatedGame
      liftIO $ do
        atomically $ do
          modifyTVar dataVar $ \state ->
            state {dbGames = Map.insert gameId newGame (dbGames state)}
          writeTVar hasChangedVar True
        putStrLn $ "Player " ++ T.unpack (playerName player) ++ " joined game"
      gameWithNames <- liftIO $ gameToGameWithNames db newGame
      return $ JoinGameResponse {responseGame = gameWithNames, responseToken = playerToken player, responsePlayerName = playerName player}

-- | Resolve player identity: use existing token, or create new player by name.
resolvePlayer :: DB -> Text -> Maybe SessionToken -> ExceptT JoinError IO Player
resolvePlayer db name maybeToken = case maybeToken of
  Just token -> do
    maybePlayer <- liftIO $ getPlayerByToken db token
    case maybePlayer of
      Just p -> do
        liftIO $ putStrLn $ "Valid token for player: " ++ T.unpack (playerName p)
        return p
      Nothing -> do
        liftIO $ putStrLn $ "Invalid token provided, treating as new player: " ++ T.unpack name
        findOrCreatePlayer db name
  Nothing -> do
    liftIO $ putStrLn $ "No token provided, checking if name available: " ++ T.unpack name
    findOrCreatePlayer db name

-- | Create a new player if the name is available, otherwise throw JENameTaken.
findOrCreatePlayer :: DB -> Text -> ExceptT JoinError IO Player
findOrCreatePlayer db name = do
  existing <- liftIO $ getPlayerByName db name
  case existing of
    Just _ -> do
      liftIO $ putStrLn $ "Name already taken: " ++ T.unpack name
      throwE JENameTaken
    Nothing -> do
      newPlayer <- liftIO $ createPlayer db name
      liftIO $ putStrLn $ "Created new player: " ++ T.unpack name
      return newPlayer

-- Concede a game - the player with the given token admits defeat
concedeGame :: DB -> GameId -> SessionToken -> IO (Maybe Color)
concedeGame db@(DB dataVar _ _ _ hasChangedVar _) gameId token = runMaybeT $ do
  player <- MaybeT $ getPlayerByToken db token
  game <- MaybeT $ getGameById db gameId
  loserSlot <- MaybeT $ return $ playerSlotInGame game (playerId player)
  let winnerColor = case loserSlot of
        PlayerWhite -> Black
        PlayerBlack -> White

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
    playerSlotInGame game pid
      | player_white game == Just pid = Just PlayerWhite
      | player_black game == Just pid = Just PlayerBlack
      | otherwise = Nothing