{-# LANGUAGE DeriveGeneric #-}

module Database where

import Api (GameId (..), GameStatus (..), GameSummary (..), GameWithNames (..), JoinError (..), JoinGameResponse (..), SessionToken (..), gameToSummary)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever, unless, when)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.ByteString.Lazy as Lazy (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as Lazy (length)
import Data.List (find)
import Data.Map (Map, empty)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Game (Color (..), Game (..), PlayerId, PlayerSlot (..))
import System.Directory (doesFileExist)

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

-- Calculate next ID from existing items
calculateNextId :: (Ord k, Enum k, Num k) => Map k v -> k
calculateNextId items =
  if Map.null items
    then 1
    else maximum (Map.keys items) + 1

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

-- Determine game status from Game data
determineGameStatus :: Game -> GameStatus
determineGameStatus (Game maybeWhiteId maybeBlackId _ _ maybeWinner _ _) =
  case maybeWinner of
    Just _ -> Completed
    Nothing ->
      case (maybeWhiteId, maybeBlackId) of
        (Nothing, Nothing) -> WaitingForPlayers
        (Nothing, _) -> WaitingForPlayers
        (_, Nothing) -> WaitingForPlayers
        _ -> InProgress

-- Start periodic saving of database (skipped when no file configured)
startPeriodicSave :: DB -> Int -> IO ()
startPeriodicSave db saveIntervalMinutes = case dbFilePath db of
  Nothing -> putStrLn "No database file configured, skipping periodic save"
  Just _ -> do
    putStrLn $ "Starting periodic database save thread (interval: " ++ show saveIntervalMinutes ++ " minutes)"
    _ <- forkIO $ forever $ do
      saveDB db
      threadDelay (saveIntervalMinutes * 60 * 1000000) -- Convert seconds to microseconds
    return ()

-- Start periodic cleanup of old games
startPeriodicCleanup :: DB -> Int -> Bool -> IO () -> IO ()
startPeriodicCleanup db cleanupIntervalMinutes enabled onCleanup = do
  if enabled
    then do
      putStrLn $ "Starting periodic game cleanup thread (interval: " ++ show cleanupIntervalMinutes ++ " minutes)"
      _ <- forkIO $ forever $ do
        cleanupOldGames db onCleanup
        threadDelay (cleanupIntervalMinutes * 60 * 1000000) -- Convert minutes to microseconds
      return ()
    else do
      putStrLn "Game cleanup disabled (--no-cleanup flag set)"
      return ()

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

  let db = DB
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

-- Convert a Game to GameWithNames by looking up player names
gameToGameWithNames :: DB -> Game -> IO GameWithNames
gameToGameWithNames db game = do
  whiteName <- case player_white game of
    Just pid -> do
      maybePlayer <- getPlayerById db pid
      return $ maybe T.empty playerName maybePlayer
    Nothing -> return T.empty

  blackName <- case player_black game of
    Just pid -> do
      maybePlayer <- getPlayerById db pid
      return $ maybe T.empty playerName maybePlayer
    Nothing -> return T.empty

  -- Extract just the moves from history (without timestamps)
  let moves = map fst (history game)

  return $
    GameWithNames
      { gameWhiteName = whiteName,
        gameBlackName = blackName,
        gameCards = cards game,
        gameHistory = moves,
        gameWinner = winner game,
        gameCreatedAt = createdAt game,
        gameLastActivity = lastActivity game
      }

-- Get a game with player names for client display
getGameWithNames :: DB -> GameId -> IO (Maybe GameWithNames)
getGameWithNames db gameId = do
  maybeGame <- getGameById db gameId
  case maybeGame of
    Nothing -> return Nothing
    Just game -> Just <$> gameToGameWithNames db game

-- Get all game summaries
getAllGameSummaries :: DB -> IO [GameSummary]
getAllGameSummaries db@(DB dataVar _ _ _ _ _) = do
  state <- readTVarIO dataVar
  let games = dbGames state
  reverse <$> mapM (uncurry $ gameIdAndGameToSummary db) (Map.toList games)
  where
    gameIdAndGameToSummary :: DB -> GameId -> Game -> IO GameSummary
    gameIdAndGameToSummary database gid game = do
      whiteName <- case player_white game of
        Just pid -> do
          maybePlayer <- getPlayerById database pid
          return $ maybe T.empty playerName maybePlayer
        Nothing -> return T.empty

      blackName <- case player_black game of
        Just pid -> do
          maybePlayer <- getPlayerById database pid
          return $ maybe T.empty playerName maybePlayer
        Nothing -> return T.empty

      return $ gameToSummary gid whiteName blackName game

-- Generate a new session token
generateToken :: IO SessionToken
generateToken = do
  SessionToken . toText <$> nextRandom

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

-- Get all player names (for dropdown in client)
getAllPlayerNames :: DB -> IO [Text]
getAllPlayerNames (DB dataVar _ _ _ _ _) = do
  state <- readTVarIO dataVar
  return $ map playerName $ Map.elems (dbPlayers state)

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

-- Join a game with player token
-- If token provided: MUST be valid (rejects invalid tokens)
-- If no token: name MUST be available (rejects duplicate names)
joinGameWithToken :: DB -> GameId -> Text -> Maybe SessionToken -> IO (Either JoinError JoinGameResponse)
joinGameWithToken db@(DB dataVar _ _ _ hasChangedVar _) gameId playerNameText maybeProvidedToken = do
  -- Validate name is not empty/whitespace
  let trimmedName = T.strip playerNameText
  if T.null trimmedName
    then return $ Left JEInvalidName
    else do
      maybeGame <- getGameById db gameId
      case maybeGame of
        Nothing -> return $ Left JEGameNotFound
        Just game@(Game maybeWhiteId maybeBlackId cards history winner _ _) -> do
          -- Determine which player is joining
          playerResult <- case maybeProvidedToken of
            Just token -> do
              -- Token provided: validate it
              maybePlayer <- getPlayerByToken db token
              case maybePlayer of
                Just p -> do
                  -- Valid token: use that player
                  putStrLn $ "Valid token for player: " ++ T.unpack (playerName p)
                  return $ Right p
                Nothing -> do
                  -- Invalid token: treat like no token, check if name available
                  putStrLn $ "Invalid token provided, treating as new player: " ++ T.unpack trimmedName
                  existing <- getPlayerByName db trimmedName
                  case existing of
                    Just _ -> do
                      putStrLn $ "Name already taken: " ++ T.unpack trimmedName
                      return $ Left JENameTaken
                    Nothing -> do
                      -- Name is available, create new player
                      newPlayer <- createPlayer db trimmedName
                      putStrLn $ "Created new player (invalid token): " ++ T.unpack trimmedName
                      return $ Right newPlayer
            Nothing -> do
              -- No token: name must be available
              putStrLn $ "No token provided, checking if name available: " ++ T.unpack trimmedName
              existing <- getPlayerByName db trimmedName
              case existing of
                Just _ -> do
                  putStrLn $ "Name already taken: " ++ T.unpack trimmedName
                  return $ Left JENameTaken
                Nothing -> do
                  -- Name is available, create new player
                  newPlayer <- createPlayer db trimmedName
                  putStrLn $ "Created new player: " ++ T.unpack trimmedName
                  return $ Right newPlayer

          case playerResult of
            Left err -> return $ Left err
            Right player -> do
              let pid = playerId player

              -- Check if player is already in this game
              if Just pid == maybeWhiteId || Just pid == maybeBlackId
                then do
                  putStrLn $ "Player " ++ T.unpack (playerName player) ++ " already in game"
                  gameWithNames <- gameToGameWithNames db game
                  return $ Right (JoinGameResponse {responseGame = gameWithNames, responseToken = playerToken player, responsePlayerName = playerName player})
                else do
                  -- Try to join an empty slot
                  now <- getCurrentTime
                  let updatedGame
                        | isNothing maybeWhiteId = Just $ Game {player_white = Just pid, player_black = maybeBlackId, cards = cards, history = history, winner = winner, createdAt = createdAt game, lastActivity = now}
                        | isNothing maybeBlackId = Just $ Game {player_white = maybeWhiteId, player_black = Just pid, cards = cards, history = history, winner = winner, createdAt = createdAt game, lastActivity = now}
                        | otherwise = Nothing -- Game is full
                  case updatedGame of
                    Nothing -> do
                      putStrLn "Game is full, cannot join"
                      return $ Left JEGameFull
                    Just newGame -> do
                      -- Update the game
                      atomically $ do
                        modifyTVar dataVar $ \state ->
                          state {dbGames = Map.insert gameId newGame (dbGames state)}
                        writeTVar hasChangedVar True
                      putStrLn $ "Player " ++ T.unpack (playerName player) ++ " joined game"
                      gameWithNames <- gameToGameWithNames db newGame
                      return $ Right (JoinGameResponse {responseGame = gameWithNames, responseToken = playerToken player, responsePlayerName = playerName player})

-- Get which slot a player occupies in a game (if any)
getPlayerSlotInGame :: Game -> PlayerId -> Maybe PlayerSlot
getPlayerSlotInGame game pid
  | player_white game == Just pid = Just PlayerWhite
  | player_black game == Just pid = Just PlayerBlack
  | otherwise = Nothing

-- Concede a game - the player with the given token admits defeat
concedeGame :: DB -> GameId -> SessionToken -> IO (Maybe Color)
concedeGame db@(DB dataVar _ _ _ hasChangedVar _) gameId token = do
  maybePlayer <- getPlayerByToken db token
  maybeGame <- getGameById db gameId

  case (maybePlayer, maybeGame) of
    (Just player, Just game) -> do
      let pid = playerId player
          maybeSlot = getPlayerSlotInGame game pid

      case maybeSlot of
        Nothing -> do
          putStrLn $ "Concede failed: player not in game " ++ show gameId
          return Nothing
        Just loserSlot -> do
          let winnerColor = case loserSlot of
                PlayerWhite -> Black
                PlayerBlack -> White

          -- Update the game with the winner
          success <- atomically $ do
            currentState <- readTVar dataVar
            case Map.lookup gameId (dbGames currentState) of
              Nothing -> return False
              Just (Game p1 p2 cards history _ created lastAct) -> do
                let updatedGame = Game {player_white = p1, player_black = p2, cards = cards, history = history, winner = Just winnerColor, createdAt = created, lastActivity = lastAct}
                writeTVar dataVar $
                  currentState {dbGames = Map.insert gameId updatedGame (dbGames currentState)}
                writeTVar hasChangedVar True
                return True

          if success
            then do
              putStrLn $ "Game " ++ show gameId ++ " ended: " ++ show winnerColor ++ " wins"
              return $ Just winnerColor
            else do
              putStrLn "Concede failed: game not found"
              return Nothing
    _ -> do
      putStrLn "Concede failed: invalid token or game not found"
      return Nothing