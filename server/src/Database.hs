{-# LANGUAGE DeriveGeneric #-}

module Database where

import Api (GameId (..), GameStatus (..), GameSummary (..), Games, JoinGameResponse (..), JoinError (..), SessionToken (..), gameToSummary, GameWithNames (..))
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
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Game (Color (..), Game (..), PlayerId, PlayerSlot (..), getCurrentPlayerSlot, give5Cards)
import System.Directory (doesFileExist)

-- Player type: stores player identity and token
data Player = Player
  { playerId    :: PlayerId
  , playerName  :: Text
  , playerToken :: SessionToken
  }
  deriving (Eq, Show, Generic)

instance FromJSON Player
instance ToJSON Player

data DBState = DBState
  { dbGames :: Games,
    dbNextGameId :: Int,
    dbPlayers :: Map PlayerId Player,
    dbNextPlayerId :: Int,
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
            return $ DBState { dbGames = empty, dbNextGameId = 1, dbPlayers = empty, dbNextPlayerId = 1, dbHasChanged = False }
      else do
        putStrLn $ "Database file at " ++ dbFilePath ++ " not found, starting with empty DB"
        return $ DBState { dbGames = empty, dbNextGameId = 1, dbPlayers = empty, dbNextPlayerId = 1, dbHasChanged = False }
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
    let newId = GameId (dbNextGameId state)
    modifyTVar dbVar $ \s ->
      s
        { dbGames = Map.insert newId (Game { player_white = Nothing, player_black = Nothing, cards = newCards, history = [], winner = Nothing }) (dbGames s),
          dbNextGameId = dbNextGameId s + 1,
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

  return $ GameWithNames
    { gameWhiteName = whiteName
    , gameBlackName = blackName
    , gameCards = cards game
    , gameHistory = history game
    , gameWinner = winner game
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
getAllGameSummaries db@(DB dbVar) = do
  state <- readTVarIO dbVar
  let games = dbGames state
  mapM (uncurry $ gameIdAndGameToSummary db) (Map.toList games)
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
getPlayerByToken (DB dbVar) token = do
  state <- readTVarIO dbVar
  return $ find (\p -> playerToken p == token) (Map.elems (dbPlayers state))

-- Find player by name
getPlayerByName :: DB -> Text -> IO (Maybe Player)
getPlayerByName (DB dbVar) name = do
  state <- readTVarIO dbVar
  return $ find (\p -> playerName p == name) (Map.elems (dbPlayers state))

-- Get player by ID
getPlayerById :: DB -> PlayerId -> IO (Maybe Player)
getPlayerById (DB dbVar) pid = do
  state <- readTVarIO dbVar
  return $ Map.lookup pid (dbPlayers state)

-- Get all player names (for dropdown in client)
getAllPlayerNames :: DB -> IO [Text]
getAllPlayerNames (DB dbVar) = do
  state <- readTVarIO dbVar
  return $ map playerName $ Map.elems (dbPlayers state)

-- Create a new player with unique ID and token
createPlayer :: DB -> Text -> IO Player
createPlayer (DB dbVar) name = do
  token <- generateToken
  atomically $ do
    state <- readTVar dbVar
    let pid = dbNextPlayerId state
        newPlayer = Player pid name token
    modifyTVar dbVar $ \s ->
      s
        { dbPlayers = Map.insert pid newPlayer (dbPlayers s),
          dbNextPlayerId = pid + 1,
          dbHasChanged = True
        }
    return newPlayer

-- Validate that a token belongs to a player and return the player
validatePlayerToken :: DB -> SessionToken -> IO (Maybe Player)
validatePlayerToken = getPlayerByToken

-- Join a game with player token
-- If token provided: MUST be valid (rejects invalid tokens)
-- If no token: name MUST be available (rejects duplicate names)
joinGameWithToken :: DB -> GameId -> Text -> Maybe SessionToken -> IO (Either JoinError JoinGameResponse)
joinGameWithToken db@(DB dbVar) gameId playerNameText maybeProvidedToken = do
  -- Validate name is not empty/whitespace
  let trimmedName = T.strip playerNameText
  if T.null trimmedName
    then return $ Left JEInvalidName
    else do
      maybeGame <- getGameById db gameId
      case maybeGame of
        Nothing -> return $ Left JEGameNotFound
        Just game@(Game maybeWhiteId maybeBlackId cards history winner) -> do
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
                  let updatedGame
                        | maybeWhiteId == Nothing = Just $ Game { player_white = Just pid, player_black = maybeBlackId, cards = cards, history = history, winner = winner }
                        | maybeBlackId == Nothing = Just $ Game { player_white = maybeWhiteId, player_black = Just pid, cards = cards, history = history, winner = winner }
                        | otherwise = Nothing -- Game is full

                  case updatedGame of
                    Nothing -> do
                      putStrLn "Game is full, cannot join"
                      return $ Left JEGameFull
                    Just newGame -> do
                      -- Update the game
                      atomically $ modifyTVar dbVar $ \state ->
                        state
                          { dbGames = Map.insert gameId newGame (dbGames state),
                            dbHasChanged = True
                          }
                      putStrLn $ "Player " ++ T.unpack (playerName player) ++ " joined game"
                      gameWithNames <- gameToGameWithNames db newGame
                      return $ Right (JoinGameResponse {responseGame = gameWithNames, responseToken = playerToken player, responsePlayerName = playerName player})

-- Validate that a token is valid for making a move in a game
-- Returns True if the player owns the current turn
validateTokenForMove :: DB -> GameId -> SessionToken -> IO Bool
validateTokenForMove db gameId token = do
  maybePlayer <- getPlayerByToken db token
  maybeGame <- getGameById db gameId
  case (maybePlayer, maybeGame) of
    (Just player, Just game) -> do
      let currentSlot = getCurrentPlayerSlot game
          pid = playerId player
      return $ case currentSlot of
        PlayerWhite -> player_white game == Just pid
        PlayerBlack -> player_black game == Just pid
    _ -> return False

-- Get which slot a player occupies in a game (if any)
getPlayerSlotInGame :: Game -> PlayerId -> Maybe PlayerSlot
getPlayerSlotInGame game pid
  | player_white game == Just pid = Just PlayerWhite
  | player_black game == Just pid = Just PlayerBlack
  | otherwise = Nothing

-- Concede a game - the player with the given token admits defeat
concedeGame :: DB -> GameId -> SessionToken -> IO (Maybe Color)
concedeGame db@(DB dbVar) gameId token = do
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
              putStrLn $ "Game " ++ show gameId ++ " ended: " ++ show winnerColor ++ " wins"
              return $ Just winnerColor
            else do
              putStrLn $ "Concede failed: game not found"
              return Nothing
    _ -> do
      putStrLn "Concede failed: invalid token or game not found"
      return Nothing