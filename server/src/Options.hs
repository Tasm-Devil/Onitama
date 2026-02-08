{-# LANGUAGE DeriveGeneric #-}

module Options
  ( ServerOptions (..),
    CleanupOptions (..),
    parseOptions,
    defaultCleanup,
    cleanupOptionsToConfig,
  )
where

import Data.Aeson (FromJSON)
import qualified Data.ByteString as BS
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Yaml as Yaml
import qualified Database
import GHC.Generics (Generic)
import Options.Applicative
import System.Directory (doesFileExist)

-- | Cleanup configuration options
data CleanupOptions = CleanupOptions
  { cleanupWaiting :: Double, -- Hours before cleaning waiting games
    cleanupActive :: Double, -- Hours before cleaning active games
    cleanupCompleted :: Double, -- Hours before cleaning completed games
    cleanupInterval :: Int, -- Minutes between cleanup checks
    cleanupEnabled :: Bool -- Whether cleanup is enabled
  }
  deriving (Show, Generic)

instance FromJSON CleanupOptions

-- | Command-line options for the server
data ServerOptions = ServerOptions
  { optVerbose :: Bool, -- Enable verbose HTTP logging
    optPort :: Int, -- Server port
    optHost :: String, -- Bind address
    optDatabase :: FilePath, -- Database file path
    optResetDB :: Bool, -- Start with fresh empty database
    optSaveInterval :: Int, -- Database save interval in minutes
    optCleanup :: CleanupOptions, -- Cleanup configuration
    optConfig :: Maybe FilePath, -- Config file path
    optShowVersion :: Bool -- Show version and exit
  }
  deriving (Show, Generic)

instance FromJSON ServerOptions

-- | Default cleanup options
defaultCleanup :: CleanupOptions
defaultCleanup =
  CleanupOptions
    { cleanupWaiting = 2.0, -- 2 hours
      cleanupActive = 24.0, -- 24 hours
      cleanupCompleted = 0, -- never (0 = disabled)
      cleanupInterval = 10, -- 10 minutes
      cleanupEnabled = True
    }

-- | Default server options
defaultOptions :: ServerOptions
defaultOptions =
  ServerOptions
    { optVerbose = False,
      optPort = 8080,
      optHost = "0.0.0.0",
      optDatabase = "gamedb.json",
      optResetDB = False,
      optSaveInterval = 1, -- 1 minute
      optCleanup = defaultCleanup,
      optConfig = Nothing,
      optShowVersion = False
    }

-- | Option parser for cleanup configuration
cleanupOptionsParser :: Parser CleanupOptions
cleanupOptionsParser =
  CleanupOptions
    <$> option
      auto
      ( long "cleanup-waiting"
          <> metavar "HOURS"
          <> value (cleanupWaiting defaultCleanup)
          <> showDefault
          <> help "Hours before cleaning waiting games"
      )
    <*> option
      auto
      ( long "cleanup-active"
          <> metavar "HOURS"
          <> value (cleanupActive defaultCleanup)
          <> showDefault
          <> help "Hours before cleaning active games"
      )
    <*> option
      auto
      ( long "cleanup-completed"
          <> metavar "HOURS"
          <> value (cleanupCompleted defaultCleanup)
          <> showDefault
          <> help "Hours before cleaning completed games (0 = never, keep forever)"
      )
    <*> option
      auto
      ( long "cleanup-interval"
          <> metavar "MINS"
          <> value (cleanupInterval defaultCleanup)
          <> showDefault
          <> help "Minutes between cleanup checks"
      )
    <*> ( not
            <$> switch
              ( long "no-cleanup"
                  <> help "Disable automatic game cleanup"
              )
        )

-- | Option parser for server configuration
serverOptionsParser :: Parser ServerOptions
serverOptionsParser =
  ServerOptions
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Enable verbose HTTP request logging"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value (optPort defaultOptions)
          <> showDefault
          <> help "Server port"
      )
    <*> strOption
      ( long "host"
          <> metavar "HOST"
          <> value (optHost defaultOptions)
          <> showDefault
          <> help "Bind address (0.0.0.0 for all, 127.0.0.1 for localhost only)"
      )
    <*> strOption
      ( long "database"
          <> short 'd'
          <> metavar "FILE"
          <> value (optDatabase defaultOptions)
          <> showDefault
          <> help "Database file path"
      )
    <*> switch
      ( long "reset-db"
          <> help "Start with fresh empty database (ignores existing file)"
      )
    <*> option
      auto
      ( long "save-interval"
          <> metavar "MINS"
          <> value (optSaveInterval defaultOptions)
          <> showDefault
          <> help "Database save interval in minutes"
      )
    <*> cleanupOptionsParser
    <*> optional
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "FILE"
              <> help "Load configuration from YAML file"
          )
      )
    <*> switch
      ( long "version"
          <> help "Show version information"
      )

-- | Load configuration from YAML file
loadConfigFile :: FilePath -> IO (Maybe ServerOptions)
loadConfigFile path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- BS.readFile path
      case Yaml.decodeEither' content of
        Left err -> do
          putStrLn $ "Warning: Failed to parse config file " ++ path ++ ": " ++ show err
          putStrLn "Using command-line options only."
          return Nothing
        Right opts -> do
          putStrLn $ "Loaded configuration from " ++ path
          return (Just opts)
    else return Nothing

-- | Merge options with precedence: defaults < config file < command-line
mergeOptions :: ServerOptions -> ServerOptions -> ServerOptions
mergeOptions config cli =
  ServerOptions
    { optVerbose = optVerbose cli || optVerbose config,
      optPort = if optPort cli == optPort defaultOptions then optPort config else optPort cli,
      optHost = if optHost cli == optHost defaultOptions then optHost config else optHost cli,
      optDatabase = if optDatabase cli == optDatabase defaultOptions then optDatabase config else optDatabase cli,
      optResetDB = optResetDB cli || optResetDB config,
      optSaveInterval = if optSaveInterval cli == optSaveInterval defaultOptions then optSaveInterval config else optSaveInterval cli,
      optCleanup = mergeCleanup (optCleanup config) (optCleanup cli),
      optConfig = optConfig cli,
      optShowVersion = optShowVersion cli || optShowVersion config
    }
  where
    mergeCleanup cfg cliClean =
      CleanupOptions
        { cleanupWaiting = if cleanupWaiting cliClean == cleanupWaiting defaultCleanup then cleanupWaiting cfg else cleanupWaiting cliClean,
          cleanupActive = if cleanupActive cliClean == cleanupActive defaultCleanup then cleanupActive cfg else cleanupActive cliClean,
          cleanupCompleted = if cleanupCompleted cliClean == cleanupCompleted defaultCleanup then cleanupCompleted cfg else cleanupCompleted cliClean,
          cleanupInterval = if cleanupInterval cliClean == cleanupInterval defaultCleanup then cleanupInterval cfg else cleanupInterval cliClean,
          cleanupEnabled = cleanupEnabled cliClean && cleanupEnabled cfg -- Both must be true
        }

-- | Parse command-line options with config file support
parseOptions :: IO ServerOptions
parseOptions = do
  -- Parse command-line arguments
  cliOpts <- execParser opts

  -- Check for version flag first
  if optShowVersion cliOpts
    then return cliOpts
    else do
      -- Only load config file if explicitly specified with --config
      case optConfig cliOpts of
        Nothing -> return cliOpts -- No config file, use CLI options only
        Just configPath -> do
          maybeConfig <- loadConfigFile configPath
          -- Merge with precedence: defaults < config < CLI
          case maybeConfig of
            Nothing -> return cliOpts
            Just fileConfig -> return $ mergeOptions fileConfig cliOpts
  where
    opts =
      info
        (serverOptionsParser <**> helper)
        ( fullDesc
            <> progDesc "Onitama multiplayer game server"
            <> header "onitama-server - A Haskell backend for the Onitama board game"
        )

-- | Convert CleanupOptions (hours as Double) to CleanupConfig (seconds as NominalDiffTime)
-- This bridges the gap between command-line/config options and the database layer
cleanupOptionsToConfig :: CleanupOptions -> Database.CleanupConfig
cleanupOptionsToConfig (CleanupOptions waitingHrs activeHrs completedHrs _ _) =
  Database.CleanupConfig
    { Database.cleanupWaitingForPlayers = realToFrac (waitingHrs * 3600),
      Database.cleanupInProgress = realToFrac (activeHrs * 3600),
      Database.cleanupCompleted = realToFrac (completedHrs * 3600)
    }
