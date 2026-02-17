{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (appWithConfig)
import Control.Monad (when)
import Options (CleanupOptions (..), ServerOptions (..), parseOptions)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Data.String (fromString)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Exit (exitSuccess)

policy :: CorsResourcePolicy
policy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "POST"],
      corsRequestHeaders = ["X-Session-Token", "Content-Type"]
    }

main :: IO ()
main = do
  -- Parse command-line options
  opts <- parseOptions

  -- Handle --version flag
  when (optShowVersion opts) $ do
    putStrLn "onitama-server version 0.1.0.0"
    exitSuccess

  let port = optPort opts
      host = optHost opts
      cleanup = optCleanup opts

  -- Print startup message
  putStrLn ""
  putStrLn $ "Starting Onitama server on " ++ host ++ ":" ++ show port
  putStrLn $ "Database: " ++ maybe "in-memory (no persistence)" id (optDatabase opts)
  putStrLn $ "Save interval: " ++ show (optSaveInterval opts) ++ " minutes"

  if cleanupEnabled cleanup
    then do
      putStrLn "Cleanup enabled:"
      putStrLn $ "  - Waiting games: " ++ show (cleanupWaiting cleanup) ++ " hours"
      putStrLn $ "  - Active games: " ++ show (cleanupActive cleanup) ++ " hours"
      putStrLn $ "  - Completed games: " ++ show (cleanupCompleted cleanup) ++ " hours"
      putStrLn $ "  - Cleanup interval: " ++ show (cleanupInterval cleanup) ++ " minutes"
    else
      putStrLn "Cleanup disabled (--no-cleanup)"

  when (optVerbose opts) $ putStrLn "Verbose logging enabled"
  when (optResetDB opts) $ putStrLn "Starting with fresh database (--reset-db)"

  -- Build application with configuration
  application <- appWithConfig opts

  -- Apply middleware based on verbose flag
  let middleware = cors (const $ Just policy)
      verboseMiddleware = if optVerbose opts
                          then logStdoutDev . middleware
                          else middleware

  -- Configure Warp settings with host binding
  let settings = setPort port $ setHost (fromString host) defaultSettings

  -- Start server
  putStrLn ""
  putStrLn "Server started successfully!"
  runSettings settings $ verboseMiddleware application
