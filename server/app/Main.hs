{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (appWithConfig)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Internal (Request (requestHeaders))
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options (CleanupOptions (..), ServerOptions (..), parseOptions)
import System.Exit (exitSuccess)

policy :: CorsResourcePolicy
policy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "POST"],
      corsRequestHeaders = ["Content-Type"]
    }

-- | Dev mode middleware: injects X-Forwarded-User and X-Forwarded-Preferred-Username headers when missing
devAuthMiddleware :: String -> Middleware
devAuthMiddleware defaultUser app req respond =
  let headers = requestHeaders req
      hasUser = any (\(k, _) -> k == "X-Forwarded-User") headers
      updatedHeaders =
        if hasUser
          then headers
          else
            let userBS = fromString defaultUser
             in ("X-Forwarded-User", userBS) : ("X-Forwarded-Preferred-Username", userBS) : headers
      updatedReq = req {requestHeaders = updatedHeaders}
   in app updatedReq respond

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
      devMode = optDevMode opts
      devUser = fromMaybe "dev" (optDevUser opts)

  -- Print startup message
  putStrLn ""
  putStrLn $ "Starting Onitama server on " ++ host ++ ":" ++ show port
  putStrLn $ "Database: " ++ fromMaybe "in-memory (no persistence)" (optDatabase opts)
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
  when devMode $ putStrLn $ "Dev mode enabled (default user: " ++ devUser ++ ")"

  -- Build application with configuration
  application <- appWithConfig opts

  -- Apply middleware based on flags
  let corsMiddleware = cors (const $ Just policy)
      devMiddleware =
        if devMode
          then devAuthMiddleware devUser
          else id
      verboseMiddleware =
        if optVerbose opts
          then logStdoutDev
          else id

  -- Configure Warp settings with host binding
  let settings = setPort port $ setHost (fromString host) defaultSettings

  -- Start server
  putStrLn ""
  putStrLn "Server started successfully!"
  runSettings settings $ verboseMiddleware . corsMiddleware . devMiddleware $ application
