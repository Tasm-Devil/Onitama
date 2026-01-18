{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (app)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

-- ^ Uncomment logStdoutDev in main function below for verbose request logging

policy :: CorsResourcePolicy
policy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "POST"],
      corsRequestHeaders = ["X-Session-Token", "Content-Type"]
    }

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Starting Onitama server on port " ++ show port
  -- For verbose request logging, replace the line below with:
  -- run port . logStdoutDev . cors (const $ Just policy) =<< app
  run port . cors (const $ Just policy) =<< app
