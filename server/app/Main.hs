{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import App (app)
import Network.Wai.Middleware.Cors
    ( cors,
      simpleCorsResourcePolicy,
      CorsResourcePolicy(corsMethods, corsRequestHeaders) )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- ^ Uncomment logStdoutDev in main function below for verbose request logging



policy :: CorsResourcePolicy
policy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["Authorization", "Content-Type"]
    }

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Starting Onitama server on port " ++ show port
    -- For verbose request logging, replace the line below with:
    -- run port . logStdoutDev . cors (const $ Just policy) =<< app
    run port . cors (const $ Just policy) =<< app
