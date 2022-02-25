{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import App (app)
import Network.Wai.Middleware.Cors
    ( cors,
      simpleCorsResourcePolicy,
      CorsResourcePolicy(corsMethods, corsRequestHeaders) )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)



policy :: CorsResourcePolicy
policy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["Authorization", "Content-Type"]
    }

main :: IO ()
main = do
    let port = 3000
    run port . logStdoutDev . cors (const $ Just policy) =<< app
