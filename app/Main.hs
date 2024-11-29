{-# LANGUAGE ImportQualifiedPost #-}

module Main where

--------------------------------------------------------------------------------

import API
import Network.Wai.Handler.Warp

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Launching service on port 8000"
  run 8000 $ eventServer
