module Main where

import OptBeer.App
import Network.Wai.Handler.Warp

main :: IO ()
main = useOptBeerApplication (const $ error "no") $ \app -> do
  putStrLn "running on port 4000..."
  run 4000 app
