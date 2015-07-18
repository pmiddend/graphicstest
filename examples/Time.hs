module Main where

import Wrench.Time
import ClassyPrelude

main :: IO ()
main = do
  putStrLn "Waiting 2 seconds"
  threadDelay (fromSeconds 2)
  putStrLn "Done"
