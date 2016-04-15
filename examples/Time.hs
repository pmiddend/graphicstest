module Main where

import Wrench.Time
import ClassyPrelude hiding (threadDelay)

main :: IO ()
main = do
  putStrLn "Waiting 2 seconds"
  threadDelay (fromSeconds 2)
  putStrLn "Done"
