{-# LANGUAGE OverloadedStrings #-}
module WrenchTestTime(wrenchTestTime) where

import Test.HUnit(Test(..),assertFailure)
import Wrench.Time
import ClassyPrelude

wrenchTestTime :: Test
wrenchTestTime = TestList [wrenchTestPlusDuration]

assertNotEquals :: (Show a,Eq a) => a -> a -> IO ()
assertNotEquals a b = unless (a /= b) (assertFailure $ "expected something different from " <> (show a))

wrenchTestPlusDuration :: Test
wrenchTestPlusDuration = TestCase $ do
  before <- getTicks
  let after = before `plusDuration` fromMilliseconds 1250
  putStrLn ("before: " <> pack (show before))
  putStrLn ("after: " <> pack (show after))
  before `assertNotEquals` after
