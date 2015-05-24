{-# LANGUAGE OverloadedStrings #-}
module WrenchTestImageParser(wrenchTestImageParser) where

import Test.HUnit(Test(..),(@?=))
import Wrench.ImageParser
import Wrench.Rectangle
import Linear.V2

wrenchTestImageParser :: Test
wrenchTestImageParser = TestCase (readImageDataFromText ("car=0,1,30,60") @?= [DataLineImage ("car",rectangleFromPoints (V2 0 1) (V2 30 60))])
