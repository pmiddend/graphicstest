{-# LANGUAGE OverloadedStrings #-}
module WrenchTestImageParser(wrenchTestImageParser) where

import Test.HUnit(Test(..),(@?=))
import Wrench.ImageParser
import Wrench.Rectangle
import Wrench.Animation
import Linear.V2

wrenchTestImageParser :: Test
wrenchTestImageParser = TestList [wrenchTestImageParserImage,wrenchTestImageParserAnims]

wrenchTestImageParserImage :: Test
wrenchTestImageParserImage = TestCase (readImageDataFromText ("car=0,1,30,60") @?= [DataLineImage ("car",rectangleFromPoints (V2 0 1) (V2 30 60))])

wrenchTestImageParserAnims :: Test
wrenchTestImageParserAnims = TestCase (readImageDataFromText ">player_walk=250|pwr0,pwr1" @?= [DataLineAnim ("player_walk",Animation 250 ["pwr0","pwr1"])])
