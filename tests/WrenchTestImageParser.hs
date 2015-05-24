{-# LANGUAGE OverloadedStrings #-}
module WrenchTestImageParser(wrenchTestImageParser) where

import Test.HUnit(Test(..),(@?=))
import Wrench.ImageParser
import Wrench.Rectangle
import Wrench.Animation
import Linear.V2

wrenchTestImageParser :: Test
wrenchTestImageParser = TestList [wrenchTestImageParserImage,wrenchTestImageParserAnims,wrenchTestImageParserImageTrailingNewline]

wrenchTestImageParserImageTrailingNewline :: Test
wrenchTestImageParserImageTrailingNewline = TestCase (readImageDataFromText ("car=0,1,30,60\n") @?= [DataLineImage ("car",rectangleFromPoints (V2 0 1) (V2 30 60))])

wrenchTestImageParserImage :: Test
wrenchTestImageParserImage = TestCase (readImageDataFromText ("car=0,1,30,60\nfoo=1,2,3,4") @?= [DataLineImage ("car",rectangleFromPoints (V2 0 1) (V2 30 60)),DataLineImage ("foo",rectangleFromPoints (V2 1 2) (V2 3 4))])

wrenchTestImageParserAnims :: Test
wrenchTestImageParserAnims = TestCase (readImageDataFromText ">player_walk=250|pwr0,pwr1\n>player_stand=250|pwr0,pwr1" @?= [DataLineAnim ("player_walk",Animation 250 ["pwr0","pwr1"]),DataLineAnim ("player_stand",Animation 250 ["pwr0","pwr1"])])
