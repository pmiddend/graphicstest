module Wrench.Animation where

import Wrench.ImageId
import ClassyPrelude

data Animation = Animation {
  animFrameSwitch :: Int,
  animFrames      :: [ImageId]
  } deriving(Eq,Show)
