module Wrench.Animation where

import Wrench.ImageId
import ClassyPrelude
import Wrench.Time

data Animation = Animation {
  animFrameSwitch :: Int,
  animFrames      :: [ImageId]
  } deriving(Eq,Show)

animLifetime :: Animation -> TimeDelta
animLifetime a = fromMilliseconds (fromIntegral (animFrameSwitch a * length (animFrames a)))
