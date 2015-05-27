{-# LANGUAGE TemplateHaskell #-}
module Wrench.Animation where

import Wrench.ImageId
import ClassyPrelude
import Wrench.Time
import Control.Lens.TH(makeLenses)
import Control.Lens((^.))
import Control.Lens.Getter(Getter,to)

data Animation = Animation {
  _animFrameSwitch :: Int,
  _animFrames      :: [ImageId]
  } deriving(Eq,Show)

$(makeLenses ''Animation)

animLifetime :: Getter Animation TimeDelta
animLifetime = to (\a -> fromMilliseconds (fromIntegral ((a ^. animFrameSwitch) * length (a ^. animFrames))))
