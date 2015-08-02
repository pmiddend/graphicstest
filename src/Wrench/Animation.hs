{-|
Module      : Wrench.Animation
Description : Type for animations (sequences of image)
Maintainer  : pmidden@secure.mailbox.org
-}
{-# LANGUAGE TemplateHaskell #-}
module Wrench.Animation where

import           ClassyPrelude
import           Control.Lens           ((^.))
import           Control.Lens.Getter    (Getter, to)
import           Control.Lens.TH        (makeLenses)
import           Wrench.ImageIdentifier
import           Wrench.Time

-- | An animation in wrench is a sequence of images, displayed with constant frame rate
data Animation = Animation {
  _animFrameSwitch :: TimeDelta,                   -- ^ when to switch frames
  _animFrames      :: [ImageIdentifier]            -- ^ frame identifiers (frames can occur multiple times)
  } deriving(Eq,Show)

$(makeLenses ''Animation)

-- | Get the total runtime of an animation
animLifetime :: Getter Animation TimeDelta
animLifetime = to (\a -> ((a ^. animFrameSwitch) * (fromSeconds . fromIntegral . length $ a ^. animFrames)))
