{-|
Module      : Wrench.RenderBlockMode
Description : Data type to specify the target frame rate
Maintainer  : pmidden@secure.mailbox.org
-}
{-# LANGUAGE TemplateHaskell #-}
module Wrench.RenderBlockMode where

import Wrench.RenderTargetFps
import Control.Lens(makePrisms)
import ClassyPrelude

data RenderBlockMode = RenderAndReturn -- ^ Render and return immediately
                     | RenderAndWait RenderTargetFps -- ^ Render and suspend to reach the number of frames specified
                     deriving(Show,Eq)

$(makePrisms ''RenderBlockMode)
