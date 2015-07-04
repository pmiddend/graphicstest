{-# LANGUAGE TemplateHaskell #-}
module Wrench.RenderBlockMode where

import Wrench.RenderTargetFps
import Control.Lens(makePrisms)
import ClassyPrelude

data RenderBlockMode = RenderAndReturn
                     | RenderAndWait RenderTargetFps
                     deriving(Show,Eq)

$(makePrisms ''RenderBlockMode)
