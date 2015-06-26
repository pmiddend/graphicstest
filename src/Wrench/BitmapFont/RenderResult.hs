{-# LANGUAGE TemplateHaskell #-}
module Wrench.BitmapFont.RenderResult where

import Control.Lens(makeLenses)
import Wrench.Picture
import Wrench.Point

data RenderResult = RenderResult {
    _bfrrPicture :: Picture
  , _bfrrSize :: Point
  }

$(makeLenses ''RenderResult)
