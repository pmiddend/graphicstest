{-# LANGUAGE TemplateHaskell #-}
module Wrench.BitmapFont.RenderResult where

import           Control.Lens   (makeLenses)
import           Linear.V2
import           Wrench.Picture

data RenderResult unit float = RenderResult {
    _bfrrPicture :: Picture unit float
  , _bfrrSize    :: V2 unit
  }

$(makeLenses ''RenderResult)
