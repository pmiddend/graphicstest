{-|
Module      : Wrench.BitmapFont.RenderResult
Description : Result of a bitmap font rendering operation
Maintainer  : pmidden@secure.mailbox.org
-}
{-# LANGUAGE TemplateHaskell #-}
module Wrench.BitmapFont.RenderResult where

import           Control.Lens   (makeLenses)
import           Linear.V2
import           Wrench.Picture

data RenderResult unit float = RenderResult {
    _bfrrPicture :: Picture unit float -- ^ Resulting picture containing all characters
  , _bfrrSize    :: V2 unit -- ^ Bounding box size of the picture, can be used for alignment
  }

$(makeLenses ''RenderResult)
