{-# LANGUAGE TemplateHaskell #-}
module Wrench.Color(
    Color
  , mkColorFromRgba
  , colorsWhite
  , colorsBlack
  , colorRed
  , colorGreen
  , colorBlue
  , colorAlpha
  , colorsRed
  ) where

import           ClassyPrelude
import           Control.Lens.TH (makeLenses)

data Color = Color { _colorRed   :: Word8
                   , _colorGreen :: Word8
                   , _colorBlue  :: Word8
                   , _colorAlpha :: Word8
                   }
                   deriving(Show,Eq)

$(makeLenses ''Color)

mkColorFromRgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color
mkColorFromRgba = Color

colorsWhite :: Color
colorsWhite = mkColorFromRgba 255 255 255 255

colorsBlack :: Color
colorsBlack = mkColorFromRgba 0 0 0 255

colorsRed :: Color
colorsRed = mkColorFromRgba 255 0 0 255
