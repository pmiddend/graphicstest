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
  ) where

import           Control.Lens.TH           (makeLenses)
import Data.Word(Word8)

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


