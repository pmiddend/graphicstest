{-# LANGUAGE TemplateHaskell #-}
module Wrench.Backends.Sdl.Sdl2AudioFile where

import Graphics.UI.SDL.Types(AudioSpec(..))
import ClassyPrelude
import Foreign.ForeignPtr(ForeignPtr)
import Control.Lens(makeLenses)

data Sdl2AudioFile = Sdl2AudioFile {
    _sdl2afAudioSpec :: AudioSpec
  , _sdl2afData :: ForeignPtr Word8
  , _sdl2afLength :: Word32
  }

$(makeLenses ''Sdl2AudioFile)
