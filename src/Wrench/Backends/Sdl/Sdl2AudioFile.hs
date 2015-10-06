{-# LANGUAGE TemplateHaskell #-}
module Wrench.Backends.Sdl.Sdl2AudioFile where

import           ClassyPrelude
import           Control.Lens          (makeLenses)
import           Foreign.ForeignPtr    (ForeignPtr)
import           SDL.Raw.Types(AudioSpec (..))

data Sdl2AudioFile = Sdl2AudioFile {
    _sdl2afAudioSpec :: AudioSpec
  , _sdl2afData      :: ForeignPtr Word8
  , _sdl2afLength    :: Word32
  }

$(makeLenses ''Sdl2AudioFile)
