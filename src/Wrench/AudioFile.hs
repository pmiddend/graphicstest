{-# LANGUAGE TemplateHaskell #-}
module Wrench.AudioFile where

import ClassyPrelude
import Control.Lens(makeLenses)
import qualified Data.Vector.Storable as SV
import Data.Int(Int8,Int16)
import Data.Word(Word16)

data AudioData = AudioDataMonoUnsigned8 (SV.Vector Word8)
               | AudioDataMonoSigned8 (SV.Vector Int8)
               | AudioDataMonoUnsigned16 (SV.Vector Word16)
               | AudioDataMonoSigned16 (SV.Vector Int16)

data AudioFile = AudioFile {
    _afChannels :: Int
  , _afFrequency :: Int
  , _afData :: AudioData
  }

$(makeLenses ''AudioFile)
