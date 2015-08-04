{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Wrench.AudioFile
Description : Contains an abstraction over raw audio data
Maintainer  : pmidden@secure.mailbox.org

The 'AudioFile' type represents a bridge between an audio loader and
an audio player. For example, you could use SDL2 to load wave files
and OpenAL to play them.
-}
module Wrench.AudioFile where

import           ClassyPrelude
import           Control.Lens         (makeLenses)
import           Data.Int             (Int16, Int8)
import qualified Data.Vector.Storable as SV
import           Data.Word            (Word16)

data AudioData = AudioDataMonoUnsigned8 (SV.Vector Word8)
               | AudioDataMonoSigned8 (SV.Vector Int8)
               | AudioDataMonoUnsigned16 (SV.Vector Word16)
               | AudioDataMonoSigned16 (SV.Vector Int16)

data AudioFile = AudioFile {
    _afChannels  :: Int  -- ^ Number of channels in the audio file
  , _afFrequency :: Int -- ^ Frequency in Hz
  , _afData      :: AudioData -- ^ Raw sample data
  }

$(makeLenses ''AudioFile)
