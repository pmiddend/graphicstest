{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Wrench.SDL2AudioLoader(sdl2LoadWave) where

import Graphics.UI.SDL.Audio(loadWAV)
import Graphics.UI.SDL.Types(AudioSpec(..),AudioFormat)
import Control.Lens(makeLenses,(^.))
import Foreign.Ptr
import           ClassyPrelude             hiding (lookup,FilePath,(</>),Vector)
import System.ByteOrder
import qualified Data.Vector.Storable as SV
import Wrench.AudioFile
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable(peek)
import Foreign.ForeignPtr(newForeignPtr_,ForeignPtr,castForeignPtr)
import Foreign.C.String(withCString)
import Data.Int(Int8,Int16)
import Data.Bits(rotateL,Bits)
import Data.Word(Word16)
import System.FilePath

data Sdl2AudioFile = Sdl2AudioFile {
    _sdl2afAudioSpec :: AudioSpec
  , _sdl2afData :: ForeignPtr Word8
  , _sdl2afLength :: Word32
  }

$(makeLenses ''Sdl2AudioFile)

sdl_AUDIO_U8 :: AudioFormat
sdl_AUDIO_U8 = 0x0008

sdl_AUDIO_S8 :: AudioFormat
sdl_AUDIO_S8 = 0x8008

sdl_AUDIO_S16LSB :: AudioFormat
sdl_AUDIO_S16LSB = 0x8010

sdl_AUDIO_S16MSB :: AudioFormat
sdl_AUDIO_S16MSB = 0x9010

sdl_AUDIO_U16LSB :: AudioFormat
sdl_AUDIO_U16LSB = 0x0010

sdl_AUDIO_U16MSB :: AudioFormat
sdl_AUDIO_U16MSB = 0x1010

sdl2AudioFileToGeneral :: Sdl2AudioFile -> AudioFile
sdl2AudioFileToGeneral sdl2wr = AudioFile {
    _afChannels = fromIntegral (audioSpecChannels (sdl2wr ^. sdl2afAudioSpec))
  , _afFrequency = fromIntegral (audioSpecFreq (sdl2wr ^. sdl2afAudioSpec))
  , _afData = convertData (audioSpecFormat (sdl2wr ^. sdl2afAudioSpec)) (sdl2wr ^. sdl2afData) (sdl2wr ^. sdl2afLength)
  }

swapByteOrder :: Bits a => a -> a
swapByteOrder w = rotateL w 8

maybeSwapVector :: (Storable a,Bits a) => ByteOrder -> SV.Vector a -> SV.Vector a
maybeSwapVector bo v = if byteOrder == bo then v else swapByteOrder `SV.map` v

convertData :: AudioFormat -> ForeignPtr Word8 -> Word32 -> AudioData
convertData af v n
  | af == sdl_AUDIO_U8 = AudioDataMonoUnsigned8 (toVector v)
  | af == sdl_AUDIO_S8 = AudioDataMonoSigned8 (toVector (castForeignPtr v :: ForeignPtr Int8))
  | af == sdl_AUDIO_S16LSB = AudioDataMonoSigned16 $ maybeSwapVector LittleEndian (toVector (castForeignPtr v :: ForeignPtr Int16))
  | af == sdl_AUDIO_S16MSB = AudioDataMonoSigned16 $ maybeSwapVector BigEndian (toVector (castForeignPtr v :: ForeignPtr Int16))
  | af == sdl_AUDIO_U16LSB = AudioDataMonoUnsigned16 $ maybeSwapVector LittleEndian (toVector (castForeignPtr v :: ForeignPtr Word16))
  | af == sdl_AUDIO_U16MSB = AudioDataMonoUnsigned16 $ maybeSwapVector BigEndian (toVector (castForeignPtr v :: ForeignPtr Word16))
  | otherwise = error $ "Invalid audio format " <> show af
  where toVector ptr = SV.unsafeFromForeignPtr0 ptr (fromIntegral n)

sdl2LoadWave :: MonadIO m => FilePath -> m (Maybe AudioFile)
sdl2LoadWave fileName = do
  res <- sdl2LoadWaveRaw fileName
  return (sdl2AudioFileToGeneral <$> res)

sdl2LoadWaveRaw :: MonadIO m => FilePath -> m (Maybe Sdl2AudioFile)
sdl2LoadWaveRaw fileName = liftIO $ withCString fileName $ \fileName' -> alloca $ \dataPtr -> alloca $ \specPtr -> alloca $ \lenPtr -> do
  retPtr <- loadWAV fileName' specPtr dataPtr lenPtr
  if retPtr == nullPtr
    then return Nothing
    else do
      spec <- peek specPtr
      data' <- peek dataPtr
      len <- peek lenPtr
      foreignData <- newForeignPtr_ data'
      return $ Just $ Sdl2AudioFile {
                    _sdl2afAudioSpec = spec
                  , _sdl2afData = foreignData
                  , _sdl2afLength = len
              }
