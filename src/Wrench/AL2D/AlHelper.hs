{-# LANGUAGE CPP #-}
#ifndef USE_OPENAL
module Wrench.AL2D.AlHelper where
#else
module Wrench.AL2D.AlHelper(useDefaultDevice,withDevice,withContext,withDefaultAl,genObject,genBuffer,genSource,bufferDataFromAudioFile,bufferToSource,playSource,bufferFromFile,sourceIsStopped,freeSource,freeBuffer) where

import Sound.ALC.Device(alcOpenDevice,alcCloseDevice)
import Sound.ALC.Context(alcMakeContextCurrent,alcCreateContext,alcDestroyContext)
import Sound.ALC.Defines(alc_SYNC)
import Sound.ALC.Errors(alcGetError)
import Sound.ALC.Types(ALCdevice)
import qualified Data.Vector.Storable as SV
import Sound.AL.Errors(alGetError)
import Sound.AL.Source(alGenSources,alSourcei,alSourcePlay,alGetSourcei,alDeleteSources)
import Sound.AL.Buffer(alGenBuffers,alBufferData,alDeleteBuffers)
import Sound.AL.Defines(al_NO_ERROR,al_FORMAT_MONO8,al_FORMAT_MONO16,al_FORMAT_STEREO8,al_FORMAT_STEREO16,al_BUFFER,al_SOURCE_STATE,al_STOPPED,al_LOOPING)
import Sound.AL.Types(ALvoid,ALint,ALsizei,ALenum)
import Foreign.Ptr
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Marshal.Array(withArray)
import Foreign.Marshal.Utils(with)
import Wrench.PlayMode
import Prelude()
import ClassyPrelude
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable(peek)
import Control.Lens((^.))
import Wrench.AL2D.FFIHelper
import Wrench.AudioFile
import Wrench.AL2D.AlcContextFlag
import Wrench.AL2D.AlBuffer
import Wrench.AL2D.AlSource
import Wrench.AL2D.AlBufferFormat
import Data.Int(Int8,Int16)
import Data.Word(Word16)

useDefaultDevice :: Maybe a
useDefaultDevice = Nothing

type AlcDevicePtr = Ptr ALCdevice

withDevice :: Maybe Text -> (AlcDevicePtr -> IO a) -> IO a
withDevice deviceName action =
  let
    inner ptr =
      bracket (alcOpenDevice ptr) alcCloseDevice $ \device ->
            if device == nullPtr
                then do
                code <- alcGetError
                error $ "Error opening device: " <> show code
                else action device
  in
    case deviceName of
        Nothing ->
          inner nullPtr
        Just deviceName' ->
          withTextToCString deviceName' $ \deviceName'' ->
            inner (castPtr deviceName'')

contextFlagToImpl :: Num b => AlcContextFlag -> b
contextFlagToImpl AlcContextSync = fromIntegral alc_SYNC

type AlcContextPtr = Ptr ALvoid

withContext :: AlcDevicePtr -> [(AlcContextFlag,ALint)] -> (AlcContextPtr -> IO b) -> IO b
withContext device flags action = withArray (concatMap (\(f,v) -> [contextFlagToImpl f,v]) flags) $ \flags' -> bracket (alcCreateContext device flags') alcDestroyContext $ \context ->
  if context == nullPtr
     then do
       code <- alcGetError
       error $ "Error creating context: " <> show code
     else do
       currentError <- alcMakeContextCurrent context
       if currentError == 0
         then do
           code <- alcGetError
           error $ "Error making context current: " <> show code
         else action context

withDefaultAl :: (AlcDevicePtr -> AlcContextPtr -> IO a) -> IO a
withDefaultAl action = withDevice useDefaultDevice $ \device -> withContext device [] $ \context -> action device context

failIfAlError :: MonadIO m => m ()
failIfAlError = do
  e <- liftIO alGetError
  if e == al_NO_ERROR
    then return ()
    else error $ "al error: " <> show e

genObject :: (Storable a2, MonadIO m) => (Ptr a2 -> IO a1) -> (a2 -> a) -> m a
genObject f g = liftIO (alloca (\bufferPtr -> f bufferPtr *> failIfAlError *> (g <$> peek bufferPtr)))

genBuffer :: MonadIO m => m AlBuffer
genBuffer = genObject (alGenBuffers 1) AlBuffer

bufferFromFile :: (MonadIO m, Applicative m) => AudioFile -> m AlBuffer
bufferFromFile file = do
  buffer <- genBuffer
  bufferDataFromAudioFile buffer file
  return buffer

genSource :: MonadIO m => m AlSource
genSource = genObject (alGenSources 1) AlSource

freeSource :: MonadIO m => AlSource -> m ()
freeSource s = liftIO $ with (fromIntegral (alSourceImpl s)) (alDeleteSources 1)

freeBuffer :: MonadIO m => AlBuffer -> m ()
freeBuffer s = liftIO $ with (fromIntegral (alBufferImpl s)) (alDeleteBuffers 1)

sourceIsStopped :: MonadIO m => AlSource -> m Bool
sourceIsStopped s = liftIO $ alloca $ \attrptr -> do
  alGetSourcei (alSourceImpl s) al_SOURCE_STATE attrptr <* failIfAlError
  attr <- peek attrptr
  return (attr == al_STOPPED)

type AlBufferByteCount = ALsizei
type AlBufferFrequency = ALsizei

alBufferFormatImpl :: AlBufferFormat -> ALenum
alBufferFormatImpl AlBufferFormatMono8 = al_FORMAT_MONO8
alBufferFormatImpl AlBufferFormatMono16 = al_FORMAT_MONO16
alBufferFormatImpl AlBufferFormatStereo8 = al_FORMAT_STEREO8
alBufferFormatImpl AlBufferFormatStereo16 = al_FORMAT_STEREO16

makeFormat :: Int -> Int -> AlBufferFormat
makeFormat 2 8 = AlBufferFormatStereo8
makeFormat 1 8 = AlBufferFormatMono8
makeFormat 2 16 = AlBufferFormatStereo16
makeFormat 1 16 = AlBufferFormatMono16
makeFormat channels bits = error $ "invalid audio format, channels " <> show channels <> " and bits " <> show bits


int8ToWord8 :: Int8 -> Word8
int8ToWord8 = (fromIntegral :: Int16 -> Word8) . (+ 127) . (fromIntegral :: Int8 -> Int16)

word16ToInt16 :: Word16 -> Int16
word16ToInt16 = (fromIntegral :: Int32 -> Int16) . (subtract 32768) . (fromIntegral :: Word16 -> Int32)

bufferDataFromAudioFile :: (MonadIO m, Applicative m) => AlBuffer -> AudioFile -> m ()
bufferDataFromAudioFile buffer audioFile =
  let
    bufferDataHelper bits d = do
      let (fptr,len) = SV.unsafeToForeignPtr0 d
      liftIO $ withForeignPtr fptr $ \ptr -> do
        bufferData buffer (makeFormat (audioFile ^. afChannels) bits) ptr (fromIntegral len) (fromIntegral (audioFile ^. afFrequency))
  in 
    case audioFile ^. afData of
      AudioDataMonoSigned8 d -> bufferDataHelper 8 (int8ToWord8 `SV.map` d)
      AudioDataMonoUnsigned8 d -> bufferDataHelper 8 d
      AudioDataMonoUnsigned16 d -> bufferDataHelper 16 (word16ToInt16 `SV.map` d)
      AudioDataMonoSigned16 d -> bufferDataHelper 16 d

bufferData :: (Applicative m,MonadIO m) => AlBuffer -> AlBufferFormat -> Ptr a -> AlBufferByteCount -> AlBufferFrequency -> m ()
bufferData buffer format data_ byteCount frequency = liftIO (alBufferData (alBufferImpl buffer) (alBufferFormatImpl format) data_ byteCount frequency) <* failIfAlError

bufferToSource :: MonadIO m => AlBuffer -> AlSource -> m ()
bufferToSource buffer source = alSourcei (alSourceImpl source) al_BUFFER (fromIntegral (alBufferImpl buffer))

playSource :: MonadIO m => PlayMode -> AlSource -> m ()
playSource pm source = do
  alSourcei (alSourceImpl source) al_LOOPING (convertPlayMode pm)
  alSourcePlay (alSourceImpl source)
  where convertPlayMode PlayModeOnce = 0
        convertPlayMode PlayModeLooping = 1
#endif
