module Main where

import           Wrench.Engine
import           Wrench.Platform
import           Wrench.PlayMode
import ClassyPrelude

loopUntilFinished :: Platform p => p -> PlatformAudioSource p -> IO ()
loopUntilFinished p source = do
  stopped <- sourceIsStopped p source
  unless stopped (loopUntilFinished p source)

main :: IO ()
main =
  withPlatform (WindowTitle "window title") DynamicWindowSize $ \p -> do
    audioBuffer <- loadAudio p "media/ding.wav"
    source <- generateAudioSource p
    audioBufferToSource p audioBuffer source
    playSource p PlayModeOnce source 
    loopUntilFinished p source

