module Main where

import           ClassyPrelude
import           Wrench.Engine
import           Wrench.MouseGrabMode
import           Wrench.Platform
import           Wrench.PlayMode
import           Wrench.WindowSize

loopUntilFinished :: Platform p => p -> PlatformAudioSource p -> IO ()
loopUntilFinished p source = do
  stopped <- sourceIsStopped p source
  unless stopped (loopUntilFinished p source)

main :: IO ()
main =
  withPlatform (WindowTitle "window title") DynamicWindowSize MouseGrabNo $ \p -> do
    audioBuffer <- loadAudio p "media/ding.wav"
    source <- playBuffer p audioBuffer PlayModeOnce
    loopUntilFinished p source

