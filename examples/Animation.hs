module Main where

import           ClassyPrelude        hiding ((</>))
import           Control.Lens         (ix, (^.), (^?!))
import           Linear.V2
import           System.FilePath
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Event
import           Wrench.ImageData
import           Wrench.MediaData
import           Wrench.MouseGrabMode
import           Wrench.Picture
import           Wrench.Platform
import           Wrench.Time
import           Wrench.WindowSize

vdiv :: (Functor f, Integral b) => f b -> b -> f b
a `vdiv` v = (`div` v) <$> a

mainLoop :: Platform p => p -> MediaData (PlatformImage p) -> Int -> IO ()
mainLoop platform mediaData currentFrameIndex = do
    events <- pollEvents platform
    unless (any isQuitEvent events) $ do
        viewport <- viewportSize platform
        let
          anim = mediaData ^?! mdAnims . ix "anim"
          animFramesTotal = length (anim ^. animFrames)
          nextFrameIndex = (currentFrameIndex + 1) `mod` animFramesTotal
          currentFrame = anim ^?! animFrames . ix currentFrameIndex
          framePicture = pictureSprite currentFrame (V2 65 47) :: Picture Int Float
          totalPicture = (viewport `vdiv` 2) `pictureTranslated` framePicture
        wrenchRender platform (mediaData ^. mdSurfaces) (error "fonts not loaded") (Just colorsBlack) totalPicture
        threadDelay (anim ^. animFrameSwitch)
        mainLoop platform mediaData nextFrameIndex

main :: IO ()
main = withPlatform "animation test" DynamicWindowSize MouseGrabNo $ \platform -> do
  mediaData <- readMediaFiles (loadImage platform) ("media" </> "images")
  mainLoop platform mediaData 0
