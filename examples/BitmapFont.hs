module Main where

import           ClassyPrelude                  hiding ((</>))
import           Control.Lens                   ((^.))
import           System.FilePath
import           Wrench.BitmapFont.Render
import           Wrench.BitmapFont.RenderResult
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Event
import           Wrench.ImageData
import           Wrench.MediaData
import           Wrench.MouseGrabMode
import           Wrench.Platform
import           Wrench.WindowSize

mainLoop :: Platform p => p -> SurfaceMap (PlatformImage p) -> IO ()
mainLoop platform images = do
    events <- pollEvents platform
    if any isQuitEvent events
      then return ()
      else do
        let picture = (textToPicture images "djvu" 0 "Füße in Osnabrück") ^. bfrrPicture
        wrenchRender platform images (error "fonts not loaded") (Just colorsBlack) picture
        mainLoop platform images

main :: IO ()
main = withPlatform "bitmap font test" DynamicWindowSize MouseGrabNo $ \platform -> do
  mediaData <- readMediaFiles (loadImage platform) ("media" </> "images")
  mainLoop platform (mediaData ^. mdSurfaces)
