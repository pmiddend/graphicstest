module Main where

import Wrench.Platform
import Wrench.Color
import Wrench.BitmapFont.Render
import Wrench.BitmapFont.RenderResult
import Wrench.Event
import Wrench.WindowSize
import Wrench.Engine
import Wrench.ImageData
import System.FilePath
import ClassyPrelude hiding ((</>))
import Control.Lens((^.))

mainLoop :: Platform p => p -> SurfaceMap (PlatformImage p) -> IO ()
mainLoop platform images = do
    events <- pollEvents platform
    if any isQuitEvent events
      then return ()
      else do
        let picture = (textToPicture images "djvu" 0 "Füße in Osnabrück") ^. bfrrPicture
        wrenchRender platform images undefined (Just colorsBlack) picture
        mainLoop platform images

main :: IO ()
main = withPlatform "bitmap font test" DynamicWindowSize $ \platform -> do
  (images, _) <- readMediaFiles (loadImage platform) ("media" </> "images")
  mainLoop platform images
