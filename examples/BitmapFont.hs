module Main where

import Wrench.Platform
import Wrench.Color
import Wrench.BitmapFont.Render
import Wrench.BitmapFont.RenderResult
import Wrench.Event
import Wrench.Engine
import Wrench.ImageData
import System.FilePath
import ClassyPrelude hiding ((</>))
import Control.Lens((^.))

mainLoop :: Platform p => p -> SurfaceMap (PlatformImage p) -> IO ()
mainLoop platform images = do
    events <- pollEvents platform
    when (any isQuitEvent events) (putStrLn "Would quit")
    let picture = (textToPicture images "abfont" 10 "aabb") ^. bfrrPicture
    wrenchRender platform images undefined (Just colorsBlack) picture
    if any isQuitEvent events
      then return ()
      else mainLoop platform images

main :: IO ()
main = withPlatform "bitmap font test" DynamicWindowSize $ \platform -> do
  (images, _) <- readMediaFiles (loadImage platform) ("media" </> "images")
  mainLoop platform images
