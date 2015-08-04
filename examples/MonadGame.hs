module Main where

import           ClassyPrelude
import           Control.Lens           (filtered, has, view)
import           Wrench.Color
import           Wrench.Event
import qualified Wrench.Keysym          as Key
import           Wrench.MonadGame
import           Wrench.MouseGrabMode
import           Wrench.Picture
import           Wrench.Rectangle       (rectDimensions)
import           Wrench.RenderBlockMode
import           Wrench.WindowSize

spaceKeyPressed :: Traversable t => t Event -> Bool
--spaceKeyPressed events = has (events ^.. traverse . _Keyboard . keySym . filtered (== Space))
spaceKeyPressed events = has (traverse . _Keyboard . keySym . filtered (== Key.Space)) events

mainLoop :: MonadGameBackend ()
mainLoop = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  unless (spaceKeyPressed events) $ do
    carImageSize <- (view rectDimensions) <$> glookupImageRectangleUnsafe "car"
    grender (pictureSprite "car" carImageSize :: Picture Int Float)
    mainLoop


main :: IO ()
main =
  runGame "media" "wrench example" DynamicWindowSize MouseGrabNo (Just colorsBlack) (RenderAndWait 30) mainLoop

