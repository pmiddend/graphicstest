module Main where

import           ClassyPrelude
import           Control.Lens           (filtered, has)
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Event
import qualified Wrench.Keysym          as Key
import           Wrench.MonadGame
import           Wrench.Picture
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
    grender (pictureSpriteTopLeft "car")
    mainLoop


main :: IO ()
main =
  runGame "media" "wrench example" DynamicWindowSize (Just colorsBlack) (RenderAndWait 30) mainLoop

