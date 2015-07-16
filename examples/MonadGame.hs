module Main where

import           Wrench.Color
import           Wrench.MonadGame
import           Wrench.RenderBlockMode
import           Wrench.Engine
import           Wrench.Picture
import           Wrench.WindowSize
import           Wrench.Event
import qualified Wrench.Keysym as Key
import ClassyPrelude
import Control.Lens(has,filtered)

spaceKeyPressed :: Traversable t => t Event -> Bool
--spaceKeyPressed events = has (events ^.. traverse . _Keyboard . keySym . filtered (== Space))
spaceKeyPressed events = has (traverse . _Keyboard . keySym . filtered (== Key.Space)) events

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
      
