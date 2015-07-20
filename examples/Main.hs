module Main where

import           ClassyPrelude
import           Linear.V2
import           Linear.Vector        ((^+^))
import           Wrench.Color
import           Wrench.Declarative
import           Wrench.Engine
import           Wrench.Event
import           Wrench.KeyMovement
import qualified Wrench.Keysym        as Key
import           Wrench.MouseGrabMode
import           Wrench.Picture
import           Wrench.Platform
import           Wrench.Point
import           Wrench.ViewportSize
import           Wrench.WindowSize

data World = World {
    carPosition :: Point
  }

toPicture :: ImageSizeGetter -> ViewportSize -> World -> Picture
toPicture _ _ world = (carPosition world) `pictureTranslated` (pictureSpriteCentered "car")

initialWorld :: World
initialWorld = World (V2 100 100)

eventHandler :: Event -> World -> World
eventHandler event world = case event of
  Keyboard (KeyboardEvent{_keyMovement=KeyDown, _keySym=Key.Left}) -> World (carPosition world ^+^ V2 (-10) 0)
  Keyboard (KeyboardEvent{_keyMovement=KeyDown, _keySym=Key.Right}) -> World (carPosition world ^+^ V2 10 0)
  Keyboard (KeyboardEvent{_keyMovement=KeyDown, _keySym=Key.Up}) -> World (carPosition world ^+^ V2 0 (-10))
  Keyboard (KeyboardEvent{_keyMovement=KeyDown, _keySym=Key.Down}) -> World (carPosition world ^+^ V2 0 10)
  MouseAxis (MouseAxisEvent{_mouseAxisDelta=delta}) -> World (carPosition world + delta)
  _ -> world


main :: IO ()
main =
  withPlatform (WindowTitle "window title") DynamicWindowSize MouseGrabYes $ \p ->
    wrenchPlay
        p
        (MediaPath "media/images")
        (BackgroundColor (Just colorsWhite))
        initialWorld
        (StepsPerSecond 1)
        (ToPictureHandler toPicture)
        (EventHandler eventHandler)
