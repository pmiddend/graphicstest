module Main where

import           Control.Lens   (from, (^.))
import           Linear.V2
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Time
import           Wrench.Platform
import ClassyPrelude

toPicture :: ViewportSize -> Double -> Picture
toPicture _ td = Translate (V2 100 100) $ Rotate (Degrees td ^. from degrees) $ Sprite "car" RenderPositionCenter

main :: IO ()
main = do
  withPlatform (WindowTitle "window title") $ \p -> do
    wrenchPlay
        p
        (MediaPath "media")
        (BackgroundColor (Just colorsWhite))
        0
        (StepsPerSecond 1)
        (ToPictureHandler toPicture)
        (EventHandler (\_ _ -> 0))
        (TickHandler (\td _ -> traceShowId (toSeconds td)))
