{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Lens   (from, (^.))
import           Debug.Trace    (traceShowId)
import           Linear.V2
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Engine  (Picture (..), RenderPositionMode (..),
                                 ViewportSize, wrenchPlay)
import           Wrench.Time

toPicture :: ViewportSize -> Double -> Picture
toPicture _ td = Translate (V2 100 100) $ Rotate (Degrees td ^. from degrees) $ Sprite "car" RenderPositionCenter

main :: IO ()
main = do
  wrenchPlay
    "window title"
    "media"
    (Just colorsWhite)
    0
    1
    toPicture
    (\_ _ -> 0)
    (\td _ -> traceShowId (toSeconds td))
