{-# LANGUAGE RankNTypes      #-}
module Main where

import Control.Lens(from,(^.))
import Wrench.Engine(wrenchPlay,Picture(..),RenderPositionMode(..))
import Wrench.Angular
import Linear.V2
import Wrench.Time
import Wrench.Color
import Debug.Trace(traceShowId)

main :: IO ()
main = do
  wrenchPlay
    "window title"
    (V2 640 480)
    "media"
    colorsWhite
    0
    1
    (\td -> Translate (V2 100 100) $ Rotate (Degrees td ^. from degrees) $ Sprite "car" RenderPositionCenter)
    (\_ _ -> 0)
    (\td _ -> traceShowId (toSeconds td))
