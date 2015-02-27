{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens   (from, (^.))
import           Linear.V2
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Engine  (Picture (..), RenderPositionMode (..),
                                 ViewportSize, wrenchPlay,withPlatform)
import           Wrench.Time
import ClassyPrelude

toPicture :: ViewportSize -> Double -> Picture
toPicture _ td = Translate (V2 100 100) $ Rotate (Degrees td ^. from degrees) $ Sprite "car" RenderPositionCenter

main :: IO ()
main = do
  withPlatform "window title" "media" $ \p -> do
    wrenchPlay
        p
        (Just colorsWhite)
        0
        1
        toPicture
        (\_ _ -> 0)
        (\td _ -> traceShowId (toSeconds td))
