module Wrench.Platform where

import Wrench.Color

type WindowTitle = String
type BackgroundColor = Color

class Platform p where
  withPlatform :: WindowTitle -> Maybe BackgroundColor -> (p -> IO ()) -> IO ()
  loadImage :: 
