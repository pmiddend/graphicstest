module Wrench.Platform where

import Wrench.Color
import Wrench.Event
import Wrench.Point
import Wrench.SpriteIdentifier
import Wrench.Rectangle
import Wrench.Angular

type WindowTitle = String
type BackgroundColor = Color

class Platform p where
  withPlatform :: WindowTitle -> Maybe BackgroundColor -> (p -> IO ()) -> IO ()
  pollEvents :: p -> IO [Event]
  renderClear :: p -> BackgroundColor -> IO ()
  renderFinish :: p -> IO ()
  renderText :: p -> String -> Color -> Point -> IO ()
  renderSetDrawColor :: p -> Color -> IO ()
  renderDrawSprite :: p -> SpriteIdentifier -> Rectangle -> Rectangle -> Radians -> Point -> IO ()
