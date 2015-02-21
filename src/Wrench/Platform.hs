module Wrench.Platform where

import Wrench.Color
import Wrench.Event
import Wrench.Point
import Wrench.SpriteIdentifier
import Wrench.Rectangle
import Wrench.Angular
import qualified Data.Text as T
import ClassyPrelude 

type WindowTitle = T.Text
type BackgroundColor = Color

class Platform p where
  pollEvents :: p -> IO [Event]
  renderClear :: p -> IO ()
  renderFinish :: p -> IO ()
  renderText :: p -> T.Text -> Color -> Point -> IO ()
  renderSetDrawColor :: p -> Color -> IO ()
  spriteDimensions :: p -> SpriteIdentifier -> IO Rectangle
  viewportSize :: p -> IO Point
  renderDrawSprite :: p -> SpriteIdentifier -> Rectangle -> Rectangle -> Radians -> IO ()
