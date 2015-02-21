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
  renderBegin :: p -> IO ()
  renderClear :: p -> Color -> IO ()
  renderFinish :: p -> IO ()
  renderText :: p -> T.Text -> Color -> Point -> IO ()
  spriteDimensions :: p -> SpriteIdentifier -> IO Rectangle
  viewportSize :: p -> IO Point
  renderDrawSprite :: p -> SpriteIdentifier -> Rectangle -> Radians -> IO ()
