{-# LANGUAGE TypeFamilies #-}
module Wrench.Platform where

import Wrench.Color
import Wrench.Event
import Wrench.Point
import Wrench.Rectangle
import Wrench.Angular
import qualified Data.Text as T
import ClassyPrelude

type WindowTitle = T.Text
type BackgroundColor = Color
type SrcRect = Rectangle
type DestRect = Rectangle

class Platform p where
  type PlatformImage p :: *
  loadImage :: p -> FilePath -> IO (PlatformImage p)
  pollEvents :: p -> IO [Event]
  renderBegin :: p -> IO ()
  renderClear :: p -> Color -> IO ()
  renderFinish :: p -> IO ()
  renderText :: p -> T.Text -> Color -> Point -> IO ()
  viewportSize :: p -> IO Point
  renderDrawSprite :: p -> (PlatformImage p) -> SrcRect -> DestRect -> Radians -> IO ()
