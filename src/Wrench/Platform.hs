{-# LANGUAGE TypeFamilies #-}
module Wrench.Platform where

import           ClassyPrelude
import qualified Data.Text        as T
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.Point
import           Wrench.Rectangle

newtype WindowTitle = WindowTitle { unpackWindowTitle :: T.Text }
type SrcRect = Rectangle
type DestRect = Rectangle
type FontSize = Int

class Platform p where
  type PlatformImage p :: *
  type PlatformFont p :: *
  loadImage :: p -> FilePath -> IO (PlatformImage p)
  loadFont :: p -> FilePath -> FontSize -> IO (PlatformFont p)
  pollEvents :: p -> IO [Event]
  renderBegin :: p -> IO ()
  renderClear :: p -> Color -> IO ()
  renderFinish :: p -> IO ()
  renderText :: p -> (PlatformFont p) -> T.Text -> Color -> Point -> IO ()
  viewportSize :: p -> IO Point
  renderDrawSprite :: p -> (PlatformImage p) -> SrcRect -> DestRect -> Radians -> IO ()
