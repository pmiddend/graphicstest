{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Wrench.Platform where

import           ClassyPrelude
import qualified Data.Text        as T
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.Point
import           Wrench.Rectangle
import Control.Lens.TH

newtype WindowTitle = WindowTitle { unpackWindowTitle :: T.Text }
type SrcRect = Rectangle
type DestRect = Rectangle
type FontSize = Int

data SpriteInstance image = SpriteInstance {
    _spriteImage :: image
  , _spriteSrcRect :: Rectangle
  , _spriteDestRect :: Rectangle
  , _spriteRotation :: Radians
  }

$(makeLenses ''SpriteInstance)

class Platform p where
  type PlatformImage p :: *
  type PlatformFont p :: *
  loadImage :: p -> FilePath -> IO (PlatformImage p)
  freeImage :: p -> PlatformImage p -> IO ()
  loadFont :: p -> FilePath -> FontSize -> IO (PlatformFont p)
  freeFont :: p -> PlatformFont p -> IO ()
  pollEvents :: p -> IO [Event]
  renderBegin :: p -> IO ()
  renderClear :: p -> Color -> IO ()
  renderFinish :: p -> IO ()
  renderText :: p -> (PlatformFont p) -> T.Text -> Color -> Point -> IO ()
  viewportSize :: p -> IO Point
  renderDrawSprites :: p -> [SpriteInstance (PlatformImage p)] -> IO ()
