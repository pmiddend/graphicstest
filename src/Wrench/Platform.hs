{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wrench.Platform where

import           ClassyPrelude hiding(FilePath,(</>))
import qualified Data.Text        as T
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.Point
import           Wrench.PlayMode
import           Wrench.Rectangle
import Control.Lens.TH
import System.FilePath

newtype WindowTitle = WindowTitle { unpackWindowTitle :: T.Text } deriving(IsString)
data WindowSize = DynamicWindowSize | ConstantWindowSize Int Int
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

data TextInstance font = TextInstance {
    _textFont :: font
  , _textContent :: T.Text
  , _textColor :: Color
  , _textPosition :: Point
  }

$(makeLenses ''TextInstance)

class Platform p where
  type PlatformImage p :: *
  type PlatformFont p :: *
  type PlatformAudioBuffer p :: *
  type PlatformAudioSource p :: *
  loadAudio :: p -> FilePath -> IO (PlatformAudioBuffer p)
  generateAudioSource :: p -> IO (PlatformAudioSource p)
  audioBufferToSource :: p -> PlatformAudioBuffer p -> PlatformAudioSource p -> IO ()
  playSource :: p -> PlayMode -> PlatformAudioSource p -> IO ()
  sourceIsStopped :: p -> PlatformAudioSource p -> IO Bool
  loadImage :: p -> FilePath -> IO (PlatformImage p)
  freeImage :: p -> PlatformImage p -> IO ()
  loadFont :: p -> FilePath -> FontSize -> IO (PlatformFont p)
  freeFont :: p -> PlatformFont p -> IO ()
  pollEvents :: p -> IO [Event]
  renderBegin :: p -> IO ()
  renderClear :: p -> Color -> IO ()
  renderFinish :: p -> IO ()
  renderText :: p -> [TextInstance (PlatformFont p)] -> IO ()
  viewportSize :: p -> IO Point
  renderSprites :: p -> [SpriteInstance (PlatformImage p)] -> IO ()
