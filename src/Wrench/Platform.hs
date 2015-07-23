{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Wrench.Platform where

import           ClassyPrelude    hiding (FilePath, (</>))
import           Control.Lens.TH
import qualified Data.Text        as T
import           Linear.V2        (V2)
import           System.FilePath
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.PlayMode
import           Wrench.Point
import           Wrench.Rectangle

newtype WindowTitle = WindowTitle { unpackWindowTitle :: T.Text } deriving(IsString)
type SrcRect = Rectangle
type DestRect = Rectangle
type FontSize = Int

data SpriteInstance image a b = SpriteInstance {
    _spriteImage    :: image
  , _spriteSrcRect  :: Rectangle a
  , _spriteDestRect :: Rectangle a
  , _spriteRotation :: Radians b
  }

$(makeLenses ''SpriteInstance)

data TextInstance font a = TextInstance {
    _textFont     :: font
  , _textContent  :: T.Text
  , _textColor    :: Color
  , _textPosition :: V2 a
  }

$(makeLenses ''TextInstance)

class Platform p where
  type PlatformImage p :: *
  type PlatformFont p :: *
  type PlatformAudioBuffer p :: *
  type PlatformAudioSource p :: *
  loadAudio :: p -> FilePath -> IO (PlatformAudioBuffer p)
  playBuffer :: p -> PlatformAudioBuffer p -> PlayMode -> IO (PlatformAudioSource p)
  freeBuffer :: p -> PlatformAudioBuffer p -> IO ()
  freeSource :: p -> PlatformAudioSource p -> IO ()
  sourceIsStopped :: p -> PlatformAudioSource p -> IO Bool
  loadImage :: p -> FilePath -> IO (PlatformImage p)
  freeImage :: p -> PlatformImage p -> IO ()
  loadFont :: p -> FilePath -> FontSize -> IO (PlatformFont p)
  freeFont :: p -> PlatformFont p -> IO ()
  pollEvents :: p -> IO [Event]
  renderBegin :: p -> IO ()
  renderClear :: p -> Color -> IO ()
  renderFinish :: p -> IO ()
  renderText :: p -> [TextInstance (PlatformFont p) a] -> IO ()
  viewportSize :: p -> IO Point
  renderSprites :: (RealFloat b,Num a,Integral a) => p -> [SpriteInstance (PlatformImage p) a b] -> IO ()
