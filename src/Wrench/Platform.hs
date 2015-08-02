{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Wrench.Platform where

import           ClassyPrelude    hiding (FilePath, (</>))
import           Control.Lens.TH
import qualified Data.Text        as T
import           Linear.V2
import           System.FilePath
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.PlayMode
import           Wrench.Rectangle

newtype WindowTitle = WindowTitle { unpackWindowTitle :: T.Text } deriving(IsString)
type SrcRect = Rectangle
type DestRect = Rectangle
type FontSize = Int

data SpriteInstance unit float image = SpriteInstance {
    _spriteImage    :: image
  , _spriteSrcRect  :: Rectangle unit
  , _spriteDestRect :: Rectangle unit
  , _spriteRotation :: Radians float
  , _spriteColor    :: Color
  }

$(makeLenses ''SpriteInstance)

mapSpriteInstanceUnit :: (a -> b) -> SpriteInstance a float image -> SpriteInstance b float image
mapSpriteInstanceUnit f s@(SpriteInstance{_spriteSrcRect=srcRect,_spriteDestRect=dstRect}) = s{_spriteSrcRect=f <$> srcRect,_spriteDestRect=f <$> dstRect}

data TextInstance unit font = TextInstance {
    _textFont     :: font
  , _textContent  :: T.Text
  , _textColor    :: Color
  , _textPosition :: V2 unit
  }

mapTextInstanceUnit :: (a -> b) -> TextInstance a font -> TextInstance b font
mapTextInstanceUnit f (TextInstance font content color position) = TextInstance font content color (f <$> position)

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
  renderText :: Integral unit => p -> [TextInstance unit (PlatformFont p)] -> IO ()
  viewportSize :: p -> IO (V2 Int)
  renderSprites :: (Integral int,Floating real,Real real) => p -> [SpriteInstance int real (PlatformImage p)] -> IO ()
