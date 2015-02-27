{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Wrench.SDLPlatform where

import           ClassyPrelude             hiding (lookup)
import           Control.Lens              ((^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Iso          (Iso', from, iso)
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Loops       (unfoldM)
import qualified Data.Text                 as T
import qualified Graphics.UI.SDL.Color     as SDLC
import qualified Graphics.UI.SDL.Events    as SDLE
import           Graphics.UI.SDL.Image     as SDLImage
import qualified Graphics.UI.SDL.Keycode   as SDLKeycode
import qualified Graphics.UI.SDL.Keysym    as SDLKeysym
import qualified Graphics.UI.SDL.Rect      as SDLRect
import qualified Graphics.UI.SDL.Render    as SDLR
import qualified Graphics.UI.SDL.TTF       as SDLTtf
import           Graphics.UI.SDL.TTF.Types (TTFFont)
import qualified Graphics.UI.SDL.Types     as SDLT
import qualified Graphics.UI.SDL.Video     as SDLV
import           Linear.V2                 (V2 (..), _x, _y)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.KeyMovement
import qualified Wrench.Keysym             as Keysym
import           Wrench.Platform
import           Wrench.Point
import           Wrench.Rectangle

floored :: (RealFrac a,Integral b) => Getter a b
floored = to floor

data SDLPlatform = SDLPlatform {
    _sdlpRenderer :: SDLT.Renderer
  , _sdlpWindow   :: SDLT.Window
  }

$(makeLenses ''SDLPlatform)

toSdlRect :: Rectangle -> SDLRect.Rect
toSdlRect r = SDLRect.Rect
              (r ^. rectLeftTop ^. _x ^. floored)
              (r ^. rectLeftTop ^. _y ^. floored)
              (r ^. rectangleDimensions ^. _x ^. floored)
              (r ^. rectangleDimensions ^. _y ^. floored)

fromSdlRect :: SDLRect.Rect -> Rectangle
fromSdlRect (SDLRect.Rect x y w h) = rectangleFromPoints (V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral (x + w)) (fromIntegral (y + h)))

fromSdlColor :: SDLC.Color -> Color
fromSdlColor (SDLC.Color r g b a) = mkColorFromRgba r g b a

toSdlColor :: Color -> SDLC.Color
toSdlColor c = SDLC.Color (c ^. colorRed) (c ^. colorGreen) (c ^. colorBlue) (c ^. colorAlpha)

wrenchRect :: Iso' SDLRect.Rect Rectangle
wrenchRect = iso fromSdlRect toSdlRect

wrenchColor :: Iso' SDLC.Color Color
wrenchColor = iso fromSdlColor toSdlColor


fromSdlEvent :: Getter SDLE.Event (Maybe Event)
fromSdlEvent = to fromSdlEvent'
  where fromSdlEvent' (SDLE.Event _ edata) = fromSdlEvent'' edata
        fromSdlEvent'' (SDLE.Keyboard movement _ r sym) = Just $ Keyboard (fromSdlKeyMovement movement) r (fromSdlSym sym)
        fromSdlEvent'' (SDLE.Quit) = Just Quit
        fromSdlEvent'' _ = Nothing
        fromSdlKeyMovement SDLE.KeyUp = KeyUp
        fromSdlKeyMovement SDLE.KeyDown = KeyDown
        fromSdlSym (SDLKeysym.Keysym _ keycode _) = fromSdlKeycode keycode
        fromSdlKeycode SDLKeycode.Up = Keysym.Up
        fromSdlKeycode SDLKeycode.Down = Keysym.Down
        fromSdlKeycode SDLKeycode.Left = Keysym.Left
        fromSdlKeycode SDLKeycode.Right = Keysym.Right
        fromSdlKeycode _ = error "Unknown keycode"

setRenderDrawColor :: SDLT.Renderer -> Color -> IO ()
setRenderDrawColor renderer c = do
  _ <- SDLR.setRenderDrawColor renderer (c ^. colorRed) (c ^. colorGreen) (c ^. colorBlue) (c ^. colorAlpha)
  return ()

renderClear' :: SDLT.Renderer -> IO ()
renderClear' renderer = do
  _ <- SDLR.renderClear renderer
  return ()

renderFinish' :: SDLT.Renderer -> IO ()
renderFinish' = SDLR.renderPresent


withWindow :: T.Text -> (SDLT.Window -> IO a) -> IO a
withWindow title callback = SDLV.withWindow (T.unpack title) (SDLT.Position SDLV.windowPosUndefined SDLV.windowPosUndefined) (SDLT.Size (fromIntegral screenAbsoluteWidth) (fromIntegral screenAbsoluteHeight)) windowFlags $ \win -> callback win
  where screenAbsoluteWidth,screenAbsoluteHeight :: Int
        screenAbsoluteWidth = 0
        screenAbsoluteHeight = 0
        windowFlags :: [SDLT.WindowFlag]
        windowFlags = [SDLT.WindowResizable]

withRenderer :: SDLT.Window -> (SDLT.Renderer -> IO a) -> IO a
withRenderer window callback =
  let acquireResource = SDLR.createRenderer window SDLT.FirstSupported []
      releaseResource = SDLR.destroyRenderer
  in bracket acquireResource releaseResource callback

sizeToPoint :: SDLT.Size -> Point
sizeToPoint (SDLT.Size w h) = V2 (fromIntegral w) (fromIntegral h)

withSDLPlatform :: WindowTitle -> (SDLPlatform -> IO ()) -> IO ()
withSDLPlatform windowTitle cb =
  withFontInit $
    withImgInit $
      withWindow windowTitle $ \window -> do
        withRenderer window $ \renderer -> do
          cb (SDLPlatform renderer window)

instance Platform SDLPlatform where
  type PlatformImage SDLPlatform = SDLT.Texture
  type PlatformFont SDLPlatform = TTFFont
  pollEvents _ = mapMaybe (^. fromSdlEvent) <$> unfoldM SDLE.pollEvent
  loadFont _ fp fs = SDLTtf.openFont (fpToString fp) fs
  renderBegin _ = return ()
  renderClear p c = setRenderDrawColor (p ^. sdlpRenderer) c >> renderClear' (p ^. sdlpRenderer)
  renderFinish p = renderFinish' (p ^. sdlpRenderer)
  loadImage p fp = SDLImage.loadTexture (p ^. sdlpRenderer) (fpToString fp)
  renderText p font s c pos = do
    texture <- createFontTexture (p ^. sdlpRenderer) font s c
    (width, height) <- SDLTtf.sizeText font (T.unpack s)
    SDLR.renderCopy (p ^. sdlpRenderer) texture Nothing (Just $ SDLRect.Rect (pos ^. _x ^. floored) (pos ^. _y ^. floored) width height)
    destroyTexture texture
  viewportSize p = sizeToPoint <$> SDLV.getWindowSize (p ^. sdlpWindow)
  renderDrawSprite p texture srcRect destRect rads = do
    let rot = rads ^. degrees
        rotCenter = Nothing
        flipFlags = []
    SDLR.renderCopyEx
      (p ^. sdlpRenderer)
      texture
      (Just $ srcRect ^. from wrenchRect)
      (Just $ destRect ^. from wrenchRect)
      (rot ^. getDegrees)
      rotCenter
      flipFlags

destroyTexture :: SDLT.Texture -> IO ()
destroyTexture t = SDLR.destroyTexture t

withFontInit :: IO a -> IO a
withFontInit = bracket_ SDLTtf.init SDLTtf.quit

withImgInit :: IO a -> IO a
withImgInit = bracket_ (SDLImage.init [SDLImage.initPng]) SDLImage.quit

createFontTexture :: SDLT.Renderer -> TTFFont -> T.Text -> Color -> IO SDLT.Texture
createFontTexture renderer font text color = do
  surface <- SDLTtf.renderUTF8Blended font (T.unpack text) (color ^. from wrenchColor)
  SDLR.createTextureFromSurface renderer surface
