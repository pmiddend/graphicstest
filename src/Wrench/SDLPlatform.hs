{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.SDLPlatform where

import Wrench.Platform
import qualified Graphics.UI.SDL.Color     as SDLC
import qualified Graphics.UI.SDL.Events    as SDLE
import           Graphics.UI.SDL.Image     as SDLImage
import qualified Graphics.UI.SDL.Keycode   as SDLKeycode
import qualified Graphics.UI.SDL.Keysym    as SDLKeysym
import qualified Graphics.UI.SDL.Rect      as SDLRect
import qualified Graphics.UI.SDL.Render    as SDLR
import qualified Graphics.UI.SDL.TTF       as SDLTtf
import Wrench.Color
import Wrench.Event
import qualified Wrench.Keycode            as Wrenchkeycode
import           Control.Exception         (bracket, bracket_)
import Data.Monoid
import           Graphics.UI.SDL.TTF.Types (TTFFont)
import qualified Graphics.UI.SDL.Types     as SDLT
import qualified Graphics.UI.SDL.Video     as SDLV
import Wrench.ImageData
import Wrench.Rectangle
import           Control.Lens              ((&), (^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Iso          (Iso', from, iso)
import           Control.Lens.Setter       ((%~), (+~), (.~))
import           Control.Lens.TH           (makeLenses)
import           Linear.V2                 (V2 (..), _x, _y)

floored :: (RealFrac a,Integral b) => Getter a b
floored = to floor

data SDLPlatform = SDLPlatform {
    _sdlpRenderer        :: SDLT.Renderer
  , _sdlpSurfaceMap      :: (SurfaceMap SDLT.Texture,AnimMap)
  }

toSdlRect :: Rectangle -> SDLRect.Rect
toSdlRect r = SDLRect.Rect (r ^. rectLeftTop ^. _x ^. floored) (r ^. rectLeftTop ^. _y ^. floored) (r ^. rectangleDimensions ^. _x ^. floored) (r ^. rectangleDimensions ^. _y ^. floored)

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
        fromSdlSym (SDLKeysym.Keysym _ keycode modifiers) = Keysym (fromSdlKeycode keycode) modifiers
        fromSdlKeycode SDLKeycode.Up = Wrenchkeycode.Up
        fromSdlKeycode SDLKeycode.Down = Wrenchkeycode.Down
        fromSdlKeycode SDLKeycode.Left = Wrenchkeycode.Left
        fromSdlKeycode SDLKeycode.Right = Wrenchkeycode.Right

setRenderDrawColor :: SDLT.Renderer -> Color -> IO ()
setRenderDrawColor renderer c = do
  _ <- SDLR.setRenderDrawColor renderer (c ^. colorRed) (c ^. colorGreen) (c ^. colorBlue) (c ^. colorAlpha)
  return ()

renderClear :: SDLT.Renderer -> IO ()
renderClear renderer = do
  _ <- SDLR.renderClear renderer
  return ()

renderFinish' :: SDLT.Renderer -> IO ()
renderFinish' = SDLR.renderPresent


withWindow :: String -> (SDLT.Window -> IO a) -> IO a
withWindow title callback = SDLV.withWindow title (SDLT.Position SDLV.windowPosUndefined SDLV.windowPosUndefined) (SDLT.Size (fromIntegral screenAbsoluteWidth) (fromIntegral screenAbsoluteHeight)) windowFlags $ \win -> callback win
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

instance Platform SDLPlatform where
  withPlatform windowTitle mediaPath callback = withFontInit $
    withImgInit $
      withWindow windowTitle $ \window -> do
        withRenderer window $ \renderer -> do
          stdfont <- SDLTtf.openFont (mediaPath <> "/stdfont.ttf") 15
          surfaceData <- readMediaFiles (SDLT.loadTexture renderer) mediaPath
          callback (SDLPlatform renderer surfaceData)
  pollEvents p = mapMaybe (^. fromSdlEvent) <$> unfoldM SDLE.pollEvent
  renderClear p = renderClear' (p ^. sdlpRenderer)
  renderFinish p = renderFinish' (p ^. sdlpRenderer)
  renderText p s c pos = do
    texture <- createFontTexture renderer font text color
    (width, height) <- liftIO $ SDLTtf.sizeText font (unpack text)
    liftIO $ SDLR.renderCopy renderer texture Nothing (Just $ SDLRect.Rect (position ^. _x ^. floored) (position ^. _y ^. floored) width height)
  renderSetDrawColor p c = setRenderDrawColor (p ^. sdlpRenderer) c
  renderDrawSprite p identifier srcRect dstRect rads rot = do
    let surfaces = p ^. sdlpSurfaceMap
        renderer = p ^. sdlpRenderer
    case pack identifier `lookup` surfaces of
      Nothing -> error $ "Couldn't find image \"" <> identifier <> "\""
      Just (texture,rectangle) -> do
        let rot = rads ^. degrees
            rotCenter = Nothing
            flipFlags = []
        liftIO $ SDLR.renderCopyEx renderer texture (Just $ srcRect ^. from wrenchRect) (Just $ destRect ^. from wrenchRect) (rot ^. getDegrees) (rot ^. getDegrees) rotCenter flipFlags

destroyTexture :: MonadIO m => SDLT.Texture -> m ()
destroyTexture t = liftIO $ SDLR.destroyTexture t

withFontInit :: IO a -> IO a
withFontInit = bracket_ SDLTtf.init SDLTtf.quit

withImgInit :: IO a -> IO a
withImgInit = bracket_ (SDLImage.init [SDLImage.initPng]) SDLImage.quit

createFontTexture :: MonadIO m => SDLT.Renderer -> TTFFont -> Text -> Color -> m SDLT.Texture
createFontTexture renderer font text color = do
  surface <- liftIO $ SDLTtf.renderUTF8Blended font (unpack text) (color ^. from wrenchColor)
  liftIO $ SDLR.createTextureFromSurface renderer surface
