{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Wrench.SDLOldPlatform where

import           ClassyPrelude             hiding (lookup,FilePath,(</>),Vector)
import System.FilePath
import           Control.Lens              ((^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Prism       (_Just)
import Control.Lens.Review(re)
import           Control.Lens.Iso          (Iso', from, iso)
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Ptr(castPtr)
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Loops       (unfoldM)
import qualified Data.Text                 as T
import qualified Graphics.UI.SDL.Types     as SDLT
import qualified Graphics.UI.SDL.Enum     as SDLEnum
import qualified Graphics.UI.SDL.Video     as SDLV
import qualified Graphics.UI.SDL.Event     as SDLE
import           Linear.V2                 (V2 (..), _x, _y)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.KeyMovement
import qualified Wrench.Keysym             as Keysym
import           Wrench.Platform
import           Wrench.Point
import           Wrench.Rectangle
import Foreign.C.String(withCStringLen)
import Foreign.Ptr(nullPtr,Ptr)
import Foreign.Storable(peek)
import Foreign.C.Types(CDouble(..))
import Data.Vector.Storable(unsafeToForeignPtr0,Vector)
import Foreign.Marshal.Alloc(alloca)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Marshal.Utils (with,new)
import Codec.Picture(readImage,DynamicImage(..),Image(..))


floored :: (RealFrac a,Integral b) => Getter a b
floored = to floor

data SDLPlatform = SDLPlatform {
    _sdlpRenderer :: SDLT.Renderer
  , _sdlpWindow   :: SDLT.Window
  }

$(makeLenses ''SDLPlatform)

toSdlRect :: Rectangle -> SDLT.Rect
toSdlRect r = SDLT.Rect
              (r ^. rectLeftTop ^. _x ^. floored)
              (r ^. rectLeftTop ^. _y ^. floored)
              (r ^. rectangleDimensions ^. _x ^. floored)
              (r ^. rectangleDimensions ^. _y ^. floored)

fromSdlRect :: SDLT.Rect -> Rectangle
fromSdlRect (SDLT.Rect x y w h) = rectangleFromPoints (V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral (x + w)) (fromIntegral (y + h)))

fromSdlColor :: SDLT.Color -> Color
fromSdlColor (SDLT.Color r g b a) = mkColorFromRgba r g b a

toSdlColor :: Color -> SDLT.Color
toSdlColor c = SDLT.Color (c ^. colorRed) (c ^. colorGreen) (c ^. colorBlue) (c ^. colorAlpha)

wrenchRect :: Iso' SDLT.Rect Rectangle
wrenchRect = iso fromSdlRect toSdlRect

wrenchColor :: Iso' SDLT.Color Color
wrenchColor = iso fromSdlColor toSdlColor


fromSdlEvent :: Getter SDLT.Event (Maybe Event)
fromSdlEvent = to fromSdlEvent'
  where fromSdlEvent' (SDLT.KeyboardEvent _ _ _ state r sym) = Just $ Keyboard (fromSdlKeyState state) (r /= 0) (fromSdlSym sym)
        fromSdlEvent' (SDLT.QuitEvent{}) = Just Quit
        fromSdlEvent' _ = Nothing
        fromSdlKeyState k | k == SDLEnum.SDL_RELEASED = KeyUp
                          | k == SDLEnum.SDL_PRESSED = KeyDown
        fromSdlSym (SDLT.Keysym _ keycode _) = fromSdlKeycode keycode
        fromSdlKeycode k | k == SDLEnum.SDLK_UP = Keysym.Up
                         | k == SDLEnum.SDLK_DOWN = Keysym.Down
                         | k == SDLEnum.SDLK_LEFT = Keysym.Left
                         | k == SDLEnum.SDLK_RIGHT = Keysym.Right
        fromSdlKeycode _ = error "Unknown keycode"

setRenderDrawColor :: SDLT.Renderer -> Color -> IO ()
setRenderDrawColor renderer c = do
  _ <- SDLV.setRenderDrawColor renderer (c ^. colorRed) (c ^. colorGreen) (c ^. colorBlue) (c ^. colorAlpha)
  return ()

renderClear' :: SDLT.Renderer -> IO ()
renderClear' renderer = do
  _ <- SDLV.renderClear renderer
  return ()

renderFinish' :: SDLT.Renderer -> IO ()
renderFinish' = SDLV.renderPresent


withWindow :: T.Text -> (SDLT.Window -> IO a) -> IO a
withWindow title' callback = withCStringLen (unpack title') $ \title ->
  let
    acquireResource = SDLV.createWindow (fst title) SDLEnum.SDL_WINDOWPOS_UNDEFINED SDLEnum.SDL_WINDOWPOS_UNDEFINED (fromIntegral screenAbsoluteWidth) (fromIntegral screenAbsoluteHeight) windowFlags
    screenAbsoluteWidth,screenAbsoluteHeight :: Int
    screenAbsoluteWidth = 0
    screenAbsoluteHeight = 0
    windowFlags = SDLEnum.SDL_WINDOW_RESIZABLE
    releaseResource = SDLV.destroyWindow
  in
    bracket acquireResource releaseResource callback

withRenderer :: SDLT.Window -> (SDLT.Renderer -> IO a) -> IO a
withRenderer window callback =
  let acquireResource = SDLV.createRenderer window (-1) 0
      releaseResource = SDLV.destroyRenderer
  in bracket acquireResource releaseResource callback

{-
sizeToPoint :: SDLT.Size -> Point
sizeToPoint (SDLT.Size w h) = V2 (fromIntegral w) (fromIntegral h)
-}

withSDLPlatform :: WindowTitle -> WindowSize -> (SDLPlatform -> IO ()) -> IO ()
withSDLPlatform windowTitle windowSize cb =
  withFontInit $
    withImgInit $
      withWindow (unpackWindowTitle windowTitle) $ \window -> do
        withRenderer window $ \renderer -> do
          case windowSize of
            DynamicWindowSize -> return ()
            ConstantWindowSize w h -> do
              _ <- SDLV.renderSetLogicalSize renderer (fromIntegral w) (fromIntegral h)
              return ()
          cb (SDLPlatform renderer window)

eventArrayStaticSize :: Int
eventArrayStaticSize = 128

sdlPollEvents :: IO [SDLT.Event]
sdlPollEvents = allocaArray eventArrayStaticSize $ \eventArray -> do
  SDLE.pumpEvents
  events <- SDLE.peepEvents
    eventArray
    (fromIntegral eventArrayStaticSize)
    SDLEnum.SDL_GETEVENT
    SDLEnum.SDL_FIRSTEVENT
    SDLEnum.SDL_LASTEVENT
  peekArray (fromEnum events) $ eventArray

imageRgba8Ptr = fst . unsafeToForeignPtr0 . imageData
imageRgb8Ptr = fst . unsafeToForeignPtr0 . imageData

imageToSurface :: DynamicImage -> IO (Ptr SDLT.Surface)
imageToSurface (ImageRGBA8 im) = do
  let
    bitsPerPixel = 32
    bytesPerPixel = bitsPerPixel `div` 8
  withForeignPtr (imageRgba8Ptr im) $ \imptr -> SDLV.createRGBSurfaceFrom
    (castPtr imptr)
    (fromIntegral (imageWidth im))
    (fromIntegral (imageHeight im))
    bitsPerPixel
    (fromIntegral (imageWidth im * (fromIntegral bytesPerPixel)))
    0x000000FF
    0x0000FF00
    0x00FF0000
    0xFF000000
imageToSurface (ImageRGB8 im) = do
  let
    bitsPerPixel = 24
    bytesPerPixel = bitsPerPixel `div` 8
  withForeignPtr (imageRgb8Ptr im) $ \imptr -> SDLV.createRGBSurfaceFrom
    (castPtr imptr)
    (fromIntegral (imageWidth im))
    (fromIntegral (imageHeight im))
    bitsPerPixel
    (fromIntegral (imageWidth im * (fromIntegral bytesPerPixel)))
    0x000000FF
    0x0000FF00
    0x00FF0000
    0xFF000000
imageToSurface _ = error "unsupported image format"


instance Platform SDLPlatform where
  type PlatformImage SDLPlatform = SDLT.Texture
  type PlatformFont SDLPlatform = Int
  pollEvents _ = mapMaybe (^. fromSdlEvent) <$> sdlPollEvents
  loadFont _ _ _ = return 1
  freeFont _ _ = return ()
  renderBegin _ = return ()
  renderClear p c = setRenderDrawColor (p ^. sdlpRenderer) c >> renderClear' (p ^. sdlpRenderer)
  renderFinish p = renderFinish' (p ^. sdlpRenderer)
  loadImage p fp = do
    im' <- readImage fp
    case im' of
      Left e -> error e
      Right im -> do
        bracket
          (imageToSurface im)
          SDLV.freeSurface
          (SDLV.createTextureFromSurface (p ^. sdlpRenderer))
  freeImage _ texture = SDLV.destroyTexture texture
  renderText _ _ = return ()
  viewportSize p = alloca $ \w -> alloca $ \h -> do
    SDLV.getWindowSize (p ^. sdlpWindow) w h
    w' <- peek w
    h' <- peek h
    return (V2 (fromIntegral w') (fromIntegral h'))
  renderSprites p sprites =
    forM_ sprites $ \sprite -> do
      let
        flipFlags = SDLEnum.SDL_FLIP_NONE
        srcRect = sprite ^. spriteSrcRect ^. from wrenchRect
        destRect = sprite ^. spriteDestRect ^. from wrenchRect
      with srcRect $ \srcRectPtr -> with destRect $ \destRectPtr -> do
        _ <- SDLV.renderCopyEx
          (p ^. sdlpRenderer)
          (sprite ^. spriteImage)
          srcRectPtr
          destRectPtr
          (CDouble (sprite ^. spriteRotation ^. degrees ^. getDegrees))
          nullPtr
          flipFlags
        return ()

destroyTexture :: SDLT.Texture -> IO ()
destroyTexture t = SDLV.destroyTexture t

withFontInit :: IO a -> IO a
withFontInit a = a

withImgInit :: IO a -> IO a
withImgInit a = a
