{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Wrench.Backends.Sdl.Sdl2Platform where

import           ClassyPrelude                       hiding (FilePath, Vector,
                                                      lookup, (</>))
import           Control.Lens                        (Getter, Iso', from, iso,
                                                      makeLenses, to, (^.))
import qualified Data.Text                           as T
import qualified SDL.Event               as SDLE
import qualified SDL.Video               as SDLV
import           Linear.V2                           (V2 (..))
import           Wrench.Angular
import           Wrench.Color
import Linear.Affine(Point(..),_Point)
import Linear.V4(V4(..))
import Linear.V3(V3(..))
import           Wrench.Event
import           Wrench.KeyMovement
import qualified Wrench.Keysym                       as Keysym
import           Wrench.MouseButton
import           Wrench.MouseButtonMovement
import           Wrench.MouseGrabMode
import           Wrench.Platform
import           Wrench.WindowSize
import Data.StateVar(($=),get)
#ifdef USE_OPENAL
import           Wrench.AL2D.AlBuffer
import qualified Wrench.AL2D.AlHelper                as AL
import           Wrench.AL2D.AlSource
#endif
import           Codec.Picture                       (DynamicImage (..),
                                                      Image (..), readImage)
import           Data.Vector.Storable                (thaw)
import           Foreign.C.Types                     (CDouble (..))
import SDL.Input.Mouse(setRelativeMouseMode)
import           Wrench.Backends.Sdl.Sdl2AudioLoader
import           Wrench.Rectangle
import qualified SDL.Video.Renderer as SDLR
import SDL.Video(Window)
import SDL.Input.Keyboard(Keysym(..))
import qualified SDL.Input.Keyboard.Codes as SDLK

data SDL2Platform = SDL2Platform {
    _sdlpRenderer      :: SDLR.Renderer
  , _sdlpWindow        :: Window
  , _sdlpMouseGrabMode :: MouseGrabMode
  }

$(makeLenses ''SDL2Platform)

#ifdef USE_OPENAL
type AudioBufferImpl = AlBuffer
type AudioSourceImpl = AlSource
audioPlayBuffer :: MonadIO m => AlBuffer -> PlayMode -> m AlSource
audioPlayBuffer b pm = do
    source <- AL.genSource
    AL.bufferToSource b source
    AL.playSource pm source
    return source
audioFreeBuffer :: AlBuffer -> IO ()
audioFreeBuffer = AL.freeBuffer
audioFreeSource :: AlSource -> IO ()
audioFreeSource = AL.freeSource
audioBufferFromFile :: AudioFile -> IO AlBuffer
audioBufferFromFile = AL.bufferFromFile
audioSourceIsStopped :: AlSource -> IO Bool
audioSourceIsStopped = AL.sourceIsStopped
withAudio :: (AL.AlcDevicePtr -> AL.AlcContextPtr -> IO a) -> IO a
withAudio = AL.withDefaultAl
#else
type AudioBufferImpl = ()
type AudioSourceImpl = ()
audioPlayBuffer :: Monad m => t -> t1 -> m ()
audioPlayBuffer _ _ = return ()
audioFreeBuffer :: Monad m => t -> m ()
audioFreeBuffer _ = return ()
audioFreeSource :: Monad m => t -> m ()
audioFreeSource _ = return ()
audioBufferFromFile :: Monad m => t -> m ()
audioBufferFromFile _ = return ()
audioSourceIsStopped :: Monad m => t -> m Bool
audioSourceIsStopped _ = return True
withAudio :: (a -> a1 -> t) -> t
withAudio f = f (error "null backend doesn't have a context/device") (error "null backend doesn't have a context/device")
#endif

floored :: (RealFrac a,Integral b) => Getter a b
floored = to floor

toSdlRect :: Integral a => Rectangle a -> SDLR.Rectangle a
toSdlRect r = SDLR.Rectangle (P (r ^. rectLeftTop)) (r ^. rectDimensions)

fromSdlRect :: Integral a => SDLR.Rectangle a -> Rectangle a
fromSdlRect (SDLR.Rectangle (P leftTop) dims) = rectFromOriginAndDim (fromIntegral <$> leftTop) (fromIntegral <$> dims)

fromSdlColor :: V4 Word8 -> Color
fromSdlColor (V4 r g b a) = mkColorFromRgba r g b a

toSdlColor :: Color -> V4 Word8
toSdlColor c = V4 (c ^. colorRed) (c ^. colorGreen) (c ^. colorBlue) (c ^. colorAlpha)

wrenchRect :: Integral a => Iso' (SDLR.Rectangle a) (Rectangle a)
wrenchRect = iso fromSdlRect toSdlRect

wrenchColor :: Iso' (V4 Word8) Color
wrenchColor = iso fromSdlColor toSdlColor


fromSdlEvent :: MouseGrabMode -> Getter SDLE.Event (Maybe Event)
fromSdlEvent grabMode = to (fromSdlEvent' . SDLE.eventPayload)
  where fromSdlEvent' (SDLE.KeyboardEvent (SDLE.KeyboardEventData{SDLE.keyboardEventKeyMotion=state,SDLE.keyboardEventRepeat=r,SDLE.keyboardEventKeysym=sym})) = Keyboard <$> liftM (KeyboardEvent (fromSdlMotionToKey state) r) (fromSdlSym sym)
        fromSdlEvent' (SDLE.QuitEvent{}) = Just Quit
        fromSdlEvent' (SDLE.MouseButtonEvent (SDLE.MouseButtonEventData{SDLE.mouseButtonEventMotion=motion,SDLE.mouseButtonEventButton=button,SDLE.mouseButtonEventPos=pos} )) = Just $ MouseButton MouseButtonEvent{_mouseButton=fromSdlMouseButton button,_mouseButtonMovement=fromSdlMotionToButton motion,_mousePosition=fromIntegral <$> (pos ^. _Point)}
        fromSdlEvent' (SDLE.MouseMotionEvent (SDLE.MouseMotionEventData{SDLE.mouseMotionEventPos=pos,SDLE.mouseMotionEventRelMotion=rel} )) =
          case grabMode of
            MouseGrabYes -> Just $ MouseAxis MouseAxisEvent{_mouseAxisDelta=fromIntegral <$> rel}
            MouseGrabNo ->  Just $ CursorMotion CursorMotionEvent{_cursorMotionPosition=fromIntegral <$> rel}
        fromSdlEvent' (SDLE.MouseWheelEvent(SDLE.MouseWheelEventData{SDLE.mouseWheelEventPos=w} )) = Just $ MouseWheel MouseWheelEvent {_mouseWheelDirection=fromIntegral <$> w}
        fromSdlEvent' _ = Nothing
        fromSdlMouseButton SDLE.ButtonLeft = LeftButton
        fromSdlMouseButton SDLE.ButtonMiddle = MiddleButton
        fromSdlMouseButton SDLE.ButtonRight = RightButton
        fromSdlMouseButton _ = error "Unknown mouse button"
        fromSdlMotionToKey SDLE.Pressed = KeyUp
        fromSdlMotionToKey SDLE.Released = KeyDown
        fromSdlMotionToButton SDLE.Pressed = ButtonDown
        fromSdlMotionToButton SDLE.Released = ButtonUp
        fromSdlSym (Keysym _ keycode _) = fromSdlKeycode keycode
        fromSdlKeycode k | k == SDLK.KeycodeUp = Just Keysym.Up
                         | k == SDLK.KeycodeDown = Just Keysym.Down
                         | k == SDLK.KeycodeLeft = Just Keysym.Left
                         | k == SDLK.KeycodeRight = Just Keysym.Right
                         | k == SDLK.KeycodeLGUI = Just Keysym.LeftGUI
                         | k == SDLK.KeycodeLShift = Just Keysym.LeftShift
                         | k == SDLK.KeycodeSpace = Just Keysym.Space
                         | k == SDLK.KeycodeEscape = Just Keysym.Escape
        fromSdlKeycode _ = Nothing
--        fromSdlKeycode k = error $ "Unknown keycode, normal " <> show k <> ", scancode " <> show (k .&. (complement (1 `shift` 30)))

setRenderDrawColor :: SDLR.Renderer -> Color -> IO ()
setRenderDrawColor renderer c = do
  (SDLR.rendererDrawColor renderer) $= (c ^. from wrenchColor)

renderClear' :: SDLR.Renderer -> IO ()
renderClear' = SDLR.clear

renderFinish' :: SDLR.Renderer -> IO ()
renderFinish' = SDLR.present

withWindow :: WindowSize -> T.Text -> MouseGrabMode -> (Window -> IO a) -> IO a
withWindow windowSize title mouseGrab callback = 
  let
    acquireResource = SDLV.createWindow title (SDLV.defaultWindow{SDLV.windowResizable=True,SDLV.windowInitialSize=fromIntegral <$> V2 screenAbsoluteWidth screenAbsoluteHeight})
    screenAbsoluteWidth,screenAbsoluteHeight :: Int
    screenAbsoluteWidth = case windowSize of
            DynamicWindowSize -> 0
            ConstantWindowSize w _ -> w
    screenAbsoluteHeight = case windowSize of
            DynamicWindowSize -> 0
            ConstantWindowSize _ h -> h
    releaseResource = SDLV.destroyWindow
  in
    bracket acquireResource releaseResource callback

withRenderer :: SDLV.Window -> (SDLR.Renderer -> IO a) -> IO a
withRenderer window callback =
  let acquireResource = SDLV.createRenderer window (-1) SDLR.defaultRenderer
      releaseResource = SDLV.destroyRenderer
  in bracket acquireResource releaseResource callback

{-
sizeToPoint :: SDLT.Size -> Point
sizeToPoint (SDLT.Size w h) = V2 (fromIntegral w) (fromIntegral h)
-}

withSdlPlatform :: WindowTitle -> WindowSize -> MouseGrabMode -> (SDL2Platform -> IO ()) -> IO ()
withSdlPlatform windowTitle windowSize mouseGrab cb =
  withFontInit $
    withImgInit $
      withWindow windowSize (unpackWindowTitle windowTitle) mouseGrab $ \window -> do
        when (mouseGrab == MouseGrabYes) (void (setRelativeMouseMode True))
        withRenderer window $ \renderer ->
          withAudio $ \_ _ -> do
            case windowSize of
                DynamicWindowSize -> return ()
                ConstantWindowSize w h -> do
                    SDLR.rendererLogicalSize renderer $= Just (V2 (fromIntegral w) (fromIntegral h))
            cb (SDL2Platform renderer window mouseGrab)

eventArrayStaticSize :: Int
eventArrayStaticSize = 128

imageToSurface :: DynamicImage -> IO SDLR.Surface
imageToSurface (ImageRGBA8 im) = do
  let
    bitsPerPixel = 32
    bytesPerPixel = bitsPerPixel `div` 8
  mptr <- thaw (imageData im)
  SDLV.createRGBSurfaceFrom
    mptr                                            
    (fromIntegral <$> V2 (imageWidth im) (imageHeight im))
    bitsPerPixel
    (fromIntegral (imageWidth im * fromIntegral bytesPerPixel))
    (V4 0x000000FF 0x0000FF00 0x00FF0000 0xFF000000)

imageToSurface (ImageRGB8 im) = do
  let
    bitsPerPixel = 24
    bytesPerPixel = bitsPerPixel `div` 8
  mptr <- thaw (imageData im)
  SDLV.createRGBSurfaceFrom
    mptr
    (fromIntegral <$> V2 (imageWidth im) (imageHeight im))
    bitsPerPixel
    (fromIntegral (imageWidth im * fromIntegral bytesPerPixel))
    (V4 0x000000FF 0x0000FF00 0x00FF0000 0xFF000000)
imageToSurface _ = error "unsupported image format"


instance Platform SDL2Platform where
  type PlatformImage SDL2Platform = SDLR.Texture
  type PlatformFont SDL2Platform = Int
  type PlatformAudioBuffer SDL2Platform = AudioBufferImpl
  type PlatformAudioSource SDL2Platform = AudioSourceImpl
  loadAudio _ fn = do
    wavFile <- sdl2LoadWave fn
    case wavFile of
      Nothing -> error $ "error loading " <> fn
      Just wavFile' ->
        audioBufferFromFile wavFile'
  freeBuffer _ = audioFreeBuffer
  freeSource _ = audioFreeSource
  playBuffer _ = audioPlayBuffer
  sourceIsStopped _ = audioSourceIsStopped
  pollEvents p = mapMaybe (^. fromSdlEvent (p ^. sdlpMouseGrabMode)) <$> SDLE.pollEvents
  loadFont _ _ _ = return 1
  freeFont _ _ = return ()
  renderBegin _ = return ()
  renderClear p c = setRenderDrawColor (p ^. sdlpRenderer) c >> renderClear' (p ^. sdlpRenderer)
  renderFinish p = renderFinish' (p ^. sdlpRenderer)
  loadImage p fp = do
    im' <- readImage fp
    case im' of
      Left e -> error e
      Right im ->
        bracket
          (imageToSurface im)
          SDLV.freeSurface
          (SDLV.createTextureFromSurface (p ^. sdlpRenderer))
  freeImage _ = SDLV.destroyTexture
  renderText _ _ = return ()
  viewportSize p = do
    vs <- get (SDLV.windowSize (p ^. sdlpWindow))
    return (fromIntegral <$> vs)
  renderSprites p sprites =
    forM_ sprites $ \sprite -> do
      let
        flipFlags = V2 False False
        srcRect = fromIntegral <$> sprite ^. spriteSrcRect ^. from wrenchRect
        destRect = fromIntegral <$> sprite ^. spriteDestRect ^. from wrenchRect
        sc = sprite ^. spriteColor
        colorModVar = (SDLR.textureColorMod (sprite ^. spriteImage))
      colorModOriginal <- get colorModVar
      colorModVar $= (V3 (sc ^. colorRed) (sc ^. colorGreen) (sc ^. colorBlue))
      SDLR.copyEx
        (p ^. sdlpRenderer)
        (sprite ^. spriteImage)
        (Just srcRect)
        (Just destRect)
        (CDouble (realToFrac (sprite ^. spriteRotation ^. to radToDeg ^. _Degrees)))
        Nothing
        flipFlags
      colorModVar $= colorModOriginal
      return ()

destroyTexture :: SDLR.Texture -> IO ()
destroyTexture = SDLV.destroyTexture

withFontInit :: IO a -> IO a
withFontInit a = a

withImgInit :: IO a -> IO a
withImgInit a = a
