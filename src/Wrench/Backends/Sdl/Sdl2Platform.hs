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
import           Data.Bits
import qualified Data.Text                           as T
import           Foreign.ForeignPtr                  (withForeignPtr)
import           Foreign.Ptr                         (castPtr)
import qualified Graphics.UI.SDL.Enum                as SDLEnum
import qualified Graphics.UI.SDL.Event               as SDLE
import qualified Graphics.UI.SDL.Types               as SDLT
import qualified Graphics.UI.SDL.Video               as SDLV
import           Linear.V2                           (V2 (..), _x, _y)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.KeyMovement
import qualified Wrench.Keysym                       as Keysym
import           Wrench.MouseButton
import           Wrench.MouseButtonMovement
import           Wrench.MouseGrabMode
import           Wrench.Platform
import           Wrench.WindowSize
#ifdef USE_OPENAL
import           Wrench.AL2D.AlBuffer
import qualified Wrench.AL2D.AlHelper                as AL
import           Wrench.AL2D.AlSource
#endif
import           Codec.Picture                       (DynamicImage (..),
                                                      Image (..), readImage)
import           Data.Vector.Storable                (unsafeToForeignPtr0)
import           Foreign.C.String                    (withCStringLen)
import           Foreign.C.Types                     (CDouble (..))
import           Foreign.Marshal.Alloc               (alloca)
import           Foreign.Marshal.Array               (allocaArray, peekArray)
import           Foreign.Marshal.Utils               (with)
import           Foreign.Ptr                         (Ptr, nullPtr)
import           Foreign.Storable                    (peek)
import           Wrench.Backends.Sdl.Sdl2AudioLoader
import           Wrench.Rectangle

data SDL2Platform = SDL2Platform {
    _sdlpRenderer      :: SDLT.Renderer
  , _sdlpWindow        :: SDLT.Window
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

toSdlRect :: Rectangle -> SDLT.Rect
toSdlRect r = SDLT.Rect
              (r ^. rectLeftTop ^. _x ^. floored)
              (r ^. rectLeftTop ^. _y ^. floored)
              (r ^. rectDimensions ^. _x ^. floored)
              (r ^. rectDimensions ^. _y ^. floored)

fromSdlRect :: SDLT.Rect -> Rectangle
fromSdlRect (SDLT.Rect x y w h) = rectFromPoints (V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral (x + w)) (fromIntegral (y + h)))

fromSdlColor :: SDLT.Color -> Color
fromSdlColor (SDLT.Color r g b a) = mkColorFromRgba r g b a

toSdlColor :: Color -> SDLT.Color
toSdlColor c = SDLT.Color (c ^. colorRed) (c ^. colorGreen) (c ^. colorBlue) (c ^. colorAlpha)

wrenchRect :: Iso' SDLT.Rect Rectangle
wrenchRect = iso fromSdlRect toSdlRect

wrenchColor :: Iso' SDLT.Color Color
wrenchColor = iso fromSdlColor toSdlColor


fromSdlEvent :: MouseGrabMode -> Getter SDLT.Event (Maybe Event)
fromSdlEvent grabMode = to fromSdlEvent'
  where fromSdlEvent' (SDLT.KeyboardEvent _ _ _ state r sym) = Just $ Keyboard $ KeyboardEvent (fromSdlKeyState state) (r /= 0) (fromSdlSym sym)
        fromSdlEvent' (SDLT.QuitEvent{}) = Just Quit
        fromSdlEvent' (SDLT.MouseButtonEvent{SDLT.mouseButtonEventState=state,SDLT.mouseButtonEventButton=button,SDLT.mouseButtonEventX=x,SDLT.mouseButtonEventY=y}) = Just $ MouseButton $ MouseButtonEvent{_mouseButton=fromSdlMouseButton button,_mouseButtonMovement=fromSdlMouseButtonState state,_mousePosition=fromIntegral <$> V2 x y}
        fromSdlEvent' (SDLT.MouseMotionEvent{SDLT.mouseMotionEventX=px,SDLT.mouseMotionEventY=py,SDLT.mouseMotionEventXRel=dx,SDLT.mouseMotionEventYRel=dy}) =
          case grabMode of
            MouseGrabYes -> Just $ MouseAxis $ MouseAxisEvent{_mouseAxisDelta=V2 (fromIntegral dx) (fromIntegral dy)}
            MouseGrabNo ->  Just $ CursorMotion $ CursorMotionEvent{_cursorMotionPosition=V2 (fromIntegral px) (fromIntegral py)}
        fromSdlEvent' _ = Nothing
        fromSdlMouseButton SDLEnum.SDL_BUTTON_LEFT = LeftButton
        fromSdlMouseButton SDLEnum.SDL_BUTTON_MIDDLE = MiddleButton
        fromSdlMouseButton SDLEnum.SDL_BUTTON_RIGHT = RightButton
        fromSdlMouseButton _ = error "Unknown mouse button"
        fromSdlMouseButtonState SDLEnum.SDL_PRESSED = ButtonDown
        fromSdlMouseButtonState SDLEnum.SDL_RELEASED = ButtonUp
        fromSdlMouseButtonState _ = error "Unknown mouse button state"
        fromSdlKeyState k | k == SDLEnum.SDL_RELEASED = KeyUp
                          | k == SDLEnum.SDL_PRESSED = KeyDown
        fromSdlSym (SDLT.Keysym _ keycode _) = fromSdlKeycode keycode
        fromSdlKeycode k | k == SDLEnum.SDLK_UP = Keysym.Up
                         | k == SDLEnum.SDLK_DOWN = Keysym.Down
                         | k == SDLEnum.SDLK_LEFT = Keysym.Left
                         | k == SDLEnum.SDLK_RIGHT = Keysym.Right
                         | k == SDLEnum.SDLK_RIGHT = Keysym.Right
                         | k == SDLEnum.SDLK_LGUI = Keysym.LeftGUI
                         | k == SDLEnum.SDLK_LSHIFT = Keysym.LeftShift
                         | k == SDLEnum.SDLK_SPACE = Keysym.Space
                         | k == SDLEnum.SDLK_ESCAPE = Keysym.Escape
        fromSdlKeycode k = error $ "Unknown keycode, normal " <> show k <> ", scancode " <> show (k .&. (complement (1 `shift` 30)))

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


withWindow :: WindowSize -> T.Text -> MouseGrabMode -> (SDLT.Window -> IO a) -> IO a
withWindow windowSize title' mouseGrab callback = withCStringLen (unpack title') $ \title ->
  let
    acquireResource = SDLV.createWindow (fst title) SDLEnum.SDL_WINDOWPOS_UNDEFINED SDLEnum.SDL_WINDOWPOS_UNDEFINED (fromIntegral screenAbsoluteWidth) (fromIntegral screenAbsoluteHeight) windowFlags
    screenAbsoluteWidth,screenAbsoluteHeight :: Int
    screenAbsoluteWidth = case windowSize of
            DynamicWindowSize -> 0
            ConstantWindowSize w _ -> w
    screenAbsoluteHeight = case windowSize of
            DynamicWindowSize -> 0
            ConstantWindowSize _ h -> h
    windowFlags = SDLEnum.SDL_WINDOW_RESIZABLE{- .|. (if mouseGrab == MouseGrabYes then SDLEnum.SDL_WINDOW_INPUT_GRABBED else 0)-}
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

withSdlPlatform :: WindowTitle -> WindowSize -> MouseGrabMode -> (SDL2Platform -> IO ()) -> IO ()
withSdlPlatform windowTitle windowSize mouseGrab cb =
  withFontInit $
    withImgInit $
      withWindow windowSize (unpackWindowTitle windowTitle) mouseGrab $ \window -> do
        when (mouseGrab == MouseGrabYes) (void (SDLE.setRelativeMouseMode True))
        withRenderer window $ \renderer -> do
          withAudio $ \_ _ -> do
            case windowSize of
                DynamicWindowSize -> return ()
                ConstantWindowSize w h -> do
                    _ <- SDLV.renderSetLogicalSize renderer (fromIntegral w) (fromIntegral h)
                    return ()
            cb (SDL2Platform renderer window mouseGrab)

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


instance Platform SDL2Platform where
  type PlatformImage SDL2Platform = SDLT.Texture
  type PlatformFont SDL2Platform = Int
  type PlatformAudioBuffer SDL2Platform = AudioBufferImpl
  type PlatformAudioSource SDL2Platform = AudioSourceImpl
  loadAudio _ fn = do
    wavFile <- sdl2LoadWave fn
    case wavFile of
      Nothing -> error $ "error loading " <> fn
      Just wavFile' -> do
        audioBufferFromFile wavFile'
  freeBuffer _ = audioFreeBuffer
  freeSource _ = audioFreeSource
  playBuffer _ b pm = audioPlayBuffer b pm
  sourceIsStopped _ = audioSourceIsStopped
  pollEvents p = mapMaybe (^. fromSdlEvent (p ^. sdlpMouseGrabMode)) <$> sdlPollEvents
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
