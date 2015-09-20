{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Wrench.Backends.Sge.SGEPlatform(
       SGEPlatform,
       withSGEPlatform
)
where

import           ClassyPrelude hiding((</>))
import           Control.Lens ((^.), to, _2)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Trans.Resource (allocate, runResourceT)
import qualified Data.Text as T ( unpack )
import           Linear.V2(_x, _y, V2(..))

import qualified SGE.Audio (
         BufferPtr
       , LoaderPtr
       , PlayerPtr
       , PlayStatus(..)
       , Repeat(..)
       , SoundPtr
       , createBufferExn
       , createSoundExn
       , destroyBuffer
       , destroySound
       , play
       , status
       , withFile
       )

import qualified SGE.Dim (
         Dim(..)
       , dimW
       , dimH
       )

import qualified SGE.Font (
         AddedPtr
       , ObjectPtr
       , SystemPtr
       , addFontExn
       , draw
       , createFontExn
       , destroyAdded
       , destroyFont
       )

import qualified SGE.Image (
         RGBA
       , makeRGBA
       )

import qualified SGE.Image2D (
         SystemPtr
       )

import qualified SGE.Input (
         CursorButtonCode(..)
       , CursorButtonState(..)
       , CursorPtr
       , CursorScrollCode(..)
       , FocusPtr
       , KeyCode(..)
       , KeyState(..)
       , MouseAxisCode(..)
       , MousePtr
       , withCursorButtonCallback
       , withCursorMoveCallback
       , withCursorScrollCallback
       , withFocusKeyCallback
       , withKeyRepeatCallback
       , withMouseAxisCallback
       )

import qualified SGE.Pos (
         Pos(..)
       , posX
       , posY
       )

import qualified SGE.Rect (
         Rect(..)
       )

import qualified SGE.Renderer (
         ContextPtr
       , DevicePtr
       , PlanarTexturePtr
       , beginRenderingExn
       , clear
       , destroyPlanarTexture
       , endRenderingAndDestroy
       , onscreenTarget
       , onscreenTargetDim
       , planarTextureFromPathExn
       )

import qualified SGE.Sprite (
         Object(..)
       , draw
       )

import qualified SGE.Systems (
         CursorOption(..)
       , InstancePtr
       , audioLoader
       , audioPlayer
       , cursor
       , focus
       , fontSystem
       , imageSystem
       , mouse
       , renderer
       , windowSystem
       , with
       )

import qualified SGE.Texture (
         destroyPart
       , partRawRectExn
       )

import qualified SGE.Window (
         SystemPtr
       , poll
       )

import Wrench.Angular (
         radToDeg
       , _Degrees
       )

import Wrench.Color (
         Color
       , colorAlpha
       , colorBlue
       , colorGreen
       , colorRed
       )

import Wrench.Event (
         CursorMotionEvent(..)
       , Event(..)
       , KeyboardEvent(..)
       , MouseAxisEvent(..)
       , MouseButtonEvent(..)
       , MouseWheelEvent(..)
       )

import Wrench.KeyMovement (
         KeyMovement(..)
       )

import qualified Wrench.Keysym as Keysym (
         Keysym(..)
       )

import qualified Wrench.MouseButton as MouseButton (
         MouseButton(..)
       )

import Wrench.MouseButtonMovement (
         MouseButtonMovement(..)
       )

import Wrench.MouseGrabMode (
         MouseGrabMode(..)
       )

import Wrench.Platform (
         Platform(..)
       , WindowTitle(..)
       , spriteColor
       , spriteDestRect
       , spriteImage
       , spriteRotation
       , spriteSrcRect
       , textColor
       , textContent
       , textFont
       , textPosition
       )

import Wrench.PlayMode (
         PlayMode(..)
       )

import Wrench.Rectangle (
         Rectangle
       , rectLeftTop
       , rectDimensions
       )

import Wrench.WindowSize (
         WindowSize(..)
       )

data SGEPlatform = SGEPlatform {
     _sgepSystem :: SGE.Systems.InstancePtr
   , _sgepContext :: IORef (Maybe SGE.Renderer.ContextPtr)
   , _sgepSize :: WindowSize
   }

$(makeLenses ''SGEPlatform)

audioLoader :: SGEPlatform -> SGE.Audio.LoaderPtr
audioLoader p = SGE.Systems.audioLoader (p ^. sgepSystem)

audioPlayer :: SGEPlatform -> SGE.Audio.PlayerPtr
audioPlayer p = SGE.Systems.audioPlayer (p ^. sgepSystem)

focus :: SGEPlatform -> SGE.Input.FocusPtr
focus p = SGE.Systems.focus (p ^. sgepSystem)

cursor :: SGEPlatform -> SGE.Input.CursorPtr
cursor p = SGE.Systems.cursor (p ^. sgepSystem)

mouse :: SGEPlatform -> SGE.Input.MousePtr
mouse p = SGE.Systems.mouse (p ^. sgepSystem)

renderer :: SGEPlatform -> SGE.Renderer.DevicePtr
renderer p = SGE.Systems.renderer (p ^. sgepSystem)

window :: SGEPlatform -> SGE.Window.SystemPtr
window p = SGE.Systems.windowSystem (p ^. sgepSystem)

fontSystem :: SGEPlatform -> SGE.Font.SystemPtr
fontSystem p = SGE.Systems.fontSystem (p ^. sgepSystem)

imageSystem :: SGEPlatform -> SGE.Image2D.SystemPtr
imageSystem p = SGE.Systems.imageSystem (p ^. sgepSystem)

windowSizeToMaybe:: WindowSize -> Maybe SGE.Dim.Dim
windowSizeToMaybe sz = case sz of
                  DynamicWindowSize -> Nothing
                  ConstantWindowSize w h -> Just (SGE.Dim.Dim (w, h))

failMaybe :: Maybe a -> String -> IO a
failMaybe mb message =
          case mb of
               Just x -> return x
               Nothing -> error message

contextError :: SGEPlatform -> IO SGE.Renderer.ContextPtr
contextError p = do
             context <- readIORef (p ^. sgepContext)
             failMaybe context "No active render context"

toSGEColor :: Color -> SGE.Image.RGBA
toSGEColor c = SGE.Image.makeRGBA (c ^. colorRed) (c ^. colorBlue) (c ^. colorGreen) (c ^. colorAlpha)

toSGEPos :: Integral a => V2 a -> SGE.Pos.Pos
toSGEPos p = SGE.Pos.Pos (fromIntegral (p ^._x), fromIntegral (p ^._y))

toSGEDim :: Integral a => V2 a -> SGE.Dim.Dim
toSGEDim p = SGE.Dim.Dim (fromIntegral (p ^._x), fromIntegral (p ^._y))

toSGERect :: Integral a => Rectangle a -> SGE.Rect.Rect
toSGERect r = SGE.Rect.Rect (toSGEPos (r ^. rectLeftTop), toSGEDim (r ^. rectDimensions))

fromSGEDim :: SGE.Dim.Dim -> V2 Int
fromSGEDim d = V2 (fromIntegral (SGE.Dim.dimW d)) (fromIntegral (SGE.Dim.dimH d))

fromSGEPos :: SGE.Pos.Pos -> V2 Int
fromSGEPos p = V2 (fromIntegral (SGE.Pos.posX p)) (fromIntegral (SGE.Pos.posY p))

makeMouseButtonMovement :: SGE.Input.CursorButtonState -> MouseButtonMovement
makeMouseButtonMovement s = case s of
                        SGE.Input.CursorbuttonstatePressed -> ButtonDown
                        SGE.Input.CursorbuttonstateReleased -> ButtonUp

makeMouseButton :: SGE.Input.CursorButtonCode -> Maybe MouseButton.MouseButton
makeMouseButton b = case b of
                SGE.Input.CursorbuttoncodeLeft -> Just MouseButton.LeftButton
                SGE.Input.CursorbuttoncodeMiddle -> Just MouseButton.MiddleButton
                SGE.Input.CursorbuttoncodeRight -> Just MouseButton.RightButton
                SGE.Input.CursorbuttoncodeUnknown -> Nothing

makeKeyMovement :: SGE.Input.KeyState -> KeyMovement
makeKeyMovement s = case s of
                SGE.Input.KeystatePressed -> KeyDown
                SGE.Input.KeystateReleased -> KeyUp

makeMouseDelta :: SGE.Input.MouseAxisCode -> Int -> Maybe (V2 Int)
makeMouseDelta a d = case a of
               SGE.Input.MouseaxiscodeX -> Just (V2 (fromIntegral d) 0)
               SGE.Input.MouseaxiscodeY -> Just (V2 0 (fromIntegral d))
               SGE.Input.MouseaxiscodeWheel -> Nothing
               SGE.Input.MouseaxiscodeUnknown -> Nothing

makeScrollDelta :: SGE.Input.CursorScrollCode -> Int -> V2 Int
makeScrollDelta c d = case c of
                SGE.Input.CursorscrollcodeVertical -> V2 0 (fromIntegral d)
                SGE.Input.CursorscrollcodeHorizontal -> V2 (fromIntegral d) 0

makeKeySym :: SGE.Input.KeyCode -> Maybe Keysym.Keysym
makeKeySym s = case s of
            SGE.Input.KeycodeEscape -> Just Keysym.Escape
            SGE.Input.KeycodeReturn -> Just Keysym.Return
            SGE.Input.KeycodeLeftShift -> Just Keysym.LeftShift
            SGE.Input.KeycodeRightShift -> Just Keysym.RightShift
            SGE.Input.KeycodeTab -> Just Keysym.Tab
            SGE.Input.KeycodeBackspace -> Just Keysym.Backspace
            SGE.Input.KeycodeLeftControl -> Just Keysym.LeftControl
            SGE.Input.KeycodeRightControl -> Just Keysym.RightControl
            SGE.Input.KeycodeLeftAlt -> Just Keysym.LeftAlt
            SGE.Input.KeycodeRightAlt -> Just Keysym.RightAlt
            SGE.Input.KeycodeSpace -> Just Keysym.Space
            SGE.Input.KeycodeCapital -> Nothing
            SGE.Input.KeycodeInsert -> Just Keysym.Insert
            SGE.Input.KeycodeHome -> Just Keysym.Home
            SGE.Input.KeycodePageup -> Just Keysym.PageUp
            SGE.Input.KeycodeDelete -> Just Keysym.Delete
            SGE.Input.KeycodeEnd -> Just Keysym.End
            SGE.Input.KeycodePagedown -> Just Keysym.PageDown
            SGE.Input.KeycodeLeft -> Just Keysym.Left
            SGE.Input.KeycodeRight -> Just Keysym.Right
            SGE.Input.KeycodeUp -> Just Keysym.Up
            SGE.Input.KeycodeDown -> Just Keysym.Down
            SGE.Input.KeycodeA -> Just Keysym.A
            SGE.Input.KeycodeB -> Just Keysym.B
            SGE.Input.KeycodeC -> Just Keysym.C
            SGE.Input.KeycodeD -> Just Keysym.D
            SGE.Input.KeycodeE -> Just Keysym.E
            SGE.Input.KeycodeF -> Just Keysym.F
            SGE.Input.KeycodeG -> Just Keysym.G
            SGE.Input.KeycodeH -> Just Keysym.H
            SGE.Input.KeycodeI -> Just Keysym.I
            SGE.Input.KeycodeJ -> Just Keysym.J
            SGE.Input.KeycodeK -> Just Keysym.K
            SGE.Input.KeycodeL -> Just Keysym.L
            SGE.Input.KeycodeM -> Just Keysym.M
            SGE.Input.KeycodeN -> Just Keysym.N
            SGE.Input.KeycodeO -> Just Keysym.O
            SGE.Input.KeycodeP -> Just Keysym.P
            SGE.Input.KeycodeQ -> Just Keysym.Q
            SGE.Input.KeycodeR -> Just Keysym.R
            SGE.Input.KeycodeS -> Just Keysym.S
            SGE.Input.KeycodeT -> Just Keysym.T
            SGE.Input.KeycodeU -> Just Keysym.U
            SGE.Input.KeycodeV -> Just Keysym.V
            SGE.Input.KeycodeW -> Just Keysym.W
            SGE.Input.KeycodeX -> Just Keysym.X
            SGE.Input.KeycodeY -> Just Keysym.Y
            SGE.Input.KeycodeZ -> Just Keysym.Z
            SGE.Input.Keycode0 -> Just Keysym.Number0
            SGE.Input.Keycode1 -> Just Keysym.Number1
            SGE.Input.Keycode2 -> Just Keysym.Number2
            SGE.Input.Keycode3 -> Just Keysym.Number3
            SGE.Input.Keycode4 -> Just Keysym.Number4
            SGE.Input.Keycode5 -> Just Keysym.Number5
            SGE.Input.Keycode6 -> Just Keysym.Number6
            SGE.Input.Keycode7 -> Just Keysym.Number7
            SGE.Input.Keycode8 -> Just Keysym.Number8
            SGE.Input.Keycode9 -> Just Keysym.Number9
            SGE.Input.KeycodeF1 -> Just Keysym.F1
            SGE.Input.KeycodeF2 -> Just Keysym.F2
            SGE.Input.KeycodeF3 -> Just Keysym.F3
            SGE.Input.KeycodeF4 -> Just Keysym.F4
            SGE.Input.KeycodeF5 -> Just Keysym.F5
            SGE.Input.KeycodeF6 -> Just Keysym.F6
            SGE.Input.KeycodeF7 -> Just Keysym.F7
            SGE.Input.KeycodeF8 -> Just Keysym.F8
            SGE.Input.KeycodeF9 -> Just Keysym.F9
            SGE.Input.KeycodeF10 -> Just Keysym.F10
            SGE.Input.KeycodeF11 -> Just Keysym.F11
            SGE.Input.KeycodeF12 -> Just Keysym.F12
            SGE.Input.KeycodeF13 -> Just Keysym.F13
            SGE.Input.KeycodeF14 -> Just Keysym.F14
            SGE.Input.KeycodeF15 -> Just Keysym.F15
            SGE.Input.KeycodeComma -> Just Keysym.Comma
            SGE.Input.KeycodeColon -> Just Keysym.Colon
            SGE.Input.KeycodeDecimal -> Just Keysym.DecimalSeparator
            SGE.Input.KeycodeApostrophe -> Nothing
            SGE.Input.KeycodeBackslash -> Just Keysym.Backslash
            SGE.Input.KeycodeGrave -> Nothing
            SGE.Input.KeycodeSubtract -> Just Keysym.Minus
            SGE.Input.KeycodeLeftBracket -> Just Keysym.LeftBracket
            SGE.Input.KeycodeRightBracket -> Just Keysym.RightBracket
            SGE.Input.KeycodeSemicolon -> Just Keysym.Semicolon
            SGE.Input.KeycodeSlash -> Just Keysym.Slash
            SGE.Input.KeycodeUnderline -> Just Keysym.Underscore
            SGE.Input.KeycodeScroll -> Just Keysym.ScrollLock
            SGE.Input.KeycodePause -> Just Keysym.Pause
            SGE.Input.KeycodeLeftWin -> Nothing
            SGE.Input.KeycodeRightWin -> Nothing
            SGE.Input.KeycodeNum0 -> Just Keysym.Keypad0
            SGE.Input.KeycodeNum1 -> Just Keysym.Keypad1
            SGE.Input.KeycodeNum2 -> Just Keysym.Keypad2
            SGE.Input.KeycodeNum3 -> Just Keysym.Keypad3
            SGE.Input.KeycodeNum4 -> Just Keysym.Keypad4
            SGE.Input.KeycodeNum5 -> Just Keysym.Keypad5
            SGE.Input.KeycodeNum6 -> Just Keysym.Keypad6
            SGE.Input.KeycodeNum7 -> Just Keysym.Keypad7
            SGE.Input.KeycodeNum8 -> Just Keysym.Keypad8
            SGE.Input.KeycodeNum9 -> Just Keysym.Keypad9
            SGE.Input.KeycodeNumComma -> Just Keysym.KeypadPeriod
            SGE.Input.KeycodeNumEnter -> Just Keysym.KeypadEnter
            SGE.Input.KeycodeNumEquals -> Nothing
            SGE.Input.KeycodeNumLock -> Just Keysym.NumLockClear
            SGE.Input.KeycodeAdd -> Nothing
            SGE.Input.KeycodeMinus -> Just Keysym.Minus
            SGE.Input.KeycodeMultiply-> Nothing
            SGE.Input.KeycodeDivide -> Nothing
            SGE.Input.KeycodeApps -> Just Keysym.Application
            SGE.Input.KeycodeCircumflex -> Nothing
            SGE.Input.KeycodeAt -> Just Keysym.At
            SGE.Input.KeycodeAx -> Nothing
            SGE.Input.KeycodeEquals -> Just Keysym.Equals
            SGE.Input.KeycodeKana -> Nothing
            SGE.Input.KeycodeKanji -> Nothing
            SGE.Input.KeycodeConvert -> Nothing
            SGE.Input.KeycodeNoconvert -> Nothing
            SGE.Input.KeycodePeriod -> Just Keysym.Period
            SGE.Input.KeycodePower -> Just Keysym.Power
            SGE.Input.KeycodeSleep -> Just Keysym.Sleep
            SGE.Input.KeycodeStop -> Just Keysym.Stop
            SGE.Input.KeycodeSysrq -> Just Keysym.SysReq
            SGE.Input.KeycodePrint -> Just Keysym.PrintScreen
            SGE.Input.KeycodeUnlabeled -> Nothing
            SGE.Input.KeycodeYen -> Nothing
            SGE.Input.KeycodeUnknown -> Nothing

data InputEvent =
     KeyEvent (SGE.Input.KeyCode, SGE.Input.KeyState)
     | KeyRepeatEvent SGE.Input.KeyCode
     | CursorButtonEvent (SGE.Input.CursorButtonCode, SGE.Input.CursorButtonState, SGE.Pos.Pos)
     | CursorMoveEvent SGE.Pos.Pos
     | CursorScrollEvent (SGE.Input.CursorScrollCode, Int)
     | MouseMoveEvent (SGE.Input.MouseAxisCode, Int)

type Inputs = [InputEvent]
type InputsRef = IORef Inputs

makeEvent :: InputEvent -> Maybe Event
makeEvent event = case event of
          KeyEvent (k, s) -> do
                   keysym <- makeKeySym k
                   return $ Keyboard (KeyboardEvent (makeKeyMovement s) False keysym)
          KeyRepeatEvent k -> do
                         keysym <- makeKeySym k
                         return $ Keyboard (KeyboardEvent KeyDown True keysym)
          CursorButtonEvent (k, s, p) -> do
                            buttonCode <- makeMouseButton k
                            return $ MouseButton (MouseButtonEvent buttonCode (makeMouseButtonMovement s) (fromSGEPos p))
          CursorMoveEvent p -> return $ CursorMotion (CursorMotionEvent (fromSGEPos p))
          CursorScrollEvent (c, d) -> return $ MouseWheel (MouseWheelEvent (makeScrollDelta c d))
          MouseMoveEvent (a, d) -> do
                         delta <- makeMouseDelta a d
                         return $ MouseAxis (MouseAxisEvent delta)


makeInputResults :: SGEPlatform -> InputsRef -> IO [Event]
makeInputResults p inputs = do
                 result <- SGE.Window.poll (window p)
                 if result then do
                    curInputs <- readIORef inputs
                    return $ mapMaybe makeEvent curInputs
                 else
                    return [Quit]

appendInput :: InputsRef -> InputEvent -> IO ()
appendInput inputs input = do
            old <- readIORef inputs
            writeIORef inputs (input : old)

cursorButtonCallback :: InputsRef -> SGE.Input.CursorButtonCode -> SGE.Input.CursorButtonState -> SGE.Pos.Pos -> IO ()
cursorButtonCallback inputs code status pos = appendInput inputs (CursorButtonEvent (code, status, pos))

cursorMoveCallback :: InputsRef -> SGE.Pos.Pos -> IO ()
cursorMoveCallback inputs pos = appendInput inputs (CursorMoveEvent pos)

cursorScrollCallback :: InputsRef -> SGE.Input.CursorScrollCode -> Int -> IO ()
cursorScrollCallback inputs code delta = appendInput inputs (CursorScrollEvent (code, delta))

mouseAxisCallback :: InputsRef -> SGE.Input.MouseAxisCode -> Int -> IO ()
mouseAxisCallback inputs axis delta = appendInput inputs (MouseMoveEvent (axis, delta))

keyCallback :: InputsRef -> SGE.Input.KeyCode -> SGE.Input.KeyState -> IO ()
keyCallback inputs key status = appendInput inputs (KeyEvent (key, status))

keyRepeatCallback :: InputsRef -> SGE.Input.KeyCode -> IO ()
keyRepeatCallback inputs key = appendInput inputs (KeyRepeatEvent key)

makeAudioRepeat :: PlayMode -> SGE.Audio.Repeat
makeAudioRepeat p = case p of
                PlayModeOnce -> SGE.Audio.RepeatOnce
                PlayModeLooping -> SGE.Audio.RepeatLoop

makeCursorOption :: MouseGrabMode -> SGE.Systems.CursorOption
makeCursorOption m = case m of
                 MouseGrabNo -> SGE.Systems.CursoroptionNormal
                 MouseGrabYes -> SGE.Systems.CursoroptionExclusive

instance Platform SGEPlatform where
         type PlatformAudioBuffer SGEPlatform = SGE.Audio.BufferPtr
         type PlatformAudioSource SGEPlatform = SGE.Audio.SoundPtr
         type PlatformImage SGEPlatform = SGE.Renderer.PlanarTexturePtr
         type PlatformFont SGEPlatform = (SGE.Font.AddedPtr, SGE.Font.ObjectPtr)
         loadAudio p path =
                   SGE.Audio.withFile (audioLoader p) path (SGE.Audio.createBufferExn (audioPlayer p))
         playBuffer _ buffer mode = do
                    sound <- SGE.Audio.createSoundExn buffer
                    SGE.Audio.play sound (makeAudioRepeat mode) >> return sound
         freeBuffer _ buffer =
                    SGE.Audio.destroyBuffer buffer
         freeSource _ sound =
                    SGE.Audio.destroySound sound
         sourceIsStopped _ sound = do
                         status <- SGE.Audio.status sound
                         return (status == SGE.Audio.PlayStatusStopped)
         loadImage p path =
                   SGE.Renderer.planarTextureFromPathExn (renderer p) (imageSystem p) path
         freeImage _ tex =
                   SGE.Renderer.destroyPlanarTexture tex
         -- FIXME: This is not exception-safe
         loadFont p path size = do
                  added <- SGE.Font.addFontExn (fontSystem p) path
                  font <- SGE.Font.createFontExn (fontSystem p) Nothing (Just size)
                  return (added, font)
         freeFont _ (added, font) =
                  SGE.Font.destroyFont font >> SGE.Font.destroyAdded added
         pollEvents p = do
                    inputs <- newIORef []
                    SGE.Input.withFocusKeyCallback (focus p) (keyCallback inputs)
                        (SGE.Input.withKeyRepeatCallback (focus p) (keyRepeatCallback inputs)
                             (SGE.Input.withCursorButtonCallback (cursor p) (cursorButtonCallback inputs)
                                  (SGE.Input.withCursorMoveCallback (cursor p) (cursorMoveCallback inputs)
                                       (SGE.Input.withMouseAxisCallback (mouse p) (mouseAxisCallback inputs)
                                           (SGE.Input.withCursorScrollCallback (cursor p) (cursorScrollCallback inputs) (makeInputResults p inputs))))))
         renderBegin p = do
                     context <- SGE.Renderer.beginRenderingExn $ renderer p
                     writeIORef (p ^. sgepContext) (Just context)
         renderClear p color = do
                     context <- contextError p
                     SGE.Renderer.clear context $ toSGEColor color
         renderFinish p = do
                      context <- contextError p
                      SGE.Renderer.endRenderingAndDestroy (renderer p) context
                      >> writeIORef (p ^. sgepContext) Nothing
         renderText p texts = do
                      context <- contextError p
                      mapM_ (\t -> SGE.Font.draw (renderer p) context (t ^. textFont ^. _2) (unpack (t ^. textContent)) (toSGEPos (t ^. textPosition)) (toSGEColor (t ^. textColor))) texts
         viewportSize p = do
                      dim <- SGE.Renderer.onscreenTargetDim (SGE.Renderer.onscreenTarget (renderer p))
                      return $ fromSGEDim dim
         renderSprites p sprites = runResourceT $ do
                       context <- liftIO $ contextError p
                       textures <- mapM allocTexture sprites
                       liftIO $ SGE.Sprite.draw (renderer p) context (windowSizeToMaybe (p ^. sgepSize)) (zipWith translateSprite sprites textures)
                       where allocTexture s = snd <$> allocate (SGE.Texture.partRawRectExn (s ^. spriteImage) (toSGERect (s ^. spriteSrcRect))) SGE.Texture.destroyPart
                             translateSprite s tex = SGE.Sprite.Object (toSGEPos (s ^. spriteDestRect ^. rectLeftTop))
                                                                       (toSGEDim (s ^. spriteDestRect ^. rectDimensions))
                                                                       (realToFrac (s ^. spriteRotation ^. to radToDeg ^. _Degrees))
                                                                       tex
                                                                       (toSGEColor (s ^. spriteColor))

withSGEPlatform :: WindowTitle -> WindowSize -> MouseGrabMode -> (SGEPlatform -> IO ()) -> IO ()
withSGEPlatform windowTitle size cursorOption cb =
                SGE.Systems.with (T.unpack (unpackWindowTitle windowTitle)) (windowSizeToMaybe size) (makeCursorOption cursorOption) $ \system -> do
                                 context <- newIORef Nothing
                                 cb (SGEPlatform system context size)
