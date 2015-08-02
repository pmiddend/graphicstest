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
       , KeyboardPtr
       , KeyboardKey(..)
       , KeyState(..)
       , MouseAxisCode(..)
       , MousePtr
       , withCursorButtonCallback
       , withCursorMoveCallback
       , withKeyCallback
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
       , fontSystem
       , imageSystem
       , keyboard
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

keyboard :: SGEPlatform -> SGE.Input.KeyboardPtr
keyboard p = SGE.Systems.keyboard (p ^. sgepSystem)

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

makeKeySym :: SGE.Input.KeyboardKey -> Maybe Keysym.Keysym
makeKeySym s = case s of
            SGE.Input.KeyboardkeyEscape -> Just Keysym.Escape
            SGE.Input.KeyboardkeyReturn -> Just Keysym.Return
            SGE.Input.KeyboardkeyLeftShift -> Just Keysym.LeftShift
            SGE.Input.KeyboardkeyRightShift -> Just Keysym.RightShift
            SGE.Input.KeyboardkeyTab -> Just Keysym.Tab
            SGE.Input.KeyboardkeyBackspace -> Just Keysym.Backspace
            SGE.Input.KeyboardkeyLeftControl -> Just Keysym.LeftControl
            SGE.Input.KeyboardkeyRightControl -> Just Keysym.RightControl
            SGE.Input.KeyboardkeyLeftAlt -> Just Keysym.LeftAlt
            SGE.Input.KeyboardkeyRightAlt -> Just Keysym.RightAlt
            SGE.Input.KeyboardkeySpace -> Just Keysym.Space
            SGE.Input.KeyboardkeyCapital -> Nothing
            SGE.Input.KeyboardkeyInsert -> Just Keysym.Insert
            SGE.Input.KeyboardkeyHome -> Just Keysym.Home
            SGE.Input.KeyboardkeyPageup -> Just Keysym.PageUp
            SGE.Input.KeyboardkeyDelete -> Just Keysym.Delete
            SGE.Input.KeyboardkeyEnd -> Just Keysym.End
            SGE.Input.KeyboardkeyPagedown -> Just Keysym.PageDown
            SGE.Input.KeyboardkeyLeft -> Just Keysym.Left
            SGE.Input.KeyboardkeyRight -> Just Keysym.Right
            SGE.Input.KeyboardkeyUp -> Just Keysym.Up
            SGE.Input.KeyboardkeyDown -> Just Keysym.Down
            SGE.Input.KeyboardkeyA -> Just Keysym.A
            SGE.Input.KeyboardkeyB -> Just Keysym.B
            SGE.Input.KeyboardkeyC -> Just Keysym.C
            SGE.Input.KeyboardkeyD -> Just Keysym.D
            SGE.Input.KeyboardkeyE -> Just Keysym.E
            SGE.Input.KeyboardkeyF -> Just Keysym.F
            SGE.Input.KeyboardkeyG -> Just Keysym.G
            SGE.Input.KeyboardkeyH -> Just Keysym.H
            SGE.Input.KeyboardkeyI -> Just Keysym.I
            SGE.Input.KeyboardkeyJ -> Just Keysym.J
            SGE.Input.KeyboardkeyK -> Just Keysym.K
            SGE.Input.KeyboardkeyL -> Just Keysym.L
            SGE.Input.KeyboardkeyM -> Just Keysym.M
            SGE.Input.KeyboardkeyN -> Just Keysym.N
            SGE.Input.KeyboardkeyO -> Just Keysym.O
            SGE.Input.KeyboardkeyP -> Just Keysym.P
            SGE.Input.KeyboardkeyQ -> Just Keysym.Q
            SGE.Input.KeyboardkeyR -> Just Keysym.R
            SGE.Input.KeyboardkeyS -> Just Keysym.S
            SGE.Input.KeyboardkeyT -> Just Keysym.T
            SGE.Input.KeyboardkeyU -> Just Keysym.U
            SGE.Input.KeyboardkeyV -> Just Keysym.V
            SGE.Input.KeyboardkeyW -> Just Keysym.W
            SGE.Input.KeyboardkeyX -> Just Keysym.X
            SGE.Input.KeyboardkeyY -> Just Keysym.Y
            SGE.Input.KeyboardkeyZ -> Just Keysym.Z
            SGE.Input.Keyboardkey0 -> Just Keysym.Number0
            SGE.Input.Keyboardkey1 -> Just Keysym.Number1
            SGE.Input.Keyboardkey2 -> Just Keysym.Number2
            SGE.Input.Keyboardkey3 -> Just Keysym.Number3
            SGE.Input.Keyboardkey4 -> Just Keysym.Number4
            SGE.Input.Keyboardkey5 -> Just Keysym.Number5
            SGE.Input.Keyboardkey6 -> Just Keysym.Number6
            SGE.Input.Keyboardkey7 -> Just Keysym.Number7
            SGE.Input.Keyboardkey8 -> Just Keysym.Number8
            SGE.Input.Keyboardkey9 -> Just Keysym.Number9
            SGE.Input.KeyboardkeyF1 -> Just Keysym.F1
            SGE.Input.KeyboardkeyF2 -> Just Keysym.F2
            SGE.Input.KeyboardkeyF3 -> Just Keysym.F3
            SGE.Input.KeyboardkeyF4 -> Just Keysym.F4
            SGE.Input.KeyboardkeyF5 -> Just Keysym.F5
            SGE.Input.KeyboardkeyF6 -> Just Keysym.F6
            SGE.Input.KeyboardkeyF7 -> Just Keysym.F7
            SGE.Input.KeyboardkeyF8 -> Just Keysym.F8
            SGE.Input.KeyboardkeyF9 -> Just Keysym.F9
            SGE.Input.KeyboardkeyF10 -> Just Keysym.F10
            SGE.Input.KeyboardkeyF11 -> Just Keysym.F11
            SGE.Input.KeyboardkeyF12 -> Just Keysym.F12
            SGE.Input.KeyboardkeyF13 -> Just Keysym.F13
            SGE.Input.KeyboardkeyF14 -> Just Keysym.F14
            SGE.Input.KeyboardkeyF15 -> Just Keysym.F15
            SGE.Input.KeyboardkeyComma -> Just Keysym.Comma
            SGE.Input.KeyboardkeyColon -> Just Keysym.Colon
            SGE.Input.KeyboardkeyDecimal -> Just Keysym.DecimalSeparator
            SGE.Input.KeyboardkeyApostrophe -> Nothing
            SGE.Input.KeyboardkeyBackslash -> Just Keysym.Backslash
            SGE.Input.KeyboardkeyGrave -> Nothing
            SGE.Input.KeyboardkeySubtract -> Just Keysym.Minus
            SGE.Input.KeyboardkeyLeftBracket -> Just Keysym.LeftBracket
            SGE.Input.KeyboardkeyRightBracket -> Just Keysym.RightBracket
            SGE.Input.KeyboardkeySemicolon -> Just Keysym.Semicolon
            SGE.Input.KeyboardkeySlash -> Just Keysym.Slash
            SGE.Input.KeyboardkeyUnderline -> Just Keysym.Underscore
            SGE.Input.KeyboardkeyScroll -> Just Keysym.ScrollLock
            SGE.Input.KeyboardkeyPause -> Just Keysym.Pause
            SGE.Input.KeyboardkeyLeftWin -> Nothing
            SGE.Input.KeyboardkeyRightWin -> Nothing
            SGE.Input.KeyboardkeyNum0 -> Just Keysym.Keypad0
            SGE.Input.KeyboardkeyNum1 -> Just Keysym.Keypad1
            SGE.Input.KeyboardkeyNum2 -> Just Keysym.Keypad2
            SGE.Input.KeyboardkeyNum3 -> Just Keysym.Keypad3
            SGE.Input.KeyboardkeyNum4 -> Just Keysym.Keypad4
            SGE.Input.KeyboardkeyNum5 -> Just Keysym.Keypad5
            SGE.Input.KeyboardkeyNum6 -> Just Keysym.Keypad6
            SGE.Input.KeyboardkeyNum7 -> Just Keysym.Keypad7
            SGE.Input.KeyboardkeyNum8 -> Just Keysym.Keypad8
            SGE.Input.KeyboardkeyNum9 -> Just Keysym.Keypad9
            SGE.Input.KeyboardkeyNumComma -> Just Keysym.KeypadPeriod
            SGE.Input.KeyboardkeyNumEnter -> Just Keysym.KeypadEnter
            SGE.Input.KeyboardkeyNumEquals -> Nothing
            SGE.Input.KeyboardkeyNumLock -> Just Keysym.NumLockClear
            SGE.Input.KeyboardkeyAdd -> Nothing
            SGE.Input.KeyboardkeyMinus -> Just Keysym.Minus
            SGE.Input.KeyboardkeyMultiply-> Nothing
            SGE.Input.KeyboardkeyDivide -> Nothing
            SGE.Input.KeyboardkeyApps -> Just Keysym.Application
            SGE.Input.KeyboardkeyCircumflex -> Nothing
            SGE.Input.KeyboardkeyAt -> Just Keysym.At
            SGE.Input.KeyboardkeyAx -> Nothing
            SGE.Input.KeyboardkeyEquals -> Just Keysym.Equals
            SGE.Input.KeyboardkeyKana -> Nothing
            SGE.Input.KeyboardkeyKanji -> Nothing
            SGE.Input.KeyboardkeyConvert -> Nothing
            SGE.Input.KeyboardkeyNoconvert -> Nothing
            SGE.Input.KeyboardkeyPeriod -> Just Keysym.Period
            SGE.Input.KeyboardkeyPower -> Just Keysym.Power
            SGE.Input.KeyboardkeySleep -> Just Keysym.Sleep
            SGE.Input.KeyboardkeyStop -> Just Keysym.Stop
            SGE.Input.KeyboardkeySysrq -> Just Keysym.SysReq
            SGE.Input.KeyboardkeyPrint -> Just Keysym.PrintScreen
            SGE.Input.KeyboardkeyUnlabeled -> Nothing
            SGE.Input.KeyboardkeyYen -> Nothing
            SGE.Input.KeyboardkeyUnknown -> Nothing

data InputEvent =
     KeyEvent (SGE.Input.KeyboardKey, SGE.Input.KeyState)
     | KeyRepeatEvent SGE.Input.KeyboardKey
     | CursorButtonEvent (SGE.Input.CursorButtonCode, SGE.Input.CursorButtonState, SGE.Pos.Pos)
     | CursorMoveEvent SGE.Pos.Pos
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

mouseAxisCallback :: InputsRef -> SGE.Input.MouseAxisCode -> Int -> IO ()
mouseAxisCallback inputs axis delta = appendInput inputs (MouseMoveEvent (axis, delta))

keyCallback :: InputsRef -> SGE.Input.KeyboardKey -> SGE.Input.KeyState -> IO ()
keyCallback inputs key status = appendInput inputs (KeyEvent (key, status))

keyRepeatCallback :: InputsRef -> SGE.Input.KeyboardKey -> IO ()
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
                    SGE.Input.withKeyCallback (keyboard p) (keyCallback inputs)
                        (SGE.Input.withKeyRepeatCallback (keyboard p) (keyRepeatCallback inputs)
                             (SGE.Input.withCursorButtonCallback (cursor p) (cursorButtonCallback inputs)
                                  (SGE.Input.withCursorMoveCallback (cursor p) (cursorMoveCallback inputs)
                                       (SGE.Input.withMouseAxisCallback (mouse p) (mouseAxisCallback inputs) (makeInputResults p inputs)))))
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
                                                                       (realToFrac (s ^. spriteRotation ^. to radToDeg ^. _Degrees)) tex

withSGEPlatform :: WindowTitle -> WindowSize -> MouseGrabMode -> (SGEPlatform -> IO ()) -> IO ()
withSGEPlatform windowTitle size cursorOption cb =
                SGE.Systems.with (T.unpack (unpackWindowTitle windowTitle)) (windowSizeToMaybe size) (makeCursorOption cursorOption) $ \system -> do
                                 context <- newIORef Nothing
                                 cb (SGEPlatform system context size)
