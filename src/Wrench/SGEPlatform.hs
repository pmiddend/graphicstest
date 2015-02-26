{-# LANGUAGE TemplateHaskell            #-}

module Wrench.SGEPlatform(
       SGEPlatform,
       withSGEPlatform
)
where

import           ClassyPrelude
import           Control.Lens ((^.), _1)
import           Control.Lens.TH (makeLenses)
import qualified Data.Text as T ( unpack )
import           GHC.Float ( double2Float )
import           Linear.V2(_x, _y, V2(..))
import qualified SGE.Font ( ObjectPtr, draw, withFont )
import qualified SGE.Image ( RGBA, makeRGBA )
import qualified SGE.Input ( KeyboardPtr, KeyboardKey(..), KeyState(..), withKeyCallback )
import qualified SGE.Renderer ( ContextPtr, DevicePtr, PlanarTexturePtr, beginRenderingExn, clear, endRenderingAndDestroy, onscreenTarget, onscreenTargetDim, planarTextureFromPathExn )
import qualified SGE.Sprite ( Object(..), draw )
import qualified SGE.Systems ( InstancePtr, fontSystem, imageSystem, keyboard, renderer, windowSystem, with )
import qualified SGE.Texture ( PartPtr, partRawExn )
import qualified SGE.Types ( Pos(..), Dim(..), dimW, dimH )
import qualified SGE.Window ( SystemPtr, poll )
import Wrench.Angular ( getRadians )
import Wrench.Color ( Color, colorAlpha, colorBlue, colorGreen, colorRed )
import Wrench.Event ( Event(..) )
import Wrench.ImageData ( AnimMap, SurfaceData, SurfaceMap, readMediaFiles )
import Wrench.KeyMovement ( KeyMovement(..) )
import qualified Wrench.Keysym as Keysym ( Keysym(..) )
import Wrench.Platform ( Platform(..), WindowTitle )
import Wrench.Point ( Point )
import Wrench.Rectangle ( rectLeftTop, rectangleDimensions )
import Wrench.SpriteIdentifier ( SpriteIdentifier )

type SpriteInfo = (SGE.Renderer.PlanarTexturePtr, SGE.Texture.PartPtr)

data SGEPlatform = SGEPlatform {
     _sgepSystem :: SGE.Systems.InstancePtr
   , _sgepSurfaceMap :: (SurfaceMap SpriteInfo , AnimMap)
   , _sgepFont :: SGE.Font.ObjectPtr
   , _sgepContext :: IORef (Maybe SGE.Renderer.ContextPtr)
   }

$(makeLenses ''SGEPlatform)

type SpriteData = SurfaceData SpriteInfo

keyboard :: SGEPlatform -> SGE.Input.KeyboardPtr
keyboard p = SGE.Systems.keyboard (p ^. sgepSystem)

renderer :: SGEPlatform -> SGE.Renderer.DevicePtr
renderer p = SGE.Systems.renderer (p ^. sgepSystem)

window :: SGEPlatform -> SGE.Window.SystemPtr
window p = SGE.Systems.windowSystem (p ^. sgepSystem)

contextError :: SGEPlatform -> IO SGE.Renderer.ContextPtr
contextError p = do
             context <- readIORef (p ^. sgepContext)
             case context of
                  Just c -> return c
                  Nothing -> error "No active render context"

toSGEColor :: Color -> SGE.Image.RGBA
toSGEColor c = SGE.Image.makeRGBA (c ^. colorRed) (c ^. colorBlue) (c ^. colorGreen) (c ^. colorAlpha)

toSGEPos :: Point -> SGE.Types.Pos
toSGEPos p = SGE.Types.Pos (round (p ^._x), round (p ^._y))

toSGEDim :: Point -> SGE.Types.Dim
toSGEDim p = SGE.Types.Dim (round (p ^._x), round (p ^._y))

fromSGEDim :: SGE.Types.Dim -> Point
fromSGEDim d = V2 (fromIntegral (SGE.Types.dimW d)) (fromIntegral (SGE.Types.dimH d))

makeKeyMovement :: SGE.Input.KeyState -> KeyMovement
makeKeyMovement s = case s of
                SGE.Input.KeystatePressed -> KeyDown
                SGE.Input.KeystateReleased -> KeyUp

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

makeKeyEvent :: (SGE.Input.KeyboardKey, SGE.Input.KeyState) -> Maybe Event
makeKeyEvent (k, s) = do
             keysym <- makeKeySym k
             return $ Keyboard {
                 keyMovement = makeKeyMovement s
             ,   keyRepeat = False -- TODO
             ,   keySym = keysym
             }

type KeyboardInput = (SGE.Input.KeyboardKey, SGE.Input.KeyState)
type Inputs = [KeyboardInput]
type InputsRef = IORef Inputs

makeInputResults :: SGEPlatform -> InputsRef -> IO [Event]
makeInputResults p inputs = do
                 result <- SGE.Window.poll (window p)
                 if result then do
                    curInputs <- readIORef inputs
                    return $ mapMaybe makeKeyEvent curInputs
                 else
                    return [Quit]

keyCallback :: InputsRef -> SGE.Input.KeyboardKey -> SGE.Input.KeyState -> IO ()
keyCallback inputs key status = do
            old <- readIORef inputs
            writeIORef inputs ((key, status) : old)

lookupSpriteError :: SGEPlatform -> SpriteIdentifier -> IO SpriteData
lookupSpriteError p identifier =
                  case identifier `lookup` ( p ^. sgepSurfaceMap ^. _1 )  of
                       Nothing -> error ("Couldn't find image \"" ++ (T.unpack identifier) ++ "\"")
                       Just x -> return x

instance Platform SGEPlatform where
         pollEvents p = do
                    inputs <- newIORef []
                    SGE.Input.withKeyCallback (keyboard p) (keyCallback inputs) (makeInputResults p inputs)
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
         renderText p text color pos = do
                      context <- contextError p
                      SGE.Font.draw (renderer p) context (p ^. sgepFont) (unpack text) (toSGEPos pos) (toSGEColor color)
         spriteDimensions p identifier = do
                          (_, rect) <- lookupSpriteError p identifier
                          return rect
         viewportSize p = do
                      dim <- SGE.Renderer.onscreenTargetDim (SGE.Renderer.onscreenTarget (renderer p))
                      return $ fromSGEDim dim
         renderDrawSprite p identifier dest rot = do
                          ((_, tex), _) <- lookupSpriteError p identifier
                          context <- contextError p
                          SGE.Sprite.draw (renderer p) context [SGE.Sprite.Object (toSGEPos (dest ^. rectLeftTop)) (toSGEDim (dest ^. rectangleDimensions)) (double2Float (rot ^. getRadians)) tex]

-- FIXME: This is not exception-safe
loadFile :: SGE.Systems.InstancePtr -> FilePath -> IO SpriteInfo
loadFile system path = do
         texture <- SGE.Renderer.planarTextureFromPathExn (SGE.Systems.renderer system) (SGE.Systems.imageSystem system) (fpToString path)
         part <- SGE.Texture.partRawExn texture
         return (texture, part)

withSGEPlatform :: WindowTitle -> FilePath -> (SGEPlatform -> IO ()) -> IO ()
withSGEPlatform windowTitle mediaPath cb =
                SGE.Systems.with (T.unpack windowTitle) $ \system ->
                SGE.Font.withFont (SGE.Systems.fontSystem system) $ \font -> do
                                  surfaceData <- readMediaFiles (loadFile system) mediaPath
                                  context <- newIORef Nothing
                                  cb (SGEPlatform system surfaceData font context)
