{-# LANGUAGE TemplateHaskell            #-}

module Wrench.SGEPlatform(
       SGEPlatform,
       withSGEPlatform
)
where

import           ClassyPrelude
import           Control.Lens ((^.), _1)
import           Control.Lens.TH (makeLenses)
import           Data.Maybe ( Maybe(..) )
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
import qualified Wrench.Keycode as Keycode ( Keycode(..) )
import Wrench.Keysym ( Keysym(..) )
import Wrench.Platform ( Platform(..), WindowTitle )
import Wrench.Point ( Point )
import Wrench.Rectangle ( Rectangle, rectLeftTop, rectangleDimensions )
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
                  Just context -> return context
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

makeKeyCode :: SGE.Input.KeyboardKey -> Keycode.Keycode
makeKeyCode s = case s of
            -- TODO
            SGE.Input.KeyboardkeyEscape -> Keycode.Escape
            _ -> error "Invalid key"

makeKeySym :: SGE.Input.KeyboardKey -> Keysym
makeKeySym s = Keysym (makeKeyCode s) 0 -- TODO

makeKeyEvent :: (SGE.Input.KeyboardKey, SGE.Input.KeyState) -> Event
makeKeyEvent (k, s) = Keyboard {
                 keyMovement = makeKeyMovement s
             ,   keyRepeat = False -- TODO
             ,   keySym = makeKeySym k
             }

type KeyboardInput = (SGE.Input.KeyboardKey, SGE.Input.KeyState)
type Inputs = [KeyboardInput]
type InputsRef = IORef Inputs

makeInputResults :: SGEPlatform -> InputsRef -> IO [Event]
makeInputResults p inputs = do
                 result <- SGE.Window.poll (window p)
                 if result then do
                    curInputs <- readIORef inputs
                    return $ map makeKeyEvent curInputs
                 else
                    return [Quit]

keyCallback :: InputsRef -> SGE.Input.KeyboardKey -> SGE.Input.KeyState -> IO ()
keyCallback inputs key status = do
            old <- readIORef inputs
            writeIORef inputs ((key, status) : old)

lookupSpriteError :: SGEPlatform -> SpriteIdentifier -> IO SpriteData
lookupSpriteError p id =
                  case id `lookup` ( p ^. sgepSurfaceMap ^. _1 )  of
                       Nothing -> error ("Couldn't find image \"" ++ (T.unpack id) ++ "\"")
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
         spriteDimensions p id = do
                          (_, rect) <- lookupSpriteError p id
                          return rect
         viewportSize p = do
                      dim <- SGE.Renderer.onscreenTargetDim (SGE.Renderer.onscreenTarget (renderer p))
                      return $ fromSGEDim dim
         renderDrawSprite p id dest rot = do
                          ((_, tex), _) <- lookupSpriteError p id
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
