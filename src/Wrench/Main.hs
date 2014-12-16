{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Exception         (bracket, bracket_)
import           Control.Lens              ((^.))
import           Control.Lens.TH           (makeLenses)
import           Data.Map.Strict           (Map)
import           Data.Monoid
import           Data.Word                 (Word8)
import           Graphics.UI.SDL.Image     as SDLImage
import qualified Graphics.UI.SDL.Rect      as SDLRect
import qualified Graphics.UI.SDL.Render    as SDLR
import Wrench.Time
import qualified Graphics.UI.SDL.TTF       as SDLTtf
import           Control.Monad.Loops    (unfoldM)
import           Graphics.UI.SDL.Events (Event (..), Event (..), EventData (..),
                                         KeyMovement (..), pollEvent)
import qualified Graphics.UI.SDL.TTF       as SDLTtf
import           Graphics.UI.SDL.TTF.Types (TTFFont)
import qualified Graphics.UI.SDL.Types     as SDLT
import qualified Graphics.UI.SDL.Types     as SDLT
import qualified Graphics.UI.SDL.Video     as SDLV
import           Linear.V2                 (V2(..), _x, _y)
import           Linear.V3                 (V3(..))
import           Linear.Matrix             (eye3,(!*!),mkTransformation,M33)
import           Wrench.FloatType          (FloatType)
import           Wrench.ImageData          (readMediaFiles,SurfaceMap,AnimMap)
import           Wrench.Point              (Point)

type SpriteIdentifier = String

data RenderPositionMode = RenderPositionCenter | RenderPositionTopLeft deriving(Show,Eq)

data Color = Color { _colorRed   :: Word8
                   , _colorGreen :: Word8
                   , _colorBlue  :: Word8
                   , _colorAlpha :: Word8
                   }
                   deriving(Show,Eq)

$(makeLenses ''Color)

data Picture = Line Point Point
             | Text String
             | Sprite SpriteIdentifier RenderPositionMode
             | Blank
             | InColor Color Picture
             | Translate Point Picture
             | Rotate FloatType Picture
             | Scale Point Picture
             | Pictures [Picture]
             deriving(Show,Eq)

instance Monoid Picture where
  mempty = Blank
  mappend a b = Pictures [a,b]

data Rectangle = Rectangle { _rectLeftTop     :: Point
                           , _rectRightBottom :: Point
                           } deriving(Show,Eq)

$(makeLenses ''Rectangle)

type WindowTitle = String

type ViewportSize = Point

type MediaFilePath = FilePath

type BackgroundColor = Color

type StepsPerSecond = Int

withFontInit :: IO a -> IO a
withFontInit = bracket_ SDLTtf.init SDLTtf.quit

withImgInit :: IO a -> IO a
withImgInit = bracket_ (SDLImage.init [SDLImage.initPng]) SDLImage.quit


withWindow :: String -> (SDLT.Window -> IO a) -> IO a
withWindow title callback = SDLV.withWindow title (SDLT.Position SDLV.windowPosUndefined SDLV.windowPosUndefined) (SDLT.Size (fromIntegral screenAbsoluteWidth) (fromIntegral screenAbsoluteHeight)) windowFlags $ \win -> callback win
  where screenAbsoluteWidth,screenAbsoluteHeight :: Int
        screenAbsoluteWidth = 0
        screenAbsoluteHeight = 0
        windowFlags :: [SDLT.WindowFlag]
        windowFlags = [SDLT.WindowResizable]

withRenderer :: SDLT.Window -> Int -> Int -> (SDLT.Renderer -> IO a) -> IO a
withRenderer window screenWidth screenHeight callback =
  let acquireResource = SDLR.createRenderer window SDLT.FirstSupported []
      releaseResource = SDLR.destroyRenderer
  in bracket acquireResource releaseResource $ \renderer -> do
    SDLR.renderSetLogicalSize renderer (fromIntegral screenWidth) (fromIntegral screenHeight)
    callback renderer

setRenderDrawColor :: SDLT.Renderer -> Color -> IO ()
setRenderDrawColor renderer c = do
  _ <- SDLR.setRenderDrawColor renderer (c ^. colorRed) (c ^. colorGreen) (c ^. colorBlue) (c ^. colorAlpha)
  return ()

renderClear :: SDLT.Renderer -> IO ()
renderClear renderer = do
  _ <- SDLR.renderClear renderer
  return ()

renderFinish :: SDLT.Renderer -> IO ()
renderFinish = SDLR.renderPresent

toV3 :: Num a => V2 a -> V3 a
toV3 (V2 x y) = V3 x y 1

mkTranslation :: Point -> M33 FloatType
mkTranslation p = V3 (V3 1 0 (p ^. _x)) (V3 0 1 (p ^. _y)) (V3 0 0 1)

mkRotation :: FloatType -> M33 FloatType
mkRotation r = V3 (V3 cs (-sn) 0) (V3 sn cs 0) (V3 0 0 1)
  where cs = cos r
        sn = sin r

mkScale :: Point -> M33 FloatType
mkScale p = V3 (V3 (p ^. _x) 0 0) (V3 0 (p ^. _y) 0) (V3 0 0 1)

mkTransformation :: FloatType -> Point -> M33 FloatType
mkTransformation r p = mkTranslation p !*! mkRotation r

data Radians = Radians { getRadians :: FloatType } deriving(Show,Eq)

toDegrees :: Radians -> FloatType
toDegrees = (/pi).(*180).getRadians

renderSprite :: SDLT.Renderer -> SDLT.Texture -> SDLRect.Rect -> SDLRect.Rect -> Maybe SDLRect.Point -> Radians -> IO ()
renderSprite r t srcrect dstrect rotationCenter rotation = SDLR.renderCopyEx r t (Just srcrect) (Just dstrect) (toDegrees rotation) rotationCenter []

blitRescale :: SurfaceData -> RectInt -> Maybe PointInt -> Radians -> SDLT.Renderer -> IO ()
blitRescale (srcSurface,srcRect) destRect rotationCenter rotation renderer = do
  renderSprite renderer srcSurface (toSDLRect srcRect) (toSDLRect destRect) (toSDLPoint <$> rotationCenter) rotation

blitSameSize :: SurfaceData -> PointInt -> Maybe PointInt -> RadiansReal -> SDLT.Renderer -> IO ()
blitSameSize sd@(_,srcRect) pos = blitRescale sd (Rect pos (pos + rectDimensions srcRect))

blitAt :: SpriteIdentifier -> Point -> Maybe Point -> FloatType -> RenderPositionMode -> IO ()
blitAt image pos rotCenter angle mode = do
  renderer <- use gdRenderer
  surfaces <- use gdSurfaces
  let realPos = case mode of
          RenderPositionCenter -> pos - ((`div` 2) <$> (rectDimensions $ snd $ imageData))
          RenderPositionTopLeft -> pos
      imageData = surfaces ! image
  liftIO $ SDLH.blitSameSize imageData realPos rotCenter angle renderer

wrenchRender :: SDLT.Renderer -> SurfaceMap -> BackgroundColor -> Picture -> IO ()
wrenchRender renderer surfaceMap backgroundColor picture = do
  setRenderDrawColor renderer backgroundColor
  renderClear renderer
  _ <- renderPicture eye3 picture
  return ()
  where renderPicture :: M33 FloatType -> Picture -> IO (M33 FloatType)
        renderPicture m p = case picture of
          Blank -> return m
          Pictures ps -> undefined
          Translate point picture -> renderPicture (m !*! mkTranslation point) picture
          Rotate r picture -> renderPicture (m !*! mkRotation r) picture
          Scale p picture -> renderPicture (m !*! mkScale p) picture
          Sprite identifier positionMode ->
            let (texture,rectangle) = surfaceMap ! identifier
                origin = m !* (V3 0 0 1)
                pos = case positionMode of
                        RenderPositionCenter -> origin - ((`div` 2) <$> (rectangleDimensions rectangle))
                        RenderPositionTopLeft -> origin
            in SDLR.renderCopyEx r t (Just srcrect) (Just dstrect) (toDegrees rotation) rotationCenter [] >> return m



data MainLoopContext world = MainLoopContext { _mlImages :: SurfaceMap
                                             , _mlFont :: TTFFont
                                             , _mlAnims :: AnimMap
                                             , _mlRenderer :: SDLT.Renderer
                                             , _mlBackgroundColor :: BackgroundColor
                                             , _mlStepsPerSecond :: StepsPerSecond
                                             , _mlWorldToPicture :: world -> Picture
                                             , _mlEventHandler :: Event -> world -> world
                                             , _mlSimulationStep :: TimeDelta -> world -> world
                                             }

$(makeLenses ''MainLoopContext)

pollEvents :: IO [Event]
pollEvents = unfoldM pollEvent

type PreviousTime = TimeTicks
type SinceLastSimulation = TimeDelta

splitDelta :: TimeDelta -> TimeDelta -> (Int,TimeDelta)
splitDelta maxDelta n = let iterations = floor $ toSeconds n / toSeconds maxDelta
                        in (iterations,n - fromIntegral iterations * maxDelta)

mainLoop :: MainLoopContext world -> PreviousTime -> SinceLastSimulation -> world -> IO ()
mainLoop context prevTime prevDelta world = do
  events <- pollEvents
  let worldAfterEvents = foldr (context ^. mlEventHandler) world events
  newTime <- getTicks
  let timeDelta = newTime `tickDelta` prevTime
      maxDelta = fromSeconds . recip . fromIntegral $ context ^. mlStepsPerSecond
      (simulationSteps,newDelta) = splitDelta (timeDelta + prevDelta) maxDelta
  let newWorld = foldr (context ^. mlSimulationStep) worldAfterEvents (replicate simulationSteps maxDelta)
  wrenchRender (context ^. mlRenderer) (context ^. mlSurfaceMap) (context ^. mlBackgroundColor) (context ^. mlSurfaceMap) ((context ^. mlWorldToPicture) world)
  mainLoop context newTime newDelta newWorld


wrenchPlay :: WindowTitle -> ViewportSize -> MediaFilePath -> BackgroundColor -> world -> StepsPerSecond -> (world -> Picture) -> (Event -> world -> world) -> (TimeDelta -> world -> world) -> IO ()
wrenchPlay windowTitle viewportSize mediaPath backgroundColor initialWorld stepsPerSecond worldToPicture eventHandler simulationStep =
  withFontInit $
    withImgInit $
      withWindow windowTitle $ \window ->
        withRenderer window (floor (viewportSize ^. _x)) (floor (viewportSize ^. _y)) $ \renderer -> do
          (images,anims) <- readMediaFiles renderer mediaPath
          stdfont <- SDLTtf.openFont (mediaPath <> "/stdfont.ttf") 15
          time <- getTicks
          mainLoop (MainLoopContext images stdfont anims renderer backgroundColor stepsPerSecond worldToPicture eventHandler simulationStep) time 0 initialWorld

main :: IO ()
main = return ()
