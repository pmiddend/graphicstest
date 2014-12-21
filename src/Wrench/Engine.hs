{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Engine(
    Picture(..)
  , wrenchPlay
  , RenderPositionMode(..)
  ) where

import           Control.Exception         (bracket, bracket_)
import           Control.Lens              ((&), (^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Iso          (Iso', from, iso)
import           Control.Lens.Setter       ((%~), (+~), (.~))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (liftM)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Loops       (unfoldM)
import           Data.Map.Strict           ((!))
import           Data.Monoid
import           Data.Text                 (Text, pack, unpack)
import qualified Graphics.UI.SDL.Color     as SDLC
import qualified Graphics.UI.SDL.Events as SDLE
import           Graphics.UI.SDL.Image     as SDLImage
import qualified Graphics.UI.SDL.Rect      as SDLRect
import qualified Graphics.UI.SDL.Render    as SDLR
import qualified Graphics.UI.SDL.TTF       as SDLTtf
import           Graphics.UI.SDL.TTF.Types (TTFFont)
import qualified Graphics.UI.SDL.Types     as SDLT
import qualified Graphics.UI.SDL.Video     as SDLV
import           Linear.Matrix             (M33, eye3, (!*), (!*!))
import           Linear.V2                 (V2 (..), _x, _y)
import           Linear.V3                 (V3 (..))
import           Numeric.Lens              (dividing)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.FloatType          (FloatType)
import           Wrench.ImageData          (AnimMap, SurfaceMap, readMediaFiles)
import           Wrench.Point              (Point)
import           Wrench.Rectangle          (Rectangle, rectLeftTop,
                                            rectangleDimensions,
                                            rectangleFromPoints)
import           Wrench.Time

type SpriteIdentifier = String

data RenderPositionMode = RenderPositionCenter | RenderPositionTopLeft deriving(Show,Eq)

data Picture = Line Point Point
             | Text String
             | Sprite SpriteIdentifier RenderPositionMode
             | Blank
             | InColor Color Picture
             | Translate Point Picture
             | Rotate Radians Picture
             | Scale Point Picture
             | Pictures [Picture]
             deriving(Show,Eq)

instance Monoid Picture where
  mempty = Blank
  mappend a b = Pictures [a,b]

type WindowTitle = String

type ViewportSize = Point

type MediaFilePath = FilePath

type BackgroundColor = Color

type StepsPerSecond = Int

withFontInit :: IO a -> IO a
withFontInit = bracket_ SDLTtf.init SDLTtf.quit

withImgInit :: IO a -> IO a
withImgInit = bracket_ (SDLImage.init [SDLImage.initPng]) SDLImage.quit

createFontTexture :: MonadIO m => SDLT.Renderer -> TTFFont -> Text -> Color -> m SDLT.Texture
createFontTexture renderer font text color = do
  surface <- liftIO $ SDLTtf.renderUTF8Blended font (unpack text) (color ^. from wrenchColor)
  liftIO $ SDLR.createTextureFromSurface renderer surface

floored :: (RealFrac a,Integral b) => Getter a b
floored = to floor

createAndRenderText :: MonadIO m => SDLT.Renderer -> TTFFont -> Text -> Color -> Point -> m SDLT.Texture
createAndRenderText renderer font text color position = do
  texture <- createFontTexture renderer font text color
  (width, height) <- liftIO $ SDLTtf.sizeText font (unpack text)
  liftIO $ SDLR.renderCopy renderer texture Nothing (Just $ SDLRect.Rect (position ^. _x ^. floored) (position ^. _y ^. floored) width height)
  return texture

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

-- toV3 :: Num a => V2 a -> V3 a
-- toV3 (V2 x y) = V3 x y 1

toV2 :: Getter (V3 a) (V2 a)
toV2 = to toV2'
  where toV2' (V3 x y _) = V2 x y

mkTranslation :: Point -> M33 FloatType
mkTranslation p = V3 (V3 1 0 (p ^. _x)) (V3 0 1 (p ^. _y)) (V3 0 0 1)

mkRotation :: FloatType -> M33 FloatType
mkRotation r = V3 (V3 cs (-sn) 0) (V3 sn cs 0) (V3 0 0 1)
  where cs = cos r
        sn = sin r

mkScale :: Point -> M33 FloatType
mkScale p = V3 (V3 (p ^. _x) 0 0) (V3 0 (p ^. _y) 0) (V3 0 0 1)

-- mkTransformation :: FloatType -> Point -> M33 FloatType
-- mkTransformation r p = mkTranslation p !*! mkRotation r

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

data RenderState = RenderState {   _rsTransformation :: M33 FloatType
                                 , _rsRenderer       :: SDLT.Renderer
                                 , _rsSurfaces       :: SurfaceMap
                                 , _rsFont           :: TTFFont
                                 , _rsColor          :: Color
                                 , _rsRotation       :: Radians
                               }

type RenderOutput = [SDLT.Texture]

$(makeLenses ''RenderState)

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

renderPicture :: RenderState -> Picture -> IO RenderOutput
renderPicture rs p = case p of
  Blank -> return []
  Line _ _ -> undefined
  Text s -> do texture <- createAndRenderText (rs ^. rsRenderer) (rs ^. rsFont) (pack s) (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2)
               return [texture]
  InColor color picture -> do liftIO $ setRenderDrawColor (rs ^. rsRenderer) color
                              renderPicture (rs & rsColor .~ color) picture
  Pictures ps -> concatMapM (renderPicture rs) ps
  Translate point picture -> renderPicture (rs & rsTransformation %~ (!*! mkTranslation point)) picture
  Rotate r picture -> renderPicture (rs & ((rsRotation +~ r) . (rsTransformation %~ (!*! mkRotation (r ^. getRadians))))) picture
  Scale s picture -> renderPicture (rs & rsTransformation %~ (!*! mkScale s)) picture
  Sprite identifier positionMode -> do
    let surfaces = rs ^. rsSurfaces
        m = rs ^. rsTransformation
        renderer = rs ^. rsRenderer
        (texture,rectangle) = surfaces ! pack identifier
        origin = (m !* V3 0 0 1) ^. toV2
        pos = case positionMode of
                        RenderPositionCenter -> origin - (rectangle ^. rectangleDimensions ^. dividing 2)
                        RenderPositionTopLeft -> origin
        rot = rs ^. rsRotation ^. degrees
        rotCenter = Nothing
        flipFlags = []
        srcRect = Just (rectangle ^. from wrenchRect)
        destRect = Just (rectangleFromPoints pos (pos + (rectangle ^. rectangleDimensions)) ^. from wrenchRect)
    liftIO $ SDLR.renderCopyEx renderer texture srcRect destRect (rot ^. getDegrees) rotCenter flipFlags
    return []

destroyTexture :: MonadIO m => SDLT.Texture -> m ()
destroyTexture t = liftIO $ SDLR.destroyTexture t

wrenchRender :: SDLT.Renderer -> SurfaceMap -> TTFFont -> BackgroundColor -> Picture -> IO ()
wrenchRender renderer surfaceMap font backgroundColor outerPicture = do
  setRenderDrawColor renderer backgroundColor
  renderClear renderer
  textures <- renderPicture (RenderState eye3 renderer surfaceMap font backgroundColor 0) outerPicture
  renderFinish renderer
  mapM_ destroyTexture textures

data MainLoopContext world = MainLoopContext { _mlImages          :: SurfaceMap
                                             , _mlFont            :: TTFFont
                                             , _mlAnims           :: AnimMap
                                             , _mlRenderer        :: SDLT.Renderer
                                             , _mlBackgroundColor :: BackgroundColor
                                             , _mlStepsPerSecond  :: StepsPerSecond
                                             , _mlWorldToPicture  :: world -> Picture
                                             , _mlEventHandler    :: SDLE.Event -> world -> world
                                             , _mlSimulationStep  :: TimeDelta -> world -> world
                                             }

{-
data KeyMovement = KeyUp | KeyDown
  deriving (Eq, Show)

data Event
  = Keyboard { keyMovement :: KeyMovement
             , keyRepeat :: Bool
             , keySym :: Keysym
             }
  | Quit
-}

$(makeLenses ''MainLoopContext)

pollEvents :: IO [SDLE.Event]
pollEvents = unfoldM SDLE.pollEvent

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
  wrenchRender (context ^. mlRenderer) (context ^. mlImages) (context ^. mlFont) (context ^. mlBackgroundColor) ((context ^. mlWorldToPicture) world)
  mainLoop context newTime newDelta newWorld

wrenchPlay :: WindowTitle -> ViewportSize -> MediaFilePath -> BackgroundColor -> world -> StepsPerSecond -> (world -> Picture) -> (SDLE.Event -> world -> world) -> (TimeDelta -> world -> world) -> IO ()
wrenchPlay windowTitle viewportSize mediaPath backgroundColor initialWorld stepsPerSecond worldToPicture eventHandler simulationStep =
  withFontInit $
    withImgInit $
      withWindow windowTitle $ \window ->
        withRenderer window (viewportSize ^. _x ^. floored) (viewportSize ^. _y ^. floored) $ \renderer -> do
          (images,anims) <- readMediaFiles renderer mediaPath
          stdfont <- SDLTtf.openFont (mediaPath <> "/stdfont.ttf") 15
          time <- getTicks
          mainLoop (MainLoopContext images stdfont anims renderer backgroundColor stepsPerSecond worldToPicture eventHandler simulationStep) time 0 initialWorld
