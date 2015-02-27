{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Engine(
    Picture(..)
  , wrenchPlay
  , RenderPositionMode(..)
  , Event(..)
  , ViewportSize
  , withPlatform
  , MediaPath(..)
  , BackgroundColor(..)
  , noBackgroundColor
  , StepsPerSecond(..)
  , ToPictureHandler(..)
  , EventHandler(..)
  , TickHandler(..)
  ) where

import           Control.Lens              ((&), (^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Setter       ((%~), (+~), (.~))
import           Control.Lens.TH           (makeLenses)
import           Linear.Matrix             (M33, eye3, (!*), (!*!))
import           Linear.V2                 (V2 (..), _x, _y)
import           Linear.V3                 (V3 (..))
import           Numeric.Lens              (dividing)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.FloatType          (FloatType)
import           Wrench.ImageData
import           Wrench.Picture
import           Wrench.Platform
import           Wrench.Point              (Point)
import           Wrench.Rectangle          (rectangleDimensions,
                                            rectangleFromPoints)
import           Wrench.RenderPositionMode
#if defined(USE_SGE)
import           Wrench.SGEPlatform
#else
import           Wrench.SDLPlatform
#endif
import           ClassyPrelude
import           Wrench.Time

newtype MediaPath = MediaPath { unpackMediaPath :: FilePath }
newtype BackgroundColor = BackgroundColor { unpackBackgroundColor :: Maybe Color }
newtype ToPictureHandler world = ToPictureHandler { unpackToPictureHandler :: (ViewportSize -> world -> Picture) }
newtype EventHandler world = EventHandler { unpackEventHandler :: (Event -> world -> world) }
newtype TickHandler world = TickHandler { unpackTickHandler :: (TimeDelta -> world -> world) }

noBackgroundColor :: BackgroundColor
noBackgroundColor = BackgroundColor Nothing

newtype StepsPerSecond = StepsPerSecond { unpackStepsPerSecond :: Int }

type ViewportSize = Point

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

data RenderState p = RenderState { _rsTransformation :: M33 FloatType
                                 , _rsPlatform       :: p
                                 , _rsSurfaceData    :: SurfaceMap (PlatformImage p)
                                 , _rsFont           :: PlatformFont p
                                 , _rsColor          :: Color
                                 , _rsRotation       :: Radians
                                 }

$(makeLenses ''RenderState)

renderPicture :: Platform p => RenderState p -> Picture -> IO ()
renderPicture rs p = case p of
  Blank -> return ()
  Line _ _ -> undefined
  Text s -> renderText (rs ^. rsPlatform) (rs ^. rsFont) (s) (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2)
  InColor color picture -> renderPicture (rs & rsColor .~ color) picture
  Pictures ps -> mapM_ (renderPicture rs) ps
  Translate point picture -> renderPicture (rs & rsTransformation %~ (!*! mkTranslation point)) picture
  Rotate r picture -> renderPicture (rs & ((rsRotation +~ r) . (rsTransformation %~ (!*! mkRotation (r ^. getRadians))))) picture
  Scale s picture -> renderPicture (rs & rsTransformation %~ (!*! mkScale s)) picture
  Sprite identifier positionMode -> do
    let (image,srcRect) = findSurfaceUnsafe (rs ^. rsSurfaceData) identifier
        m = rs ^. rsTransformation
        origin = (m !* V3 0 0 1) ^. toV2
        pos = case positionMode of
          RenderPositionCenter -> origin - (srcRect ^. rectangleDimensions ^. dividing 2)
          RenderPositionTopLeft -> origin
        rot = rs ^. rsRotation
        destRect = (rectangleFromPoints pos (pos + (srcRect ^. rectangleDimensions)))
    renderDrawSprite (rs ^. rsPlatform) image srcRect destRect rot

wrenchRender :: Platform p => p -> SurfaceMap (PlatformImage p) -> PlatformFont p -> Maybe Color -> Picture -> IO ()
wrenchRender platform surfaceMap font backgroundColor outerPicture = do
  renderBegin platform
  maybe (return ()) (renderClear platform) backgroundColor
  renderPicture (RenderState eye3 platform surfaceMap font (fromMaybe colorsWhite backgroundColor) 0) outerPicture
  renderFinish platform

data MainLoopContext p world = MainLoopContext { _mlPlatform        :: p
                                               , _mlSurfaceData     :: SurfaceMap (PlatformImage p)
                                               , _mlFont            :: PlatformFont p
                                               , _mlBackgroundColor :: Maybe Color
                                               , _mlStepsPerSecond  :: Int
                                               , _mlWorldToPicture  :: ViewportSize -> world -> Picture
                                               , _mlEventHandler    :: Event -> world -> world
                                               , _mlSimulationStep  :: TimeDelta -> world -> world
                                               }


$(makeLenses ''MainLoopContext)

type PreviousTime = TimeTicks
type SinceLastSimulation = TimeDelta

splitDelta :: TimeDelta -> TimeDelta -> (Int,TimeDelta)
splitDelta maxDelta n = let iterations = floor $ toSeconds n / toSeconds maxDelta
                        in (iterations,n - fromIntegral iterations * maxDelta)

mainLoop :: Platform p => MainLoopContext p world -> PreviousTime -> SinceLastSimulation -> world -> IO ()
mainLoop context prevTime prevDelta world = do
  let platform = context ^. mlPlatform
  mappedEvents <- pollEvents platform
  unless (any isQuitEvent mappedEvents) $ do
    let worldAfterEvents = foldr (context ^. mlEventHandler) world mappedEvents
    newTime <- getTicks
    let timeDelta = newTime `tickDelta` prevTime
        maxDelta = fromSeconds . recip . fromIntegral $ context ^. mlStepsPerSecond
        (simulationSteps,newDelta) = splitDelta maxDelta (timeDelta + prevDelta)
    let newWorld = foldr (context ^. mlSimulationStep) worldAfterEvents (replicate simulationSteps maxDelta :: [TimeDelta])
    ws <- viewportSize platform
    wrenchRender
        platform
        (context ^. mlSurfaceData)
        (context ^. mlFont)
        (context ^. mlBackgroundColor)
        ((context ^. mlWorldToPicture) ws world)
    mainLoop context newTime newDelta newWorld

#if defined(USE_SGE)
withPlatform :: WindowTitle -> FilePath -> (SGEPlatform -> IO ()) -> IO ()
withPlatform = withSGEPlatform
#else
withPlatform :: WindowTitle -> (SDLPlatform -> IO ()) -> IO ()
withPlatform = withSDLPlatform
#endif

wrenchPlay :: Platform p =>
              p ->
              MediaPath ->
              BackgroundColor ->
              world ->
              StepsPerSecond ->
              ToPictureHandler world ->
              EventHandler world ->
              TickHandler world ->
              IO ()
wrenchPlay platform mediaPath backgroundColor initialWorld stepsPerSecond worldToPicture eventHandler simulationStep = do
  time <- getTicks
  surfaceData <- readMediaFiles (loadImage platform) (unpackMediaPath mediaPath)
  font <- loadFont platform ((unpackMediaPath mediaPath) </> "stdfont.ttf") 15
  let loopContext = MainLoopContext
                    platform
                    (fst surfaceData)
                    font
                    (unpackBackgroundColor backgroundColor)
                    (unpackStepsPerSecond stepsPerSecond)
                    (unpackToPictureHandler worldToPicture)
                    (unpackEventHandler eventHandler)
                    (unpackTickHandler simulationStep)
  mainLoop loopContext time 0 initialWorld
