{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Engine(
    wrenchPlay
  , Event(..)
  , ViewportSize
  , withPlatform
  , MediaPath(..)
  , BackgroundColor(..)
  , noBackgroundColor
  , StepsPerSecond(..)
  , ToPictureHandler(..)
  , EventHandler(..)
  , ImageSizeGetter
  , wrenchRender
  , PlatformBackend
  ) where

import           Control.Lens              ((&), (^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Setter       ((%~), (+~), (.~))
import           Control.Lens.TH           (makeLenses)
import           Linear.Matrix             (M33, (!*), (!*!))
import           Linear.V2                 (V2 (..), _x, _y)
import           Linear.V3                 (V3 (..))
import           Numeric.Lens              (dividing)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.Rectangle
import           Wrench.FloatType          (FloatType)
import           Wrench.ImageData
import           Wrench.Internal.Picture
import           Wrench.Platform
import Wrench.SpriteIdentifier
import           Wrench.Point              (Point)
import           Wrench.RenderPositionMode
#if defined(USE_SGE)
import           Wrench.SGEPlatform
#else
#if defined(USE_OLDSDL)
import           Wrench.SDLOldPlatform
#else
import           Wrench.SDLPlatform
#endif
#endif
import           ClassyPrelude hiding(FilePath,(</>))
import System.FilePath
import           Wrench.Time
import Wrench.List(concatMapM)

-- TODO: Use Linear.Matrix.identity
eye3 :: Num a => M33 a
eye3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

type ImageSizeGetter = SpriteIdentifier -> Rectangle
newtype MediaPath = MediaPath { unpackMediaPath :: FilePath }
newtype BackgroundColor = BackgroundColor { unpackBackgroundColor :: Maybe Color }
type ToPictureHandlerImpl world = ViewportSize -> world -> Picture
newtype ToPictureHandler world = ToPictureHandler { unpackToPictureHandler :: ImageSizeGetter -> ToPictureHandlerImpl world }
newtype EventHandler world = EventHandler { unpackEventHandler :: Event -> world -> world }

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

data RenderState i f = RenderState { _rsTransformation :: M33 FloatType
                                   , _rsSurfaceData    :: SurfaceMap i
                                   , _rsFont           :: f
                                   , _rsColor          :: Color
                                   , _rsRotation       :: Radians
                                 }

$(makeLenses ''RenderState)

data RenderOperation image font = RenderOperationSprite (SpriteInstance image)
                                | RenderOperationText (TextInstance font)

opToSprite :: RenderOperation image font -> SpriteInstance image
opToSprite (RenderOperationSprite s) = s
opToSprite (RenderOperationText _) = error "Cannot extract sprite from text"

opToText :: RenderOperation image font -> TextInstance font
opToText (RenderOperationSprite _) = error "Cannot extract text from sprite"
opToText (RenderOperationText s) = s

executeOperationBatch :: Platform p => p -> [ RenderOperation (PlatformImage p) (PlatformFont p) ] -> IO ()
executeOperationBatch p ss@( RenderOperationSprite _ : _ ) = renderSprites p (map opToSprite ss)
executeOperationBatch p ss@( RenderOperationText _ : _ ) = renderText p (map opToText ss)
executeOperationBatch _ [] = return ()

equalOperation :: RenderOperation image font -> RenderOperation image font -> Bool
equalOperation (RenderOperationSprite _) (RenderOperationSprite _) = True
equalOperation (RenderOperationText _) (RenderOperationText _) = True
equalOperation _ _ = False

renderPicture :: RenderState font image -> Picture -> IO [RenderOperation font image]
renderPicture rs p = case p of
  Blank -> return []
  Line _ _ -> return [] -- TODO
  --Text s -> renderText (rs ^. rsPlatform) (rs ^. rsFont) (s) (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2)
  Text s -> return [RenderOperationText (TextInstance (rs ^. rsFont) s (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2) )]
  InColor color picture -> renderPicture (rs & rsColor .~ color) picture
  Pictures ps -> concatMapM (renderPicture rs) ps
  Translate point picture -> renderPicture (rs & rsTransformation %~ (!*! mkTranslation point)) picture
  Rotate r picture -> renderPicture (rs & ((rsRotation +~ r) . (rsTransformation %~ (!*! mkRotation (r ^. getRadians))))) picture
  Scale s picture -> renderPicture (rs & rsTransformation %~ (!*! mkScale s)) picture
  Sprite identifier positionMode resampledSize -> do
    let (image,srcRect) = findSurfaceUnsafe (rs ^. rsSurfaceData) identifier
        m = rs ^. rsTransformation
        origin = (m !* V3 0 0 1) ^. toV2
        spriteDim = fromMaybe (srcRect ^. rectDimensions) resampledSize
        pos = case positionMode of
          RenderPositionCenter -> origin - (spriteDim ^. dividing 2)
          RenderPositionTopLeft -> origin
        rot = rs ^. rsRotation
        destRect = rectFromPoints pos (pos + spriteDim)
    return [RenderOperationSprite (SpriteInstance image srcRect destRect rot)]

wrenchRender :: Platform p => p -> SurfaceMap (PlatformImage p) -> PlatformFont p -> Maybe Color -> Picture -> IO ()
wrenchRender platform surfaceMap font backgroundColor outerPicture = do
  renderBegin platform
  maybe (return ()) (renderClear platform) backgroundColor
  operations <- renderPicture (RenderState eye3 surfaceMap font (fromMaybe colorsWhite backgroundColor) 0) outerPicture
  mapM_ (executeOperationBatch platform) (groupBy equalOperation operations)
  renderFinish platform

data MainLoopContext p world = MainLoopContext { _mlPlatform        :: p
                                               , _mlSurfaceData     :: SurfaceMap (PlatformImage p)
                                               , _mlFont            :: PlatformFont p
                                               , _mlBackgroundColor :: Maybe Color
                                               , _mlStepsPerSecond  :: Int
                                               , _mlWorldToPicture  :: ViewportSize -> world -> Picture
                                               , _mlEventHandler    :: Event -> world -> world
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
    newTime <- getTicks
    let timeDelta = newTime `tickDelta` prevTime
        maxDelta = fromSeconds . recip . fromIntegral $ context ^. mlStepsPerSecond
        (simulationSteps,newDelta) = splitDelta maxDelta (timeDelta + prevDelta)
    let newWorld = foldr (context ^. mlEventHandler) world (mappedEvents <> replicate simulationSteps (Tick maxDelta))
    -- TODO: Delete if finished
    --let newWorld = foldr (context ^. mlSimulationStep) worldAfterEvents (replicate simulationSteps maxDelta :: [TimeDelta])
    ws <- viewportSize platform
    wrenchRender
        platform
        (context ^. mlSurfaceData)
        (context ^. mlFont)
        (context ^. mlBackgroundColor)
        ((context ^. mlWorldToPicture) ws world)
    mainLoop context newTime newDelta newWorld

#if defined(USE_SGE)
type PlatformBackend = SGEPlatform
#else
type PlatformBackend = SDLPlatform
#endif

withPlatform :: WindowTitle -> WindowSize -> (PlatformBackend -> IO ()) -> IO ()
#if defined(USE_SGE)
withPlatform = withSGEPlatform
#else
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
              IO ()
wrenchPlay platform mediaPath backgroundColor initialWorld stepsPerSecond worldToPicture eventHandler = do
  time <- getTicks
  surfaceData <- readMediaFiles (loadImage platform) (unpackMediaPath mediaPath)
  let imageSizeGetter spriteName = snd (fst surfaceData `findSurfaceUnsafe` spriteName)
  font <- loadFont platform (unpackMediaPath mediaPath </> "stdfont.ttf") 15
  let loopContext = MainLoopContext
                    platform
                    (fst surfaceData)
                    font
                    (unpackBackgroundColor backgroundColor)
                    (unpackStepsPerSecond stepsPerSecond)
                    (unpackToPictureHandler worldToPicture imageSizeGetter)
                    (unpackEventHandler eventHandler)
  mainLoop loopContext time 0 initialWorld
