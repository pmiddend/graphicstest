{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Engine(
    Picture(..)
  , wrenchPlay
  , RenderPositionMode(..)
  , Event(..)
  , Keysym(..)
  , SpriteIdentifier
  , KeyMovement(..)
  , ViewportSize
  ) where

import           Control.Exception         (bracket, bracket_)
import           Control.Lens              ((&), (^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Iso          (Iso', from, iso)
import           Control.Lens.Setter       ((%~), (+~), (.~))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (liftM, unless)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Loops       (unfoldM)
import           Data.Map.Strict           (lookup)
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Monoid
import           Data.Text                 (Text, pack, unpack)
import           Data.Word                 (Word16)
import           Linear.Matrix             (M33, eye3, (!*), (!*!))
import           Linear.V2                 (V2 (..), _x, _y)
import           Linear.V3                 (V3 (..))
import           Numeric.Lens              (dividing)
import           Prelude                   hiding (lookup)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.FloatType          (FloatType)
import           Wrench.SDLPlatform
import           Wrench.ImageData          (AnimMap, SurfaceMap, readMediaFiles)
import qualified Wrench.Keycode            as Wrenchkeycode
import           Wrench.Point              (Point)
import           Wrench.Rectangle          (Rectangle, rectLeftTop,
                                            rectangleDimensions,
                                            rectangleFromPoints)
import           Wrench.Time

type ViewportSize = Point

type MediaFilePath = FilePath

type StepsPerSecond = Int

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
                                 , _rsColor          :: Color
                                 , _rsRotation       :: Radians
                                 }

$(makeLenses ''RenderState)

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

renderPicture :: Platform p => RenderState p -> Picture -> IO ()
renderPicture rs p = case p of
  Blank -> return []
  Line _ _ -> undefined
  Text s -> renderText (rs ^. rsPlatform) (pack s) (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2)
  InColor color picture -> do renderSetDrawColor p color
                              renderPicture (rs & rsColor .~ color) picture
  Pictures ps -> concatMapM (renderPicture rs) ps
  Translate point picture -> renderPicture (rs & rsTransformation %~ (!*! mkTranslation point)) picture
  Rotate r picture -> renderPicture (rs & ((rsRotation +~ r) . (rsTransformation %~ (!*! mkRotation (r ^. getRadians))))) picture
  Scale s picture -> renderPicture (rs & rsTransformation %~ (!*! mkScale s)) picture
  Sprite identifier positionMode -> do
    let m = rs ^. rsTransformation
        origin = (m !* V3 0 0 1) ^. toV2
        pos = case positionMode of
          RenderPositionCenter -> origin - (rectangle ^. rectangleDimensions ^. dividing 2)
          RenderPositionTopLeft -> origin
        rot = rs ^. rsRotation ^. degrees
        srcRect = Just (rectangle ^. from wrenchRect)
        destRect = Just (rectangleFromPoints pos (pos + (rectangle ^. rectangleDimensions)) ^. from wrenchRect)
    renderDrawSprite platform identifier srcRect destRect rot

wrenchRender :: Platform p => p -> Maybe BackgroundColor -> Picture -> IO ()
wrenchRender platform backgroundColor outerPicture = do
  maybe (return ()) (\backgroundColor' -> renderSetDrawColor backgroudColor' >> renderClear p) backgroundColor
  textures <- renderPicture (RenderState eye3 platform (fromMaybe colorsWhite backgroundColor) 0) outerPicture
  renderFinish p

data MainLoopContext p world = MainLoopContext { _mlImages          :: SurfaceMap
                                               , _mlFont            :: TTFFont
                                               , _mlPlatform        :: p
                                               , _mlAnims           :: AnimMap
                                               , _mlBackgroundColor :: Maybe BackgroundColor
                                               , _mlStepsPerSecond  :: StepsPerSecond
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

sizeToPoint :: SDLT.Size -> Point
sizeToPoint (SDLT.Size w h) = V2 (fromIntegral w) (fromIntegral h)

mainLoop :: Platform p => MainLoopContext p world -> PreviousTime -> SinceLastSimulation -> world -> IO ()
mainLoop context prevTime prevDelta world = do
  let platform = context ^. mlPlatform
  mappedEvents <- pollEvents p
  let worldAfterEvents = foldr (context ^. mlEventHandler) world mappedEvents
  newTime <- getTicks
  let timeDelta = newTime `tickDelta` prevTime
      maxDelta = fromSeconds . recip . fromIntegral $ context ^. mlStepsPerSecond
      (simulationSteps,newDelta) = splitDelta maxDelta (timeDelta + prevDelta)
  let newWorld = foldr (context ^. mlSimulationStep) worldAfterEvents (replicate simulationSteps maxDelta)
  ws <- SDLV.getWindowSize (context ^. mlWindow)
  wrenchRender
    platform
    (context ^. mlBackgroundColor)
    ((context ^. mlWorldToPicture) (sizeToPoint ws) world)
  unless (any isQuitEvent mappedEvents) $ mainLoop context newTime newDelta newWorld

$(makeLenses ''SDLPlatform)

wrenchPlay :: Platform p =>
              WindowTitle ->
              MediaFilePath ->
              Maybe BackgroundColor ->
              world ->
              StepsPerSecond ->
              (ViewportSize -> world -> Picture) ->
              (Event -> world -> world) ->
              (TimeDelta -> world -> world) ->
              IO ()
wrenchPlay windowTitle mediaPath backgroundColor initialWorld stepsPerSecond worldToPicture eventHandler simulationStep =
  withPlatform windowTitle backgroundColor mediaPath $ \platform -> do
    time <- getTicks
    mainLoop (MainLoopContext images stdfont platform anims renderer window backgroundColor stepsPerSecond worldToPicture eventHandler simulationStep) time 0 initialWorld
