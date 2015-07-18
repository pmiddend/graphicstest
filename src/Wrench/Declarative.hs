{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Declarative(
    wrenchPlay
  , MediaPath(..)
  , BackgroundColor(..)
  , ImageSizeGetter
  , noBackgroundColor
  , EventHandler(..)
  , StepsPerSecond(..)
  , ToPictureHandler(..)) where

import           ClassyPrelude           hiding (FilePath, (</>))
import           Control.Lens            ((^.))
import           Control.Lens.TH         (makeLenses)
import           System.FilePath
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Event
import           Wrench.ImageData
import           Wrench.Internal.Picture
import           Wrench.MediaData
import           Wrench.Platform
import           Wrench.Rectangle
import           Wrench.SpriteIdentifier
import           Wrench.Time
import           Wrench.ViewportSize

newtype StepsPerSecond = StepsPerSecond { unpackStepsPerSecond :: Int }

type ImageSizeGetter = SpriteIdentifier -> Rectangle

newtype BackgroundColor = BackgroundColor { unpackBackgroundColor :: Maybe Color }

noBackgroundColor :: BackgroundColor
noBackgroundColor = BackgroundColor Nothing

newtype MediaPath = MediaPath { unpackMediaPath :: FilePath }

newtype EventHandler world = EventHandler { unpackEventHandler :: Event -> world -> world }
type ToPictureHandlerImpl world = ViewportSize -> world -> Picture
newtype ToPictureHandler world = ToPictureHandler { unpackToPictureHandler :: ImageSizeGetter -> ToPictureHandlerImpl world }

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
  let imageSizeGetter spriteName = snd ((surfaceData ^. mdSurfaces) `findSurfaceUnsafe` spriteName)
  font <- loadFont platform (unpackMediaPath mediaPath </> "stdfont.ttf") 15
  let loopContext = MainLoopContext
                    platform
                    (surfaceData ^. mdSurfaces)
                    font
                    (unpackBackgroundColor backgroundColor)
                    (unpackStepsPerSecond stepsPerSecond)
                    (unpackToPictureHandler worldToPicture imageSizeGetter)
                    (unpackEventHandler eventHandler)
  mainLoop loopContext time 0 initialWorld
