{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           ClassyPrelude
import           Control.Lens                (has, only, (^.), (^?!))
import           Linear.V2
import qualified Reactive.Banana.Combinators as RBC
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Switch
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Event
import           Wrench.ImageData
import           Wrench.KeyMovement
import           Wrench.MediaData
import           Wrench.Picture
import           Wrench.Platform
import           Wrench.Time
import           Wrench.WindowSize

initialCarPosition = V2 100 100

keyDownSyms event = (^?! _Keyboard . keySym) <$> (RBC.filterE (has (_Keyboard . keyMovement . only KeyDown)) event)

setupNetwork :: forall t p. Frameworks t => Platform p => p -> SurfaceMap (PlatformImage p) -> AddHandler TimeTicks -> AddHandler Event -> Moment t ()
setupNetwork platform surfaces tickAddHandler eventAddHandler = do
  etick <- fromAddHandler tickAddHandler
  eevent <- fromAddHandler eventAddHandler
  let
    cumulatedPositionBehavior :: RBC.Behavior t Int
    cumulatedPositionBehavior = RBC.accumB 100 ((+10) <$ keyDownSyms eevent)
    currentPictureEvent = ((\pos -> (V2 (fromIntegral pos) 100) `pictureTranslated` (pictureSpriteCentered "car")) <$> cumulatedPositionBehavior) RBC.<@ etick
  --let carPosX = accumB 100 (1 <$ keyDownSyms eevent)
  reactimate $ (wrenchRender platform surfaces (error "no font specified") (Just colorsBlack)) <$> currentPictureEvent
--  reactimate $ (\v -> putStrLn $ "Ah, an event: " <> pack (show v) ) <$> cumulatedPosition

main :: IO ()
main = do
  withPlatform (WindowTitle "test") DynamicWindowSize $ \platform -> do
    (tickAddHandler,tickFire) <- newAddHandler
    (eventAddHandler,eventFire) <- newAddHandler
    md <- liftIO (readMediaFiles (loadImage platform) "media/images")
    compiledNetwork <- compile (setupNetwork platform (md ^. mdSurfaces) tickAddHandler eventAddHandler)
    actuate compiledNetwork
    forever $ do
      ticks <- getTicks
      events <- pollEvents platform
      mapM_ eventFire events
      tickFire ticks
      threadDelay (fromSeconds (1/30))
