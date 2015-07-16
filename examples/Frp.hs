{-# LANGUAGE RankNTypes #-}
module Main where

import ClassyPrelude
import Wrench.Picture
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch
import Wrench.Time
import Wrench.Engine(withPlatform)
import Wrench.Platform
import Wrench.WindowSize
import Wrench.MediaData
import Wrench.Engine
import Wrench.Color
import Control.Lens((^.),has,only,(^?!))
import Linear.V2
import Wrench.Event
import Wrench.KeyMovement

initialCarPosition = V2 100 100

keyDownSyms event = (^?! _Keyboard . keySym) <$> (filterE (has (_Keyboard . keyMovement . only KeyDown)) event)

setupNetwork platform surfaces tickAddHandler eventAddHandler = do
  etick <- fromAddHandler tickAddHandler
  eevent <- fromAddHandler eventAddHandler
  --let carPosX = accumB 100 (1 <$ keyDownSyms eevent)
  reactimate $ (\_ -> wrenchRender platform surfaces (error "no font specified") (Just colorsBlack) mempty) <$> etick
  reactimate $ (\_ -> putStrLn "Ah, an event") <$> eevent

main :: IO ()
main = do
  withPlatform (WindowTitle "test") DynamicWindowSize $ \platform -> do
    (tickAddHandler,tickFire) <- newAddHandler
    (eventAddHandler,eventFire) <- newAddHandler
    md <- liftIO (readMediaFiles (loadImage platform) "media")
    compiledNetwork <- compile (setupNetwork platform (md ^. mdSurfaces) tickAddHandler eventAddHandler)
    actuate compiledNetwork
    forever $ do
      ticks <- getTicks
      events <- pollEvents platform
      mapM_ eventFire events
      tickFire ticks
      threadDelay (fromSeconds (1/30))
