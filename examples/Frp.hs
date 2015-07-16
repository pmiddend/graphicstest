module Main where

import ClassyPrelude
import Wrench.Picture
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Control.Concurrent(forkIO)
import Wrench.Time(threadDelay,fromSeconds,getTicks)

setupNetwork tickAddHandler = do
  etick <- fromAddHandler tickAddHandler
  reactimate $ print <$> etick

wrenchFrpRender :: Behavior t Picture -> IO ()
wrenchFrpRender pictureBehavior = do
  (tickAddHandler,tickFire) <- newAddHandler
  compiledNetwork <- compile $ setupNetwork tickAddHandler
  actuate compiledNetwork
  --tid <- forkIO (forever (threadDelay (fromSeconds 1) >> getTicks >>= tickFire))
  forever (threadDelay (fromSeconds 1) >> getTicks >>= tickFire)

pictureHandler :: Behavior t Picture
pictureHandler = undefined

main = do
  withPlatform "test" WindowSizeDynamic $ \
