{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Wrench.Time(
    TimeDelta
  , TimeTicks
  , tickDelta
  , threadDelay
  , getTicks
  , fromSeconds
  , toSeconds
  , plusDuration
  , fromMilliseconds
  , toMilliseconds
) where

import           System.Clock (Clock (Monotonic), TimeSpec (TimeSpec), getTime)
import ClassyPrelude
import qualified Control.Concurrent as CC

newtype TimeDelta = TimeDelta { _timeDelta :: Double } deriving(Show,Num,Eq,Ord)
newtype TimeTicks = TimeTicks { _timeTicks :: Word64 } deriving(Show,Num,Eq,Ord)

tickSeconds :: Integral a => a -> TimeTicks
tickSeconds = TimeTicks . fromIntegral . (* 1000000000)

tickNanoSeconds :: Int -> TimeTicks
tickNanoSeconds = TimeTicks . fromIntegral

nanoSecondsDivisor :: Double
nanoSecondsDivisor = (1000.0 * 1000.0 * 1000.0)

tickDelta :: TimeTicks -> TimeTicks -> TimeDelta
tickDelta new old = TimeDelta $ fromIntegral (_timeTicks new - _timeTicks old) / nanoSecondsDivisor 

plusDuration :: TimeTicks -> TimeDelta -> TimeTicks
plusDuration t d = TimeTicks (_timeTicks t + floor (_timeDelta d * nanoSecondsDivisor))

fromSeconds :: Double -> TimeDelta
fromSeconds = TimeDelta

fromMilliseconds :: Double -> TimeDelta
fromMilliseconds = TimeDelta . (/ 1000)

toMilliseconds :: TimeDelta -> Double
toMilliseconds (TimeDelta t) = t * 1000

toMicroseconds :: TimeDelta -> Double
toMicroseconds (TimeDelta t) = t * 1000 * 1000

toSeconds :: TimeDelta -> Double
toSeconds (TimeDelta t) = t

getTicks :: IO TimeTicks
getTicks = do
  (TimeSpec s ns) <- getTime Monotonic
  return $ tickSeconds s + tickNanoSeconds (fromIntegral ns)

threadDelay :: MonadIO m => TimeDelta -> m ()
threadDelay t = liftIO (CC.threadDelay (floor (toMicroseconds t)))
