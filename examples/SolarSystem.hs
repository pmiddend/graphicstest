{-# LANGUAGE TemplateHaskell #-}
module Main where

import           ClassyPrelude
import           Control.Lens            (filtered, has, makeLenses, re, (^.),(^..))
import           Linear.V2
import           Linear.Vector
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import qualified Wrench.Keysym           as Key
import           Wrench.MonadGame
import           Wrench.MouseGrabMode
import           Wrench.Picture
import           Wrench.RenderBlockMode
import           Wrench.SpriteIdentifier
import           Wrench.Time
import           Wrench.WindowSize

data Planet = Planet {
    _planetImage             :: SpriteIdentifier
  , _planetDistanceSun       :: Double
  , _planetRadius            :: Double
  , _planetAroundSunSpeed    :: Double
  , _planetAroundItselfSpeed :: Double
  }

$(makeLenses ''Planet)

vdiv :: Integral a => V2 a -> a -> V2 a
vdiv a d = (`div` d) <$> a

planets :: [Planet]
planets = [
    Planet{
      _planetImage = "mercury"
    , _planetDistanceSun = 0.012873
    , _planetRadius = 0.03488
    , _planetAroundSunSpeed = 1.0
    , _planetAroundItselfSpeed = 58
    },
    Planet{
      _planetImage = "venus"
    , _planetDistanceSun = 0.024055
    , _planetRadius = 0.08655
    , _planetAroundSunSpeed = 0.388392
    , _planetAroundItselfSpeed = -243
    },
    Planet{
      _planetImage = "earth"
    , _planetDistanceSun = 0.033255
    , _planetRadius = 0.09113
    , _planetAroundSunSpeed = 0.238356
    , _planetAroundItselfSpeed = 1
    },
    Planet{
      _planetImage = "mars"
    , _planetDistanceSun = 0.050672
    , _planetRadius = 0.04847
    , _planetAroundSunSpeed = 0.126822
    , _planetAroundItselfSpeed = 1
    },
    Planet{
      _planetImage = "jupiter"
    , _planetDistanceSun = 0.173026
    , _planetRadius = 1
    , _planetAroundSunSpeed = 0.0200831
    , _planetAroundItselfSpeed = 0.41
    },
    Planet{
      _planetImage = "saturn"
    , _planetDistanceSun = 0.317149
    , _planetRadius = 0.832944
    , _planetAroundSunSpeed = 0.008089
    , _planetAroundItselfSpeed = 0.444
    },
    Planet{
      _planetImage = "uranus"
    , _planetDistanceSun = 0.638151
    , _planetRadius = 0.36277
    , _planetAroundSunSpeed = 0.00283507
    , _planetAroundItselfSpeed = -0.71
    },
    Planet{
      _planetImage = "neptune"
    , _planetDistanceSun = 1
    , _planetRadius = 0.352190
    , _planetAroundSunSpeed = 0.0014454
    , _planetAroundItselfSpeed = 0.67
    }
  ]

baseImageSize :: V2 Double
baseImageSize = V2 64 64

timeScale :: Radians Double
timeScale = 10

planetToPicture :: Radians Double -> V2 Double -> Planet -> Picture Double Double
planetToPicture rot viewportSize p =
  let
    minViewport = min (viewportSize ^. _x) (viewportSize ^. _y)
    translation = V2 ((minViewport / 2) * (p ^. planetDistanceSun)) 0
    imageSize = baseImageSize ^* (p ^. planetRadius)
    base = pictureSprite (p ^. planetImage) imageSize
  in
    (rot * (p ^. planetAroundSunSpeed . re _Radians)) `pictureRotated` (translation `pictureTranslated` ((rot * (p ^. planetAroundItselfSpeed . re _Radians)) `pictureRotated` base))

programQuitEvents :: Traversable t => t Event -> Bool
programQuitEvents events = has (traverse . _Keyboard . keySym . filtered (== Key.Space)) events || has (traverse . _Quit) events

applyMouseMotion :: V2 Int -> Int -> Int
applyMouseMotion (V2 _ y) scale | scale <= y = 1
                                | otherwise = scale - y

mainLoop :: Radians Double -> Int -> MonadGameBackend ()
mainLoop oldRot spaceScale = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  d <- gcurrentTimeDelta
  let newSpaceScale = foldr applyMouseMotion spaceScale (events ^.. traverse . _MouseWheel . mouseWheelDirection)
  unless (programQuitEvents events) $ do
    viewportSize <- fmap fromIntegral <$> gviewportSize
    let newRot = oldRot + timeScale * (radians (toSeconds d))
    let
      ps = planetToPicture newRot viewportSize <$> planets
      sun = pictureSprite "sun" (baseImageSize ^* 0.1)
      solarSystem = (viewportSize / 2) `pictureTranslated` ((fromIntegral <$> V2 newSpaceScale newSpaceScale) `pictureScaled` (pictures ps <> sun))
    grender (first (floor :: Double -> Int) solarSystem)
    mainLoop newRot newSpaceScale

main :: IO ()
main =
  runGame "media" "wrench solar system example" DynamicWindowSize MouseGrabNo (Just colorsBlack) (RenderAndWait 60) (mainLoop 0 1)

