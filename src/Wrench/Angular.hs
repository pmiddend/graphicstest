{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Angular where

import           ClassyPrelude
import           Control.Lens.Iso (Iso', iso)
import           Control.Lens.TH  (makeLenses)

newtype Radians a = Radians { _getRadians :: a } deriving(Show,Eq,Num)
$(makeLenses ''Radians)
newtype Degrees a = Degrees { _getDegrees :: a } deriving(Show,Eq,Num)
$(makeLenses ''Degrees)

degrees :: (Num a,Floating a) => Iso' (Radians a) (Degrees a)
degrees = iso radToDeg degToRad

degToRad :: (Num a,Floating a) => (Degrees a) -> (Radians a)
degToRad (Degrees x) = Radians (x * pi / 180)

radToDeg :: (Num a,Floating a) => (Radians a) -> (Degrees a)
radToDeg (Radians x) = Degrees (x * 180 / pi)

