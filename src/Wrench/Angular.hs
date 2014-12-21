{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wrench.Angular where

import           Control.Lens.TH           (makeLenses)
import Wrench.FloatType
import           Control.Lens.Iso          (Iso', iso)

newtype Radians = Radians { _getRadians :: FloatType } deriving(Show,Eq,Num)
$(makeLenses ''Radians)
newtype Degrees = Degrees { _getDegrees :: FloatType } deriving(Show,Eq,Num)
$(makeLenses ''Degrees)

degrees :: Iso' Radians Degrees
degrees = iso radToDeg degToRad

degToRad :: Degrees -> Radians
degToRad (Degrees x) = Radians (x * pi / 180)

radToDeg :: Radians -> Degrees
radToDeg (Radians x) = Degrees (x * 180 / pi)

