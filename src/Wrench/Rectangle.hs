{-# LANGUAGE  TemplateHaskell #-}
module Wrench.Rectangle(
      Rectangle
    , rectLeftTop
    , rectRightBottom
    , rectangleFromPoints
    , rectangleFromOriginAndDim
    , rectangleDimensions
    ) where

import Wrench.Point
import Control.Lens.TH
import Control.Lens.Getter(to,Getter)
import ClassyPrelude

data Rectangle = Rectangle { _rectLeftTop     :: Point
                           , _rectRightBottom :: Point
                           } deriving(Show,Eq)

rectangleFromPoints :: Point -> Point -> Rectangle
rectangleFromPoints = Rectangle

rectangleFromOriginAndDim :: Point -> Point -> Rectangle
rectangleFromOriginAndDim origin dim = rectangleFromPoints origin (origin + dim)

rectangleDimensions :: Getter Rectangle Point
rectangleDimensions = to rectangleDimensions'
 where rectangleDimensions' (Rectangle lt rb) = rb - lt

$(makeLenses ''Rectangle)
