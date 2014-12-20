{-# LANGUAGE  TemplateHaskell #-}
module Wrench.Rectangle(
      Rectangle
    , rectLeftTop
    , rectRightBottom
    , rectangleFromPoints
    , rectangleDimensions
    ) where

import Wrench.Point
import Control.Lens.TH
import Control.Lens.Getter(to,Getter)

data Rectangle = Rectangle { _rectLeftTop     :: Point
                           , _rectRightBottom :: Point
                           } deriving(Show,Eq)

rectangleFromPoints :: Point -> Point -> Rectangle
rectangleFromPoints = Rectangle

rectangleDimensions :: Getter Rectangle Point
rectangleDimensions = to rectangleDimensions'
 where rectangleDimensions' (Rectangle lt rb) = rb - lt

$(makeLenses ''Rectangle)
