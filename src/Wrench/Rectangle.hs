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

data Rectangle = Rectangle { _rectLeftTop     :: Point
                           , _rectRightBottom :: Point
                           } deriving(Show,Eq)

rectangleFromPoints :: Point -> Point -> Rectangle
rectangleFromPoints = Rectangle

rectangleDimensions :: Rectangle -> Point
rectangleDimensions (Rectangle lt rb) = rb - lt

$(makeLenses ''Rectangle)
