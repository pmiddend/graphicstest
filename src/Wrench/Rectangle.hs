{-# LANGUAGE  TemplateHaskell #-}
module Wrench.Rectangle(
      Rectangle
    , rectLeftTop
    , rectRightBottom
    , rectFromPoints
    , rectFromOriginAndDim
    , rectDimensions
    ) where

import Wrench.Point
import Control.Lens.TH
import Control.Lens.Getter(to,Getter)
import ClassyPrelude

data Rectangle = Rectangle { _rectLeftTop     :: Point
                           , _rectRightBottom :: Point
                           } deriving(Show,Eq)

rectFromPoints :: Point -> Point -> Rectangle
rectFromPoints = Rectangle

rectFromOriginAndDim :: Point -> Point -> Rectangle
rectFromOriginAndDim origin dim = rectFromPoints origin (origin + dim)

rectDimensions :: Getter Rectangle Point
rectDimensions = to rectDimensions'
 where rectDimensions' (Rectangle lt rb) = rb - lt

$(makeLenses ''Rectangle)
