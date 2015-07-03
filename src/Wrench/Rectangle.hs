{-# LANGUAGE  TemplateHaskell #-}
module Wrench.Rectangle(
      Rectangle
    , rectLeftTop
    , rectLeft
    , rectRight
    , rectTop
    , rectBottom
    , rectWidth
    , rectHeight
    , rectRightBottom
    , rectFromPoints
    , rectFromOriginAndDim
    , rectDimensions
    ) where

import Wrench.Point
import Wrench.FloatType
import Control.Lens.TH
import Control.Lens.Getter(to,Getter)
import Linear.V2
import ClassyPrelude

data Rectangle = Rectangle { _rectLeftTop     :: Point
                           , _rectRightBottom :: Point
                           } deriving(Show,Eq)

$(makeLenses ''Rectangle)

rectLeft :: Getter Rectangle FloatType
rectLeft = rectLeftTop . _x

rectTop :: Getter Rectangle FloatType
rectTop = rectLeftTop . _y

rectRight :: Getter Rectangle FloatType
rectRight = rectRightBottom . _x

rectBottom :: Getter Rectangle FloatType
rectBottom = rectRightBottom . _y

rectFromPoints :: Point -> Point -> Rectangle
rectFromPoints = Rectangle

rectFromOriginAndDim :: Point -> Point -> Rectangle
rectFromOriginAndDim origin dim = rectFromPoints origin (origin + dim)

rectDimensions :: Getter Rectangle Point
rectDimensions = to rectDimensions'
 where rectDimensions' (Rectangle lt rb) = rb - lt

rectWidth :: Getter Rectangle FloatType
rectWidth = rectDimensions . _x

rectHeight :: Getter Rectangle FloatType
rectHeight = rectDimensions . _y
