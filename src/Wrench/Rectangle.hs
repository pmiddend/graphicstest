{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
module Wrench.Rectangle(
      Rectangle
    , rectLeftTop
    , rectLeft
    , rectRight
    , rectTop
    , rectBottom
    , rectWidth
    , rectHeight
    , rectIntCenter
    , rectFracCenter
    , rectRightBottom
    , rectFromPoints
    , rectFromOriginAndDim
    , rectDimensions
    ) where

import           ClassyPrelude
import           Control.Lens.Getter (Getter, to, (^.))
import           Control.Lens.TH
import           Linear.V2

data Rectangle a = Rectangle { _rectLeftTop     :: V2 a
                             , _rectRightBottom :: V2 a
                             } deriving(Show,Eq,Functor)

$(makeLenses ''Rectangle)

rectLeft :: Getter (Rectangle a) a
rectLeft = rectLeftTop . _x

rectTop :: Getter (Rectangle a) a
rectTop = rectLeftTop . _y

rectRight :: Getter (Rectangle a) a
rectRight = rectRightBottom . _x

rectBottom :: Getter (Rectangle a) a
rectBottom = rectRightBottom . _y

rectFromPoints :: V2 a -> V2 a -> Rectangle a
rectFromPoints = Rectangle

rectIntCenter :: Integral a => Getter (Rectangle a) (V2 a)
rectIntCenter = to (\r -> r ^. rectLeftTop + ((`div` 2)<$> r ^. rectDimensions))

rectFracCenter :: Fractional a => Getter (Rectangle a) (V2 a)
rectFracCenter = to (\r -> r ^. rectLeftTop + ((/ 2)<$> r ^. rectDimensions))

rectFromOriginAndDim :: Num a => V2 a -> V2 a -> Rectangle a
rectFromOriginAndDim origin dim = rectFromPoints origin (origin + dim)

rectDimensions :: Num a => Getter (Rectangle a) (V2 a)
rectDimensions = to rectDimensions'
 where rectDimensions' (Rectangle lt rb) = rb - lt

rectWidth :: Num a => Getter (Rectangle a) a
rectWidth = rectDimensions . _x

rectHeight :: Num a => Getter (Rectangle a) a
rectHeight = rectDimensions . _y
