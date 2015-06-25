module Wrench.CommonGeometry where

import Wrench.Rectangle
import Wrench.FloatType
import Wrench.Point
import Linear.V2
import Control.Lens((^.))
import ClassyPrelude

pointOp :: (FloatType -> FloatType -> Bool) -> Point -> Point -> Bool
pointOp f (V2 x1 y1) (V2 x2 y2) = x1 `f` x2 && y1 `f` y2

pointGE :: Point -> Point -> Bool
pointGE a b = pointOp (>=) a b

pointLE :: Point -> Point -> Bool
pointLE a b = pointOp (<=) a b

pointInRectangle :: Point -> Rectangle -> Bool
pointInRectangle point rect = point `pointGE` (rect ^. rectLeftTop) && point `pointLE` (rect ^. rectRightBottom)

