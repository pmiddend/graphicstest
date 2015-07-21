module Wrench.CommonGeometry where

import           ClassyPrelude
import           Control.Lens     ((^.))
import           Linear.V2
import           Wrench.FloatType
import           Wrench.Point
import           Wrench.Rectangle

pointOp :: (FloatType -> FloatType -> Bool) -> Point -> Point -> Bool
pointOp f (V2 x1 y1) (V2 x2 y2) = x1 `f` x2 && y1 `f` y2

pointGE :: Point -> Point -> Bool
pointGE a b = pointOp (>=) a b

pointLE :: Point -> Point -> Bool
pointLE a b = pointOp (<=) a b

pointL :: Point -> Point -> Bool
pointL a b = pointOp (<) a b

pointG :: Point -> Point -> Bool
pointG a b = pointOp (>) a b

pointInRectangle :: Point -> Rectangle -> Bool
pointInRectangle point rect = point `pointGE` (rect ^. rectLeftTop) && point `pointLE` (rect ^. rectRightBottom)

