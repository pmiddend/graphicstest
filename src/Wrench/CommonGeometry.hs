module Wrench.CommonGeometry where

import           ClassyPrelude
import           Control.Lens     ((^.))
import           Linear.V2
import           Wrench.Rectangle

pointOp :: (a -> a -> Bool) -> V2 a -> V2 a -> Bool
pointOp f (V2 x1 y1) (V2 x2 y2) = x1 `f` x2 && y1 `f` y2

pointGE :: Ord a => V2 a -> V2 a -> Bool
pointGE a b = pointOp (>=) a b

pointLE :: Ord a => V2 a -> V2 a -> Bool
pointLE a b = pointOp (<=) a b

pointL :: Ord a => V2 a -> V2 a -> Bool
pointL a b = pointOp (<) a b

pointG :: Ord a => V2 a -> V2 a -> Bool
pointG a b = pointOp (>) a b

pointInRectangle :: Ord a => V2 a -> Rectangle a -> Bool
pointInRectangle point rect = point `pointGE` (rect ^. rectLeftTop) && point `pointLE` (rect ^. rectRightBottom)

