{-|
Module      : Wrench.CommonGeometry
Description : Functions belonging to more than one geometric primitive
Maintainer  : pmidden@secure.mailbox.org
-}
module Wrench.CommonGeometry where

import           ClassyPrelude
import           Control.Lens     ((^.))
import           Linear.V2
import           Wrench.Rectangle

pointOp :: (a -> a -> Bool) -> V2 a -> V2 a -> Bool
pointOp f (V2 x1 y1) (V2 x2 y2) = x1 `f` x2 && y1 `f` y2

-- | Point greater or equal, "component-wise"
pointGE :: Ord a => V2 a -> V2 a -> Bool
pointGE a b = pointOp (>=) a b

-- | Point less or equal, "component-wise"
pointLE :: Ord a => V2 a -> V2 a -> Bool
pointLE a b = pointOp (<=) a b

-- | Point less, "component-wise"
pointL :: Ord a => V2 a -> V2 a -> Bool
pointL a b = pointOp (<) a b

-- | Point greater, "component-wise"
pointG :: Ord a => V2 a -> V2 a -> Bool
pointG a b = pointOp (>) a b

pointInRectangle :: Ord a => V2 a -> Rectangle a -> Bool
pointInRectangle point rect = point `pointGE` (rect ^. rectLeftTop) && point `pointLE` (rect ^. rectRightBottom)

