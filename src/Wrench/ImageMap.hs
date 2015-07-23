module Wrench.ImageMap where

import qualified Data.Map.Strict  as M
import           Wrench.ImageId
import           Wrench.Rectangle

type ImageMap a = M.Map ImageId (Rectangle a)
