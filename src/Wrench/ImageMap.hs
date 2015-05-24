module Wrench.ImageMap where

import qualified Data.Map.Strict           as M
import Wrench.Rectangle
import Wrench.ImageId

type ImageMap = M.Map ImageId Rectangle
