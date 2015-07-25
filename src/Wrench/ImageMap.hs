module Wrench.ImageMap where

import           ClassyPrelude
import qualified Data.Map.Strict  as M
import           Wrench.ImageId
import           Wrench.Rectangle

type ImageMap = M.Map ImageId (Rectangle Int)
