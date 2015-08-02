module Wrench.ImageMap where

import           ClassyPrelude
import qualified Data.Map.Strict        as M
import           Wrench.ImageIdentifier
import           Wrench.Rectangle

type ImageMap = M.Map ImageIdentifier (Rectangle Int)
