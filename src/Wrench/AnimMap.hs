module Wrench.AnimMap where

import qualified Data.Map.Strict           as M
import Wrench.AnimId
import Wrench.Animation

type AnimMap = M.Map AnimId Animation
