{-|
Module      : Wrench.AnimMap
Description : Type for animation maps
Maintainer  : pmidden@secure.mailbox.org
-}
module Wrench.AnimMap where

import qualified Data.Map.Strict       as M
import           Wrench.Animation
import           Wrench.AnimIdentifier

type AnimMap = M.Map AnimIdentifier Animation
