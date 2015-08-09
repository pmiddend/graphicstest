{-|
Module      : Wrench.ImageMap
Description : Map images to their atlased components
Maintainer  : pmidden@secure.mailbox.org
-}
module Wrench.ImageMap where

import           ClassyPrelude
import qualified Data.Map.Strict        as M
import           Wrench.ImageIdentifier
import           Wrench.Rectangle

-- ^ Map a sub-rectangle inside an image
type ImageMap = M.Map ImageIdentifier (Rectangle Int)
