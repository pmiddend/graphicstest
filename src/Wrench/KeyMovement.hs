{-|
Module      : Wrench.KeyMovement
Description : Key up and key down
Maintainer  : pmidden@secure.mailbox.org
-}
module Wrench.KeyMovement where

import           ClassyPrelude

data KeyMovement = KeyUp | KeyDown
  deriving (Eq, Show)
