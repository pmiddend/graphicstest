module Wrench.PlayMode where

import ClassyPrelude

data PlayMode = PlayModeOnce
              | PlayModeLooping
              deriving(Eq,Show,Enum)
