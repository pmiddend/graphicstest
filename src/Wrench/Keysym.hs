module Wrench.Keysym where

import Wrench.Keycode
import Data.Word(Word16)

data Keysym = Keysym { keyKeycode :: Keycode
                     , keyModifiers :: Word16
                     }
  deriving (Eq, Show)
