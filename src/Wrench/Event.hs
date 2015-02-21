module Wrench.Event where

import Wrench.KeyMovement
import Wrench.Keysym
import ClassyPrelude

data Event =
    Keyboard { keyMovement :: KeyMovement
           , keyRepeat :: Bool
           , keySym :: Keysym
           }
  | Quit

isQuitEvent :: Event -> Bool
isQuitEvent Quit = True
isQuitEvent _ = False
