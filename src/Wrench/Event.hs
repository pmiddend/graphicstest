module Wrench.Event where

import Wrench.KeyMovement
import Wrench.Keysym
import Wrench.Time
import ClassyPrelude

data Event =
    Keyboard { keyMovement :: KeyMovement
           , keyRepeat :: Bool
           , keySym :: Keysym
           }
    | Quit
    | Tick TimeDelta

isQuitEvent :: Event -> Bool
isQuitEvent Quit = True
isQuitEvent _ = False
