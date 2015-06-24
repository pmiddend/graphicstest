{-# LANGUAGE TemplateHaskell #-}
module Wrench.Event where

import Wrench.KeyMovement
import Wrench.Keysym
import Wrench.MouseButton
import Wrench.Time
import Wrench.MouseButtonMovement
import Wrench.Point
import ClassyPrelude
import Control.Lens(makeLenses,makePrisms)

data KeyboardEvent = KeyboardEvent {
    _keyMovement :: KeyMovement
  , _keyRepeat :: Bool
  , _keySym :: Keysym
  }

$(makeLenses ''KeyboardEvent)

data MouseButtonEvent = MouseButtonEvent {
    _mouseButton :: MouseButton
  , _mouseButtonMovement :: MouseButtonMovement
  , _mousePosition :: Point
  }

data Event =
      Keyboard KeyboardEvent
    | MouseButton MouseButtonEvent
    | Quit
    | Tick TimeDelta

$(makePrisms ''Event)

isQuitEvent :: Event -> Bool
isQuitEvent Quit = True
isQuitEvent _ = False
