{-# LANGUAGE TemplateHaskell #-}
module Wrench.Event where

import           ClassyPrelude
import           Control.Lens               (makeLenses, makePrisms)
import           Wrench.KeyMovement
import           Wrench.Keysym
import           Wrench.MouseButton
import           Wrench.MouseButtonMovement
import           Wrench.Point
import           Wrench.Time

data KeyboardEvent = KeyboardEvent {
    _keyMovement :: KeyMovement
  , _keyRepeat   :: Bool
  , _keySym      :: Keysym
  }

$(makeLenses ''KeyboardEvent)

data MouseButtonEvent = MouseButtonEvent {
    _mouseButton         :: MouseButton
  , _mouseButtonMovement :: MouseButtonMovement
  , _mousePosition       :: Point
  }

$(makeLenses ''MouseButtonEvent)

data MouseMotionEvent = MouseMotionEvent {
    _mouseMotionDelta    :: Point
  , _mouseMotionPosition :: Point
  }

data Event =
      Keyboard KeyboardEvent
    | MouseButton MouseButtonEvent
    | MouseMotion MouseMotionEvent
    | Quit
    | Tick TimeDelta

$(makePrisms ''Event)

isQuitEvent :: Event -> Bool
isQuitEvent Quit = True
isQuitEvent _ = False
