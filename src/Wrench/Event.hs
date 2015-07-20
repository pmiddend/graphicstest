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

data CursorMotionEvent = CursorMotionEvent {
    _cursorMotionPosition :: Point
  }

$(makeLenses ''CursorMotionEvent)

data MouseAxisEvent = MouseAxisEvent {
    _mouseAxisDelta :: Point
  }

$(makeLenses ''MouseAxisEvent)

data Event =
      Keyboard KeyboardEvent
    | MouseButton MouseButtonEvent
    | CursorMotion CursorMotionEvent
    | MouseAxis MouseAxisEvent
    | Quit
    | Tick TimeDelta

$(makePrisms ''Event)

isQuitEvent :: Event -> Bool
isQuitEvent Quit = True
isQuitEvent _ = False
