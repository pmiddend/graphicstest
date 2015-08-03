{-# LANGUAGE TemplateHaskell #-}
module Wrench.Event where

import           ClassyPrelude
import           Control.Lens               (makeLenses, makePrisms)
import           Linear.V2
import           Wrench.KeyMovement
import           Wrench.Keysym
import           Wrench.MouseButton
import           Wrench.MouseButtonMovement
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
  , _mousePosition       :: V2 Int
  }

$(makeLenses ''MouseButtonEvent)

data MouseWheelEvent = MouseWheelEvent {
    _mouseWheelDirection :: V2 Int
  }

$(makeLenses ''MouseWheelEvent)

data CursorMotionEvent = CursorMotionEvent {
    _cursorMotionPosition :: V2 Int
  }

$(makeLenses ''CursorMotionEvent)

data MouseAxisEvent = MouseAxisEvent {
    _mouseAxisDelta :: V2 Int
  }

$(makeLenses ''MouseAxisEvent)

data Event =
      Keyboard KeyboardEvent
    | MouseButton MouseButtonEvent
    | CursorMotion CursorMotionEvent
    | MouseAxis MouseAxisEvent
    | MouseWheel MouseWheelEvent
    | Quit
    | Tick TimeDelta

$(makePrisms ''Event)

isQuitEvent :: Event -> Bool
isQuitEvent Quit = True
isQuitEvent _ = False
