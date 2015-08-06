{-|
Module      : Wrench.Event
Description : Event data types
Maintainer  : pmidden@secure.mailbox.org
-}
{-# LANGUAGE TemplateHaskell #-}
module Wrench.Event where

import           ClassyPrelude
import           Control.Lens               (makeLenses, makePrisms)
import           Linear.V2
import           Wrench.KeyMovement
import           Wrench.Keysym
import           Wrench.MouseButton
import           Wrench.MouseButtonMovement

-- | A keyboard event (does not include typed text)
data KeyboardEvent = KeyboardEvent {
    _keyMovement :: KeyMovement -- | Key press or release
  , _keyRepeat   :: Bool -- | Was this event generated because of the OS key repeat mechanism?
  , _keySym      :: Keysym -- | Which key was pressed
  }

$(makeLenses ''KeyboardEvent)

-- | Mouse button event
data MouseButtonEvent = MouseButtonEvent {
    _mouseButton         :: MouseButton -- | Which button was pressed
  , _mouseButtonMovement :: MouseButtonMovement -- | Button press or release
  , _mousePosition       :: V2 Int -- | Mouse position relative to the window at the time where the button was pressed
  }

$(makeLenses ''MouseButtonEvent)

-- | Mouse wheel event
data MouseWheelEvent = MouseWheelEvent {
    _mouseWheelDirection :: V2 Int -- | Direction and magnitude of wheel rotation
  }

$(makeLenses ''MouseWheelEvent)

-- | Cursor motion events; this is "relative movement" and is only generated when the cursor is not grabbed
data CursorMotionEvent = CursorMotionEvent {
    _cursorMotionPosition :: V2 Int -- | Cursor movement delta
  }

$(makeLenses ''CursorMotionEvent)

-- | Mouse motion events; this is only generated when the mouse is grabbed
data MouseAxisEvent = MouseAxisEvent {
    _mouseAxisDelta :: V2 Int
  }

$(makeLenses ''MouseAxisEvent)

-- | All sorts of events
data Event =
      Keyboard KeyboardEvent
    | MouseButton MouseButtonEvent
    | CursorMotion CursorMotionEvent
    | MouseAxis MouseAxisEvent
    | MouseWheel MouseWheelEvent
    | Quit -- ^ Quit event, such as killing the application via the window manager

$(makePrisms ''Event)

isQuitEvent :: Event -> Bool
isQuitEvent Quit = True
isQuitEvent _ = False
