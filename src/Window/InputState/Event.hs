module Window.InputState.Event
    ( InputEvent(..)
    ) where

import qualified SDL

data InputEvent
    = KeyEvent SDL.KeyboardEventData
    | MouseButtonEvent SDL.MouseButtonEventData
    | MouseMotionEvent SDL.MouseMotionEventData
    | MouseWheelEvent SDL.MouseWheelEventData
    | ControllerDeviceEvent SDL.ControllerDeviceEventData
    | ControllerButtonEvent SDL.ControllerButtonEventData
    | ControllerAxisEvent SDL.ControllerAxisEventData
    | TextEvent SDL.TextInputEventData
    | QuitEvent
    deriving Eq
