module Window.InputState.Types
    ( InputType(..)
    , InputState(..)
    , InputRead(..)
    ) where

import Data.Aeson.Types (FromJSON)
import GHC.Generics     (Generic)
import qualified Data.Text as T
import qualified SDL

import Util
import Window.InputState.Alias
import Window.InputState.GamepadManager.Types
import Window.InputState.RawData

data InputType
    = MouseKbInputType
    | GamepadInputType
    deriving (Eq, FromJSON, Generic)

data InputState = InputState
    { _inactive             :: Bool
    , _keyStatePressed      :: [SDL.Scancode]
    , _keyStateReleased     :: [SDL.Scancode]
    , _keyHoldState         :: SDL.Scancode -> Bool
    , _keyHoldPrevState     :: SDL.Scancode -> Bool
    , _mouseStatePressed    :: [SDL.MouseButton]
    , _mouseStateReleased   :: [SDL.MouseButton]
    , _mouseHoldState       :: SDL.MouseButton -> Bool
    , _mouseHoldPrevState   :: SDL.MouseButton -> Bool
    , _mouseWheelEventData  :: Maybe SDL.MouseWheelEventData
    , _mousePos             :: Pos2
    , _mouseWorldPos        :: Pos2
    , _mouseMoved           :: Bool
    , _gamepadManager       :: GamepadManager
    , _textBuffer           :: T.Text
    , _lastUsedInputType    :: InputType
    , _lastGamepadType      :: GamepadType
    , _pressedInputRawData  :: Maybe InputRawData
    , _inputAliasRawDataMap :: InputAliasRawDataMap
    }

class Monad m => InputRead m where
    readInputState :: m InputState
