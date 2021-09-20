module Window.InputState.GamepadManager.Types
    ( GamepadAxisMap
    , JoystickIndex
    , JoystickId
    , GamepadType(..)
    , Gamepad(..)
    , GamepadManager(..)
    ) where

import Data.Int        (Int16)
import Foreign.C.Types (CInt)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified SDL.Input.GameController as SDL
import qualified SDL.Raw

type GamepadAxisMap = M.Map SDL.Raw.GameControllerAxis Int16
type JoystickIndex  = CInt
type JoystickId     = SDL.Raw.JoystickID

data GamepadType
    = Xbox360GamepadType
    | XboxOneGamepadType
    | PS4GamepadType
    | PS5GamepadType
    | PSXGamepadType
    | SwitchProGamepadType
    deriving (Eq, Show)

data Gamepad = Gamepad
    { _type           :: GamepadType
    , _gameController :: SDL.Raw.GameController
    , _joystickId     :: JoystickId
    , _heldButtons    :: S.Set SDL.ControllerButton
    , _axisMap        :: GamepadAxisMap
    , _prevAxisMap    :: GamepadAxisMap
    }

data GamepadManager = GamepadManager
    { _gamepadsMap     :: M.Map JoystickId Gamepad
    , _buttonsPressed  :: S.Set SDL.ControllerButton
    , _buttonsReleased :: S.Set SDL.ControllerButton
    }
