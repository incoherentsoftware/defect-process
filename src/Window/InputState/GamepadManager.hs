module Window.InputState.GamepadManager
    ( module Window.InputState.GamepadManager.Types
    , axisMaxDeadzoneValue
    , gamepadManagerGamepadType
    , mkGamepadManager
    , clearGamepadManager
    , updateGamepadManager
    , addGamepadManagerGamepad
    , removeGamepadManagerGamepad
    , remapGamepadManagerGamepad
    , gamepadManagerButtonPressed
    , gamepadManagerButtonReleased
    , gamepadManagerButtonHold
    , gamepadManagerAxisPosition
    , gamepadManagerPrevAxisPosition
    , gamepadManagerNegAxisPressed
    , gamepadManagerPosAxisPressed
    , processGamepadManagerButtonEvent
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe             (catMaybes)
import Data.Traversable       (for)
import Data.Word              (Word8)
import Foreign.C.String       (peekCString)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Internal.Numbered as SDL
import qualified SDL.Raw

import Configs
import Configs.All.Settings
import Configs.All.Settings.Controls
import Window.InputState.GamepadManager.Types

axisMaxValue          = 32768 :: Int  -- range is technically [-32768, 32767], but this is fine
axisMaxDeadzoneValue  = 8190  :: Int
sdlButtonStatePressed = 1     :: Word8

allControllerButtons =
    [ SDL.Raw.SDL_CONTROLLER_BUTTON_A
    , SDL.Raw.SDL_CONTROLLER_BUTTON_B
    , SDL.Raw.SDL_CONTROLLER_BUTTON_X
    , SDL.Raw.SDL_CONTROLLER_BUTTON_Y
    , SDL.Raw.SDL_CONTROLLER_BUTTON_BACK
    , SDL.Raw.SDL_CONTROLLER_BUTTON_START
    , SDL.Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK
    , SDL.Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK
    , SDL.Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER
    , SDL.Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER
    , SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_UP
    , SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN
    , SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT
    , SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT
    ] :: [SDL.Raw.GameControllerButton]

allControllerAxis =
    [ SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX
    , SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY
    , SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTX
    , SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTY
    , SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERLEFT
    , SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERRIGHT
    ] :: [SDL.Raw.GameControllerAxis]

gamepadManagerGamepadType :: SDL.Raw.JoystickID -> GamepadManager -> GamepadType
gamepadManagerGamepadType joystickId gamepadManager = case M.lookup joystickId (_gamepadsMap gamepadManager) of
    Nothing      -> Xbox360GamepadType
    Just gamepad -> _type gamepad

readGamepadType :: MonadIO m => SDL.Raw.Joystick -> ControlsConfig -> m GamepadType
readGamepadType joystick controlsCfg = do
    name <- T.pack <$> (liftIO . peekCString =<< SDL.Raw.joystickName joystick)
    let
        isXboxOne   = or $ map (`T.isInfixOf` name) (_xboxOneNameSubstrings controlsCfg)
        isPS4       = or $ map (`T.isInfixOf` name) (_ps4NameSubstrings controlsCfg)
        isPS5       = or $ map (`T.isInfixOf` name) (_ps5NameSubstrings controlsCfg)
        isPSX       = or $ map (`T.isInfixOf` name) (_psxNameSubstrings controlsCfg)
        isSwitchPro = or $ map (`T.isInfixOf` name) (_switchProNameSubstrings controlsCfg)

    return $ if
        | isXboxOne   -> XboxOneGamepadType
        | isPS4       -> PS4GamepadType
        | isPS5       -> PS5GamepadType
        | isPSX       -> PSXGamepadType
        | isSwitchPro -> SwitchProGamepadType
        | otherwise   -> Xbox360GamepadType

mkGamepad :: MonadIO m => JoystickIndex -> ControlsConfig -> m Gamepad
mkGamepad joystickIndex controlsCfg = do
    gameController <- SDL.Raw.gameControllerOpen joystickIndex
    joystick       <- SDL.Raw.gameControllerGetJoystick gameController
    gamepadType    <- readGamepadType joystick controlsCfg
    joystickId     <- SDL.Raw.joystickInstanceID joystick

    return $ Gamepad
        { _type           = gamepadType
        , _gameController = gameController
        , _joystickId     = joystickId
        , _heldButtons    = S.empty
        , _axisMap        = M.empty
        , _prevAxisMap    = M.empty
        }

updateGamepad :: MonadIO m => Gamepad -> m Gamepad
updateGamepad gamepad = do
    let controller = _gameController gamepad
    buttonStates  <- traverse (SDL.Raw.gameControllerGetButton controller) allControllerButtons
    axisValues    <- traverse (SDL.Raw.gameControllerGetAxis controller) allControllerAxis

    return $ gamepad
        { _heldButtons = S.fromList
            [ SDL.fromNumber btn
            | (btn, btnState) <- zip allControllerButtons buttonStates
            , btnState == sdlButtonStatePressed
            ]
        , _axisMap     = M.fromList $ zip allControllerAxis axisValues
        , _prevAxisMap = _axisMap gamepad
        }

gamepadAxisValue :: SDL.Raw.GameControllerAxis -> Gamepad -> Int
gamepadAxisValue axis gamepad = fromIntegral $ M.findWithDefault 0 axis (_axisMap gamepad)

gamepadPrevAxisValue :: SDL.Raw.GameControllerAxis -> Gamepad -> Int
gamepadPrevAxisValue axis gamepad = fromIntegral $ M.findWithDefault 0 axis (_prevAxisMap gamepad)

mkGamepadManager :: MonadIO m => ControlsConfig -> m GamepadManager
mkGamepadManager controlsCfg = do
    numJoysticks <- fromIntegral <$> SDL.numJoysticks
    gamepadsMap  <- M.fromList . catMaybes <$> if
        | numJoysticks <= 0 -> return []
        | otherwise         -> for [0..numJoysticks - 1] $ \i -> do
            SDL.Raw.isGameController i >>= \case
                False -> return Nothing
                True  -> do
                    gamepad <- mkGamepad i controlsCfg
                    return $ Just (_joystickId gamepad, gamepad)

    return $ GamepadManager
        { _gamepadsMap     = gamepadsMap
        , _buttonsPressed  = S.empty
        , _buttonsReleased = S.empty
        }

clearGamepadManager :: GamepadManager -> GamepadManager
clearGamepadManager gamepadManager = gamepadManager
    { _buttonsPressed  = S.empty
    , _buttonsReleased = S.empty
    }

updateGamepadManager :: MonadIO m => GamepadManager -> m GamepadManager
updateGamepadManager gamepadManager = do
    gamepadsMap <- traverse updateGamepad (_gamepadsMap gamepadManager)
    return $ gamepadManager {_gamepadsMap = gamepadsMap}

addGamepadManagerGamepad :: (ConfigsRead m, MonadIO m) => JoystickIndex -> GamepadManager -> m GamepadManager
addGamepadManagerGamepad joystickIndex gamepadManager = do
    controlsCfg <- readConfig _settings _controls
    gamepad     <- mkGamepad joystickIndex controlsCfg
    return $ gamepadManager {_gamepadsMap = M.insert (_joystickId gamepad) gamepad (_gamepadsMap gamepadManager)}

removeGamepadManagerGamepad :: MonadIO m => JoystickId -> GamepadManager -> m GamepadManager
removeGamepadManagerGamepad joystickId gamepadManager = case joystickId `M.lookup` gamepadsMap of
    Nothing      -> return gamepadManager
    Just gamepad -> do
        SDL.Raw.gameControllerClose $ _gameController gamepad
        return $ gamepadManager {_gamepadsMap = joystickId `M.delete` gamepadsMap}
    where gamepadsMap = _gamepadsMap gamepadManager

remapGamepadManagerGamepad :: MonadIO m => JoystickId -> GamepadManager -> m GamepadManager
remapGamepadManagerGamepad joystickId gamepadManager = case joystickId `M.lookup` gamepadsMap of
    Nothing      -> return gamepadManager
    Just gamepad -> do
        newJoystickId <- SDL.Raw.joystickInstanceID =<< SDL.Raw.gameControllerGetJoystick (_gameController gamepad)
        let
            gamepad'     = gamepad {_joystickId = newJoystickId}
            gamepadsMap' = joystickId `M.delete` gamepadsMap
        return $ gamepadManager {_gamepadsMap = M.insert newJoystickId gamepad' gamepadsMap'}
    where gamepadsMap = _gamepadsMap gamepadManager

gamepadManagerButtonPressed :: SDL.ControllerButton -> GamepadManager -> Bool
gamepadManagerButtonPressed button gamepadManager = button `S.member` _buttonsPressed gamepadManager

gamepadManagerButtonReleased :: SDL.ControllerButton -> GamepadManager -> Bool
gamepadManagerButtonReleased button gamepadManager = button `S.member` _buttonsReleased gamepadManager

gamepadManagerButtonHold :: SDL.ControllerButton -> GamepadManager -> Bool
gamepadManagerButtonHold button gamepadManager = or
    [ button `S.member` _heldButtons gamepad
    | gamepad <- M.elems $ _gamepadsMap gamepadManager
    ]

getAxisPosition :: (SDL.Raw.GameControllerAxis -> Gamepad -> Int) -> SDL.Raw.GameControllerAxis -> [Gamepad] -> Float
getAxisPosition _ _ []                             = 0.0
getAxisPosition axisValueF axis (gamepad:gamepads) =
    let
        isAxisValueDeadzone = \val -> abs val <= axisMaxDeadzoneValue

        axisValue                           = axisValueF axis gamepad
        isDeadzone
            | isAxisValueDeadzone axisValue = case axis of
                SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTX ->
                    isAxisValueDeadzone $ axisValueF SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTY gamepad
                SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTY ->
                    isAxisValueDeadzone $ axisValueF SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTX gamepad
                SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX ->
                    isAxisValueDeadzone $ axisValueF SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY gamepad
                SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY ->
                    isAxisValueDeadzone $ axisValueF SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX gamepad
                _                                 -> True
            | otherwise                     = False
    in if
        | isDeadzone -> getAxisPosition axisValueF axis gamepads
        | otherwise  -> realToFrac axisValue / realToFrac axisMaxValue

gamepadManagerAxisPosition :: SDL.Raw.GameControllerAxis -> GamepadManager -> Float
gamepadManagerAxisPosition axis gamepadManager = getAxisPosition gamepadAxisValue axis gamepads
    where gamepads = M.elems $ _gamepadsMap gamepadManager

gamepadManagerPrevAxisPosition :: SDL.Raw.GameControllerAxis -> GamepadManager -> Float
gamepadManagerPrevAxisPosition axis gamepadManager = getAxisPosition gamepadPrevAxisValue axis gamepads
    where gamepads = M.elems $ _gamepadsMap gamepadManager

gamepadManagerNegAxisPressed :: SDL.Raw.GameControllerAxis -> GamepadManager -> Bool
gamepadManagerNegAxisPressed axis gamepadManager =
    gamepadManagerAxisPosition axis gamepadManager <= (-0.5) &&
    gamepadManagerPrevAxisPosition axis gamepadManager > (-0.5)

gamepadManagerPosAxisPressed :: SDL.Raw.GameControllerAxis -> GamepadManager -> Bool
gamepadManagerPosAxisPressed axis gamepadManager =
    gamepadManagerAxisPosition axis gamepadManager >= 0.5 &&
    gamepadManagerPrevAxisPosition axis gamepadManager < 0.5

processGamepadManagerButtonEvent :: SDL.ControllerButtonEventData -> GamepadManager -> GamepadManager
processGamepadManagerButtonEvent controllerButtonEventData gamepadManager =
    case SDL.controllerButtonEventState controllerButtonEventData of
        SDL.ControllerButtonPressed      ->
            gamepadManager {_buttonsPressed = button `S.insert` _buttonsPressed gamepadManager}
        SDL.ControllerButtonReleased     ->
            gamepadManager {_buttonsReleased = button `S.insert` _buttonsReleased gamepadManager}
        SDL.ControllerButtonInvalidState -> gamepadManager
    where button = SDL.controllerButtonEventButton controllerButtonEventData
