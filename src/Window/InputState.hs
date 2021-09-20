module Window.InputState
    ( module Window.InputState.Controls
    , module Window.InputState.Event
    , module Window.InputState.RawData
    , module Window.InputState.Types
    , mkInputState
    , freeInputState
    , inactivateInputState
    , clearInputState
    , updateInputState
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execStateT, get, lift, modify, put)
import Data.Foldable          (foldlM)
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Internal.Numbered as SDL.Internal

import Configs
import Configs.All.Settings
import Configs.All.Settings.Controls
import Configs.All.Settings.Debug
import Constants
import Util
import Window.Graphics
import Window.Graphics.Renderer
import Window.InputState.Controls
import Window.InputState.Event
import Window.InputState.GamepadManager
import Window.InputState.RawData
import Window.InputState.Types

mkInputState :: MonadIO m => Int -> Int -> SettingsConfig -> m InputState
mkInputState winWidth winHeight settingsCfg =
    let
        mousePos             = Pos2 (fromIntegral winWidth / 2.0) (fromIntegral winHeight / 2.0)
        controlsCfg          = _controls settingsCfg
        inputAliasRawDataMap = _inputAliasRawDataMap (controlsCfg :: ControlsConfig)
    in do
        gamepadManager <- mkGamepadManager controlsCfg

        return $ InputState
            { _inactive             = False
            , _keyStatePressed      = []
            , _keyStateReleased     = []
            , _keyHoldState         = const False
            , _keyHoldPrevState     = const False
            , _mouseStatePressed    = []
            , _mouseStateReleased   = []
            , _mouseHoldState       = const False
            , _mouseHoldPrevState   = const False
            , _mouseWheelEventData  = Nothing
            , _mousePos             = mousePos
            , _mouseWorldPos        = mousePos
            , _mouseMoved           = False
            , _gamepadManager       = gamepadManager
            , _textBuffer           = ""
            , _lastUsedInputType    = MouseKbInputType
            , _lastGamepadType      = Xbox360GamepadType
            , _pressedInputRawData  = Nothing
            , _inputAliasRawDataMap = inputAliasRawDataMap
            }

freeInputState :: MonadIO m => InputState -> m ()
freeInputState _ = return ()

inactivateInputState :: InputState -> InputState
inactivateInputState inputState = inputState {_inactive = True}

clearInputState :: InputState -> InputState
clearInputState inputState = inputState
    { _keyStatePressed     = []
    , _keyStateReleased    = []
    , _mouseStatePressed   = []
    , _mouseStateReleased  = []
    , _mouseWheelEventData = Nothing
    , _mouseMoved          = False
    , _gamepadManager      = clearGamepadManager $ _gamepadManager inputState
    , _textBuffer          = ""
    , _pressedInputRawData = Nothing
    }

processInputEvents :: forall m. (ConfigsRead m, MonadIO m) => [InputEvent] -> InputState -> m InputState
processInputEvents events inputState = foldlM processEvent inputState events
    where
        processEvent :: InputState -> InputEvent -> m InputState
        processEvent !inSt e = case e of
            KeyEvent keyEventData
                | SDL.keyboardEventRepeat keyEventData -> return inSt
                | otherwise                            ->
                    let
                        scancode            = SDL.keysymScancode $ SDL.keyboardEventKeysym keyEventData
                        inputMotion         = SDL.keyboardEventKeyMotion keyEventData
                        keyStatePressed     = _keyStatePressed inSt
                        keyStateReleased    = _keyStateReleased inSt
                        pressedInputRawData = case inputMotion of
                            SDL.Pressed  -> Just $ KeyRawData (SDL.Internal.toNumber scancode)
                            SDL.Released -> _pressedInputRawData inSt

                        inSt' = inSt
                            { _lastUsedInputType   = MouseKbInputType
                            , _pressedInputRawData = pressedInputRawData
                            }
                    in return $ case inputMotion of
                        SDL.Pressed  -> inSt' {_keyStatePressed = scancode:keyStatePressed}
                        SDL.Released -> inSt' {_keyStateReleased = scancode:keyStateReleased}

            MouseButtonEvent mouseButtonEventData ->
                let
                    btn                 = SDL.mouseButtonEventButton mouseButtonEventData
                    inputMotion         = SDL.mouseButtonEventMotion mouseButtonEventData
                    mouseStatePressed   = _mouseStatePressed inSt
                    mouseStateReleased  = _mouseStateReleased inSt
                    pressedInputRawData = case inputMotion of
                        SDL.Pressed  -> Just $ MouseButtonRawData (SDL.Internal.toNumber btn)
                        SDL.Released -> _pressedInputRawData inSt

                    inSt' = inSt
                        { _lastUsedInputType   = MouseKbInputType
                        , _pressedInputRawData = pressedInputRawData
                        }
                in return $ case inputMotion of
                    SDL.Pressed  -> inSt' {_mouseStatePressed = btn:mouseStatePressed}
                    SDL.Released -> inSt' {_mouseStateReleased = btn:mouseStateReleased}

            MouseMotionEvent _ -> return $ inSt
                { _mouseMoved        = True
                , _lastUsedInputType = MouseKbInputType
                }

            MouseWheelEvent mouseWheelEventData ->
                let
                    inSt'   = inSt
                        { _mouseWheelEventData = Just mouseWheelEventData
                        , _lastUsedInputType   = MouseKbInputType
                        }
                    scrollX = mouseWheelScrollX inSt'
                    scrollY = mouseWheelScrollY inSt'

                    pressedInputRawData = if
                        | scrollX < 0 -> Just $ MouseWheelScrollXRawData (-1)
                        | scrollX > 0 -> Just $ MouseWheelScrollXRawData 1
                        | scrollY < 0 -> Just $ MouseWheelScrollYRawData (-1)
                        | scrollY > 0 -> Just $ MouseWheelScrollYRawData 1
                        | otherwise   -> _pressedInputRawData inSt'
                in return $ inSt' {_pressedInputRawData = pressedInputRawData}

            ControllerDeviceEvent controllerDeviceEventData -> do
                gamepadManager' <- case SDL.controllerDeviceEventConnection controllerDeviceEventData of
                    SDL.ControllerDeviceAdded    ->
                        let joystickIndex = fromIntegral $ SDL.controllerDeviceEventWhich controllerDeviceEventData
                        in addGamepadManagerGamepad joystickIndex gamepadManager
                    SDL.ControllerDeviceRemoved  ->
                        let joystickId = SDL.controllerDeviceEventWhich controllerDeviceEventData
                        in removeGamepadManagerGamepad joystickId gamepadManager
                    SDL.ControllerDeviceRemapped ->
                        let joystickId = SDL.controllerDeviceEventWhich controllerDeviceEventData
                        in remapGamepadManagerGamepad joystickId gamepadManager
                return $ inSt {_gamepadManager = gamepadManager'}

            ControllerButtonEvent controllerButtonEventData ->
                let
                    gamepadManager' = processGamepadManagerButtonEvent controllerButtonEventData gamepadManager
                    joystickId      = SDL.controllerButtonEventWhich controllerButtonEventData
                    gamepadType     = gamepadManagerGamepadType joystickId gamepadManager'

                    inSt' = inSt
                        { _gamepadManager      = gamepadManager'
                        , _lastUsedInputType   = GamepadInputType
                        , _lastGamepadType     = gamepadType
                        }
                in case SDL.controllerButtonEventState controllerButtonEventData of
                    SDL.ControllerButtonPressed      ->
                        let
                            btn                 = SDL.controllerButtonEventButton controllerButtonEventData
                            pressedInputRawData = Just $ GamepadButtonRawData (SDL.Internal.toNumber btn)
                        in return $ inSt' {_pressedInputRawData = pressedInputRawData}
                    SDL.ControllerButtonReleased     -> return inSt'
                    SDL.ControllerButtonInvalidState -> return inSt

            ControllerAxisEvent controllerAxisEventData ->
                let
                    axisValue   = fromIntegral $ SDL.controllerAxisEventValue controllerAxisEventData
                    axis        = fromIntegral $ SDL.controllerAxisEventAxis controllerAxisEventData
                    joystickId  = SDL.controllerAxisEventWhich controllerAxisEventData
                    gamepadType = gamepadManagerGamepadType joystickId (_gamepadManager inSt)

                    pressedInputRawData
                        | gamepadManagerNegAxisPressed axis gamepadManager = Just $ GamepadNegAxisRawData axis
                        | gamepadManagerPosAxisPressed axis gamepadManager = Just $ GamepadPosAxisRawData axis
                        | otherwise                                        = _pressedInputRawData inSt
                in if
                    | abs axisValue > axisMaxDeadzoneValue -> return $ inSt
                        { _lastUsedInputType   = GamepadInputType
                        , _lastGamepadType     = gamepadType
                        , _pressedInputRawData = pressedInputRawData
                        }
                    | otherwise                            -> return inSt

            TextEvent textInputEventData ->
                let textBuffer = _textBuffer inSt `T.append` SDL.textInputEventText textInputEventData
                in return $ inSt {_textBuffer = textBuffer}

            -- handled in updateWindow
            QuitEvent -> return inSt

            where gamepadManager = _gamepadManager inSt

updateInputState :: (ConfigsRead m, GraphicsRead m, MonadIO m) => [InputEvent] -> InputState -> m InputState
updateInputState inputEvents inputState = do
    keysState                    <- SDL.getKeyboardState
    mouseState                   <- SDL.getMouseButtons
    gamepadManager               <- updateGamepadManager $ _gamepadManager inputState
    SDL.P (SDL.V2 mouseX mouseY) <- SDL.getAbsoluteMouseLocation
    cameraPos                    <- getCameraPos
    settingsCfg                  <- _settings <$> readConfigs

    SDL.Rectangle (SDL.P (SDL.V2 offsetX offsetY)) (SDL.V2 w h) <- do
        (renderW, renderH) <- getGraphicsWindowSize
        renderer           <- _renderer <$> getGraphics
        rendererDestRect renderW renderH renderer

    let
        mouseX'  = realToFrac (mouseX - offsetX) * (virtualRenderWidth / fromIntegral w)
        mouseY'  = realToFrac (mouseY - offsetY) * (virtualRenderHeight / fromIntegral h)
        mousePos = Pos2 mouseX' mouseY'

        inputAliasRawDataMap = _inputAliasRawDataMap (_controls (settingsCfg :: SettingsConfig) :: ControlsConfig)


    flip execStateT inputState $ do
        modify $ \inSt -> inSt
            { _keyHoldPrevState     = _keyHoldState inSt
            , _keyHoldState         = keysState
            , _mouseHoldPrevState   = _mouseHoldState inSt
            , _mouseHoldState       = mouseState
            , _mouseWheelEventData  = Nothing
            , _mousePos             = mousePos
            , _mouseWorldPos        = mousePos `vecAdd` cameraPos
            , _gamepadManager       = gamepadManager
            , _pressedInputRawData  = Nothing
            , _inputAliasRawDataMap = inputAliasRawDataMap
            }

        get >>= lift . processInputEvents inputEvents >>= put

        lift (readSettingsConfig _debug _forceLastUsedInputType) >>= \case
            Nothing -> return ()
            Just t  -> modify $ \inSt -> inSt {_lastUsedInputType = t}
