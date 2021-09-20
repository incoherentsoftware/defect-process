module Window.InputState.Controls
    ( module Window.InputState.Alias
    , keyPressed
    , keyReleased
    , keyHold
    , keyPrevHold
    , keyNotHold
    , keyPrevNotHold
    , mousePressed
    , mouseReleased
    , mouseHold
    , mouseNotHold
    , mouseWheelScrollX
    , mouseWheelScrollY
    , gamepadPressed
    , gamepadReleased
    , gamepadHold
    , gamepadNotHold
    , gamepadAxis
    , gamepadPrevAxis
    , gamepadNegAxisPressed
    , gamepadPosAxisPressed
    , aliasPressed
    , aliasReleased
    , aliasHold
    , aliasNotHold
    , anyKeyPressed
    ) where

import Data.Maybe (isJust)
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Raw

import Window.InputState.Alias
import Window.InputState.GamepadManager
import Window.InputState.Types

keyPressed :: SDL.Scancode -> InputState -> Bool
keyPressed keyCode inputState
    | _inactive inputState = False
    | otherwise            = keyStatePressed || keyHoldPrevNotHold
        where
            -- SDL2 key press events seem to get dropped occasionally for some reason so
            -- do the hold/not-hold key checks in addition for redundancy
            keyStatePressed    = keyCode `elem` _keyStatePressed inputState
            keyHoldPrevNotHold = keyHold keyCode inputState && keyPrevNotHold keyCode inputState

keyReleased :: SDL.Scancode -> InputState -> Bool
keyReleased keyCode inputState
    | _inactive inputState = False
    | otherwise            = keyCode `elem` (_keyStateReleased inputState)

keyHold :: SDL.Scancode -> InputState -> Bool
keyHold keyCode inputState
    | _inactive inputState = False
    | otherwise            = (_keyHoldState inputState) keyCode

keyPrevHold :: SDL.Scancode -> InputState -> Bool
keyPrevHold keyCode inputState
    | _inactive inputState = False
    | otherwise            = (_keyHoldPrevState inputState) keyCode

keyNotHold :: SDL.Scancode -> InputState -> Bool
keyNotHold keyCode inputState
    | _inactive inputState = False
    | otherwise            = not $ keyHold keyCode inputState

keyPrevNotHold :: SDL.Scancode -> InputState -> Bool
keyPrevNotHold keyCode inputState
    | _inactive inputState = False
    | otherwise            = not $ keyPrevHold keyCode inputState

mousePressed :: SDL.MouseButton -> InputState -> Bool
mousePressed mouseButton inputState
    | _inactive inputState = False
    | otherwise            = pressed || pressed'
        where
            pressed           = mouseButton `elem` _mouseStatePressed inputState
            -- SDL2 mouse button events don't seem reliable if keyboard events are also being sent
            -- for some reason so fallback to this in addition
            buttonPrevNotHold = not $ (_mouseHoldPrevState inputState) mouseButton
            pressed'          = mouseHold mouseButton inputState && buttonPrevNotHold

mouseReleased :: SDL.MouseButton -> InputState -> Bool
mouseReleased mouseButton inputState
    | _inactive inputState = False
    | otherwise            = released || released'
        where
            released       = mouseButton `elem` _mouseStateReleased inputState
            -- SDL2 mouse button events unreliable, see above comment
            buttonPrevHold = _mouseHoldPrevState inputState $ mouseButton
            released'      = not (mouseHold mouseButton inputState) && buttonPrevHold

mouseHold :: SDL.MouseButton -> InputState -> Bool
mouseHold mouseButton inputState
    | _inactive inputState = False
    | otherwise            = _mouseHoldState inputState $ mouseButton

mouseNotHold :: SDL.MouseButton -> InputState -> Bool
mouseNotHold mouseButton inputState
    | _inactive inputState = False
    | otherwise            = not $ mouseHold mouseButton inputState

mouseWheelEventDataSign :: SDL.MouseWheelEventData -> Int
mouseWheelEventDataSign eventData = case SDL.mouseWheelEventDirection eventData of
    SDL.ScrollNormal  -> 1
    SDL.ScrollFlipped -> -1

mouseWheelScrollX :: InputState -> Int
mouseWheelScrollX inputState = case _mouseWheelEventData inputState of
    Nothing        -> 0
    Just eventData ->
        let SDL.V2 scrollX _ = SDL.mouseWheelEventPos eventData
        in fromIntegral scrollX * mouseWheelEventDataSign eventData

mouseWheelScrollY :: InputState -> Int
mouseWheelScrollY inputState = case _mouseWheelEventData inputState of
    Nothing        -> 0
    Just eventData ->
        let SDL.V2 _ scrollY = SDL.mouseWheelEventPos eventData
        in fromIntegral scrollY * mouseWheelEventDataSign eventData

gamepadPressed :: SDL.ControllerButton -> InputState -> Bool
gamepadPressed button inputState
    | _inactive inputState = False
    | otherwise            = gamepadManagerButtonPressed button (_gamepadManager inputState)

gamepadReleased :: SDL.ControllerButton  -> InputState -> Bool
gamepadReleased button inputState
    | _inactive inputState = False
    | otherwise            = gamepadManagerButtonReleased button (_gamepadManager inputState)

gamepadHold :: SDL.ControllerButton -> InputState -> Bool
gamepadHold button inputState
    | _inactive inputState = False
    | otherwise            = gamepadManagerButtonHold button (_gamepadManager inputState)

gamepadNotHold :: SDL.ControllerButton -> InputState -> Bool
gamepadNotHold button inputState
    | _inactive inputState = False
    | otherwise            = not $ gamepadHold button inputState

gamepadAxis :: SDL.Raw.GameControllerAxis -> InputState -> Float
gamepadAxis axis inputState = gamepadManagerAxisPosition axis (_gamepadManager inputState)

gamepadPrevAxis :: SDL.Raw.GameControllerAxis -> InputState -> Float
gamepadPrevAxis axis inputState = gamepadManagerPrevAxisPosition axis (_gamepadManager inputState)

gamepadNegAxisPressed :: SDL.Raw.GameControllerAxis -> InputState -> Bool
gamepadNegAxisPressed axis inputState = gamepadManagerNegAxisPressed axis (_gamepadManager inputState)

gamepadPosAxisPressed :: SDL.Raw.GameControllerAxis -> InputState -> Bool
gamepadPosAxisPressed axis inputState = gamepadManagerPosAxisPressed axis (_gamepadManager inputState)

aliasPressed :: InputAlias -> InputState -> Bool
aliasPressed alias inputState
    | _inactive inputState = False
    | otherwise            =
        let
            aliasData         = toInputAliasData alias (_inputAliasRawDataMap inputState)
            keyPress          = or [keyPressed key inputState | key <- _keys aliasData]
            mousePress        = or [mousePressed btn inputState | btn <- _mouseButtons aliasData]
            mouseScrollXPress = or
                [ mouseWheelScrollX inputState `cmp` scrollX
                | scrollX <- _mouseWheelScrollXs aliasData
                , let cmp = if scrollX < 0 then (<=) else (>=)
                ]
            mouseScrollYPress = or
                [ mouseWheelScrollY inputState `cmp` scrollY
                | scrollY <- _mouseWheelScrollYs aliasData
                , let cmp = if scrollY < 0 then (<=) else (>=)
                ]
            padPress          = or [gamepadPressed btn inputState | btn <- _gamepadButtons aliasData]
            negAxisPress      = or [gamepadNegAxisPressed axis inputState | axis <- _negAxes aliasData]
            posAxisPress      = or [gamepadPosAxisPressed axis inputState | axis <- _posAxes aliasData]
        in or [keyPress, mousePress, mouseScrollXPress, mouseScrollYPress, padPress, negAxisPress, posAxisPress]

aliasReleased :: InputAlias -> InputState -> Bool
aliasReleased alias inputState
    | _inactive inputState = False
    | otherwise            =
        let
            aliasData      = toInputAliasData alias (_inputAliasRawDataMap inputState)
            keyRelease     = or [keyReleased key inputState | key <- _keys aliasData]
            mouseRelease   = or [mouseReleased btn inputState | btn <- _mouseButtons aliasData]
            padRelease     = or [gamepadReleased btn inputState | btn <- _gamepadButtons aliasData]
            negAxisRelease = or
                [ gamepadAxis axis inputState >= (-0.5) && gamepadPrevAxis axis inputState < (-0.5)
                | axis <- _negAxes aliasData
                ]
            posAxisRelease = or
                [ gamepadAxis axis inputState <= 0.5 && gamepadPrevAxis axis inputState > 0.5
                | axis <- _posAxes aliasData
                ]
        in or [keyRelease, mouseRelease, padRelease, negAxisRelease, posAxisRelease]

aliasHold :: InputAlias -> InputState -> Bool
aliasHold alias inputState
    | _inactive inputState = False
    | otherwise            =
        let
            aliasData  = toInputAliasData alias (_inputAliasRawDataMap inputState)
            keyHld     = or [keyHold key inputState | key <- _keys aliasData]
            mouseHld   = or [mouseHold btn inputState | btn <- _mouseButtons aliasData]
            padHld     = or [gamepadHold btn inputState | btn <- _gamepadButtons aliasData]
            negAxisHld = or [gamepadAxis axis inputState <= (-0.5) | axis <- _negAxes aliasData]
            posAxisHld = or [gamepadAxis axis inputState >= 0.5 | axis <- _posAxes aliasData]
        in or [keyHld, mouseHld, padHld, negAxisHld, posAxisHld]

aliasNotHold :: InputAlias -> InputState -> Bool
aliasNotHold alias inputState
    | _inactive inputState = False
    | otherwise            = not $ aliasHold alias inputState

anyKeyPressed :: InputState -> Bool
anyKeyPressed = isJust . _pressedInputRawData
