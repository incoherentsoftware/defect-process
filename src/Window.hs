module Window
    ( module Window.Types
    , module Window.Graphics
    , module Window.InputState
    , mkWindow
    , updateWindow
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (traverse_)
import qualified SDL
import qualified SDL.Font

import Configs
import Configs.All.Settings
import Configs.All.Settings.Render
import Msg
import Util
import Window.Graphics
import Window.InputState
import Window.Types

mkWindow :: MonadIO m => Configs -> m Window
mkWindow cfgs = do
    SDL.initialize [SDL.InitVideo, SDL.InitEvents, SDL.InitGameController]
    SDL.Font.initialize

    let
        settingsCfg = _settings cfgs
        renderCfg   = _render settingsCfg
        winWidth    = _winWidth renderCfg
        winHeight   = _winHeight renderCfg

    graphics   <- mkGraphics winWidth winHeight "Defect Process" settingsCfg
    inputState <- mkInputState winWidth winHeight settingsCfg

    return $ Window
        { _graphics   = graphics
        , _inputState = inputState
        , _closed     = False
        }

updateWindow :: (ConfigsRead m, GraphicsRead m, MonadIO m, MsgsWrite WindowMsgsPhase m) => Window -> m Window
updateWindow window = do
    sdlEvents <- map SDL.eventPayload <$> SDL.pollEvents

    processWindowGraphicsEvents sdlEvents
    let inputEvents = processWindowInputEvents sdlEvents
    inputState     <- updateInputState inputEvents (_inputState window)

    return $ window
        { _inputState = inputState
        , _closed     = _closed window || QuitEvent `elem` inputEvents
        }

processWindowGraphicsEvents
    :: forall m.
       (ConfigsRead m, GraphicsRead m, MonadIO m, MsgsWrite WindowMsgsPhase m)
    => [SDL.EventPayload]
    -> m ()
processWindowGraphicsEvents sdlEvents = traverse_ processEvent sdlEvents
    where
        processEvent :: SDL.EventPayload -> m ()
        processEvent = \case
            SDL.WindowMovedEvent _ -> whenM isGraphicsDisplayIndexChanged $
                writeMsgs
                    [ mkMsg ConsoleMsgUpdateRenderConfigWinDisplayIndex
                    , mkMsg ConsoleMsgSaveSettings
                    ]
            _                      -> return ()

processWindowInputEvents :: [SDL.EventPayload] -> [InputEvent]
processWindowInputEvents sdlEvents = foldr processEvent [] sdlEvents
    where
        processEvent :: SDL.EventPayload -> [InputEvent] -> [InputEvent]
        processEvent sdlEvent !inputEvents = case sdlEvent of
            SDL.QuitEvent                          -> QuitEvent:inputEvents
            SDL.KeyboardEvent keyEventData         -> KeyEvent keyEventData:inputEvents
            SDL.MouseButtonEvent mouseBtnEventData -> MouseButtonEvent mouseBtnEventData:inputEvents
            SDL.MouseMotionEvent mouseMtnEventData -> MouseMotionEvent mouseMtnEventData:inputEvents
            SDL.MouseWheelEvent mouseWhlEventData  -> MouseWheelEvent mouseWhlEventData:inputEvents
            SDL.TextInputEvent textEventData       -> TextEvent textEventData:inputEvents

            SDL.ControllerDeviceEvent controllerDeviceEventData ->
                ControllerDeviceEvent controllerDeviceEventData:inputEvents
            SDL.ControllerButtonEvent controllerBtnEventData    ->
                ControllerButtonEvent controllerBtnEventData:inputEvents
            SDL.ControllerAxisEvent controllerBtnEventData      ->
                ControllerAxisEvent controllerBtnEventData:inputEvents

            _ -> inputEvents
