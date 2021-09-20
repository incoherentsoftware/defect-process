module Game
    ( runGame
    ) where

import Control.Monad (when)
import Data.Either   (fromRight)
import System.Mem    (performGC)
import qualified Data.Text as T

import AppEnv
import Async.BackgroundThread
import Async.MainThread
import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Configs.All.Settings.Render
import Console
import Constants
import Game.Types
import Menu
import Msg.Phase
import SaveFiles
import Util
import Util.Time
import Window
import World
import World.Draw
import World.Main

initialConsoleCmdLines =
    [ "defaultGraphicsFallback"
    , "load settings"
    , "load progress"
    ] :: [T.Text]

consoleAutoexecPath = "data/autoexec" :: FilePath

mkGame :: AppEnv BaseMsgsPhase Game
mkGame = do
    initFmod

    gameMode <- readSettingsConfig _debug _startingMode
    case gameMode of
        WorldMode -> setGraphicsCursor _crosshair
        _         -> setGraphicsCursor _menu

    inputState <- readInputState
    cfgs       <- readConfigs
    cfgs'      <- fromRight cfgs <$> readSaveFilesProgress cfgs

    withAppEnvReadData inputState cfgs' $
        withMsgsPhase @SetupMsgsPhase $
            Game <$>
            pure gameMode <*>
            pure gameMode <*>
            mkWorld <*>
            mkMenu <*>
            mkTime <*>
            mkConsole <*>
            pure False

stepGame :: Window -> Configs -> Game -> AppEnv BaseMsgsPhase Game
stepGame window cfgs game =
    let
        inputState          = _inputState (window :: Window)
        activeConsole       = _active $ _console (game :: Game)
        inputState'
            | activeConsole = inactivateInputState inputState
            | otherwise     = inputState
    in do
        updateFmod

        withAppEnvReadData inputState' cfgs $ case _mode game of
            WorldMode       -> worldMain game
            PauseMenuMode   -> pauseMenuMain game
            MainMenuMode    -> mainMenuMain game
            UnlocksMenuMode -> unlocksMenuMain game

updateGame :: Window -> Game -> AppEnv BaseMsgsPhase (Window, Configs, Game)
updateGame window game =
    let
        step :: Window -> Configs -> Game -> AppEnv BaseMsgsPhase (Window, Configs, Game)
        step win cfgs gm =
            let
                time     = _time gm
                diffSecs = _diffSecs time
            in if
                | diffSecs < timeStep -> return (win, cfgs, gm)
                | otherwise           -> do
                    clearAppEnvMsgs
                    win'        <- withMsgsPhase @WindowMsgsPhase (updateWindow win)
                    gm'         <- stepGame win' cfgs gm
                    asyncStatus <- loadMainThreadAsyncData

                    time' <- do
                        elapsedTime <- updateTime time
                        return $ case asyncStatus of
                            MainThreadLoadedAsyncData
                                | _diffSecs elapsedTime > timeStep -> updateTimeDiffSecs timeStep elapsedTime
                            _                                      -> updateTimeDiffSecs (diffSecs - timeStep) time

                    result <- withAppEnvReadData (_inputState win') cfgs $
                        let
                            world   = _world (gm' :: Game)
                            console = _console (gm' :: Game)
                        in withMsgsPhase @ConsoleMsgsPhase (updateConsole world console)

                    let
                        win'' = win' {_inputState = clearInputState $ _inputState win'}
                        cfgs' = _configs (result :: ConsoleUpdateResult)

                    step win'' cfgs' $ gm'
                        { _world   = _world (result :: ConsoleUpdateResult)
                        , _time    = time'
                        , _console = _console (result :: ConsoleUpdateResult)
                        }
    in do
        cfgs <- readConfigs
        time <- updateTime $ _time game
        step window cfgs (game {_time = time})

drawGame :: Lerp -> Game -> AppEnv DrawMsgsPhase ()
drawGame lerp game = do
    setGraphicsLerp lerp

    case _mode game of
        WorldMode       -> drawWorld game
        PauseMenuMode   -> drawPauseMenu game
        MainMenuMode    -> drawMainMenu game
        UnlocksMenuMode -> drawUnlocksMenu game

    drawConsole $ _console (game :: Game)

    displayGraphics

gameMain :: AppEnvData -> Window -> Game -> IO ()
gameMain appEnvData window game = do
    (window', configs, game') <- runAppEnv appEnvData $ do
        (win, cfgs, gm) <- updateGame window game

        when (_closed win || _quit gm) $
            freeGraphicsAndExit

        let
            diffSecs                  = _diffSecs $ _time gm
            world                     = _world (gm :: Game)
            lerp
                | worldIsHitlag world = Lerp 0.0
                | otherwise           = Lerp $ diffSecs / timeStep
        withMsgsPhase @DrawMsgsPhase (drawGame lerp gm)

        return (win, cfgs, gm)

    when (_manualPerformGC ( _render (_settings configs))) $
        performGC

    let
        inputState  = _inputState (window' :: Window)
        appEnvData' = updateAppEnvReadData inputState configs appEnvData
    gameMain appEnvData' window' game'

runGame :: AppEnvData -> Window -> IO ()
runGame appEnvData window = do
    forkBackgroundThread appEnvData
    game <- runAppEnv appEnvData mkGame

    result <- runAppEnv appEnvData $
        let
            world   = _world (game :: Game)
            console = _console (game :: Game)
        in withMsgsPhase @ConsoleMsgsPhase $ do
            consoleAutoexecPath' <- translateResourcePath consoleAutoexecPath
            runConsoleCommandsAndFile initialConsoleCmdLines consoleAutoexecPath' world console

    let
        configs     = _configs (result :: ConsoleUpdateResult)
        inputState  = _inputState window
        appEnvData' = updateAppEnvReadData inputState configs appEnvData
        console     = printConsole " " (_console (result :: ConsoleUpdateResult))  -- add newline

        game' = game
            { _world   = _world (result :: ConsoleUpdateResult)
            , _console = console
            } :: Game

    gameMain appEnvData' window game'
