module Console
    ( module Console.Types
    , mkConsole
    , drawConsole
    , updateConsole
    , printConsole
    , runConsoleCommandsFile
    , runConsoleCommandsAndFile
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (get, lift, liftIO, modify, put, runStateT)
import Data.Foldable          (foldlM, foldrM, for_)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe, listToMaybe)
import Data.Traversable       (for)
import System.Directory       (doesFileExist)
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified SDL
import qualified SDL.Internal.Numbered as SDL.Internal

import AppEnv
import Configs
import Configs.All.Settings
import Configs.All.Settings.Audio
import Configs.All.Settings.Controls
import Configs.All.Settings.Debug
import Configs.All.Settings.Render
import Console.Commands
import Console.Types
import Console.Util
import Constants
import Msg
import SaveFiles
import Util
import Window
import World.Types
import World.ZIndex

textStartX       = 15.0                  :: Float
textStartYOffset = 20.0                  :: Float
textColor        = Color 255 255 255 255 :: Color
rectColor        = Color 50 50 50 155    :: Color

maxTextLines    = 28   :: Int
textLinesOffset = 18.0 :: Float

mkConsoleUpdateResult :: Console -> World -> Configs -> ConsoleUpdateResult
mkConsoleUpdateResult console world cfgs = ConsoleUpdateResult
    { _console = console
    , _world   = world
    , _configs = cfgs
    , _output  = []
    }

mkConsole :: AppEnv SetupMsgsPhase Console
mkConsole = do
    drwText      <- mkDisplayText ">" Font16 textColor
    drwTextLines <- for [1..maxTextLines] (const $ mkDisplayText "" Font16 textColor)

    let
        commands = M.fromList [(T.toLower name, cmd) | (names, cmd) <- consoleCommands, name <- names]
        console  = Console
            { _active               = False
            , _displayText          = drwText
            , _displayTextLines     = drwTextLines
            , _commands             = commands
            , _commandsHistoryIndex = Nothing
            , _commandBinds         = M.empty
            }

    cfgs          <- readConfigs
    (_, console') <- processMessages cfgs console
    return console'

drawConsole :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Console -> m ()
drawConsole console = when (_active console) $ do
    setCameraSpace CameraScreenSpace

    let rectHeight = virtualRenderHeight / 2.0
    drawRect zeroPos2 virtualRenderWidth rectHeight rectColor consoleBackgroundZIndex

    let
        textStartY   = rectHeight - textStartYOffset
        textStartPos = Pos2 textStartX textStartY
        text         = _displayText console
        textLines    = _displayTextLines console

    drawDisplayText textStartPos consoleTextZIndex text
    for_ (zip textLines [1..]) $ \(t, n) ->
        let pos = Pos2 textStartX (textStartY - textLinesOffset * n)
        in drawDisplayText pos consoleTextZIndex t

    setCameraSpace CameraWorldSpace

tokenizeConsoleCommandText :: T.Text -> [T.Text]
tokenizeConsoleCommandText text = tokenize "" (T.words text)
    where
        tokenize :: T.Text -> [T.Text] -> [T.Text]
        tokenize "" []         = []
        tokenize prependTxt [] = [prependTxt]
        tokenize prependTxt (txt:txts)
            | prependTxt == "" && isQuoteStart && isQuoteEnd =
                let txt' = T.dropEnd 1 (T.drop 1 txt)
                in txt':tokenize "" txts

            | prependTxt == "" && isQuoteStart =
                let txt' = fromMaybe txt (T.stripPrefix "\"" txt)
                in tokenize txt' txts

            | prependTxt /= "" = if
                | "\"" `T.isSuffixOf` txt ->
                    let txt' = fromMaybe txt (T.stripSuffix "\"" txt)
                    in (prependTxt <> " " <> txt'):tokenize "" txts
                | otherwise               -> tokenize (prependTxt <> " " <> txt) txts

            | otherwise = txt:tokenize prependTxt txts

            where
                isQuoteStart = "\"" `T.isPrefixOf` txt
                isQuoteEnd   = "\"" `T.isSuffixOf` txt

execConsoleCommand :: T.Text -> World -> Console -> AppEnv ConsoleMsgsPhase ConsoleCommandResult
execConsoleCommand text world console
    -- #foo indicates a comment
    | "#" `T.isPrefixOf` text = return $ NoUpdateResult ""
    | otherwise               =
        let
            -- Ignore the > prompt char and whitespace
            text'   = T.strip $ fromMaybe text (T.stripPrefix ">" text)
            tokens  = tokenizeConsoleCommandText text'
            cmdName = maybe "" T.toLower (listToMaybe tokens)
            args    = safeTail tokens
            cmds    = _commands console
        in case cmdName `M.lookup` cmds of
            Just cmd -> cmd args world
            Nothing  -> return $ NoUpdateResult ""

runConsolePrompt :: World -> Console -> AppEnv ConsoleMsgsPhase ConsoleUpdateResult
runConsolePrompt world console =
    let
        dspText  = _displayText console
        text     = T.stripEnd $ _text (dspText :: DisplayText)
        console' = printConsole text console
    in do
        cmdResult <- runConsoleCommands [text] world console'

        return $ cmdResult
            { _console = (_console cmdResult)
                { _displayText          = updateDisplayText ">" dspText
                , _commandsHistoryIndex = Nothing
                }
            }

consoleCommandsHistory :: ConsoleCommandHistoryOffset -> Console -> (ConsoleCommandHistoryIndex, T.Text)
consoleCommandsHistory historyOffset console = (historyIndex, line)
    where
        textLines = map (_text :: DisplayText -> T.Text) (_displayTextLines console)
        index     = case _commandsHistoryIndex console of
            Nothing  -> case historyOffset of
                PrevHistory -> 0
                NextHistory -> max 0 (length textLines - 1)
            Just idx -> case historyOffset of
                PrevHistory -> idx + 1
                NextHistory -> max 0 (idx - 1)
        cmdLines  = nubAdjacent $ filter (\t -> ">" `T.isPrefixOf` t && T.length t > 1) textLines

        (historyIndex, line) = case drop index cmdLines of
            []     -> case maybeLast cmdLines of
                Nothing -> (Nothing, ">")
                Just ln ->
                    let idx = max 0 (length cmdLines - 1)
                    in (Just idx, ln)
            (ln:_) -> (Just index, ln)

processMessages :: forall p. AllowMsgRead p ConsoleMsgPayload => Configs -> Console -> AppEnv p (Configs, Console)
processMessages configs console = foldrM processMsg (configs, console) =<< readMsgs
    where
        processMsg
            :: (GraphicsReadWrite m, MonadIO m)
            => ConsoleMsgPayload
            -> (Configs, Console)
            -> m (Configs, Console)
        processMsg d (cfgs, cns) = case d of
            ConsoleMsgPrint txt       -> return (cfgs, printConsole ("| " <> txt) cns)
            ConsoleMsgPrintString str -> return (cfgs, printConsole ("| " <> T.pack str) cns)

            ConsoleMsgSetControlsInputAlias inputAlias prevInputRawData inputRawData ->
                return (setControlsInputAliasConsole inputAlias prevInputRawData inputRawData cfgs, cns)

            ConsoleMsgSetGraphicsResolution winWidth winHeight ->
                (,cns) <$> setGraphicsResolutionConsole winWidth winHeight cfgs
            ConsoleMsgSetGraphicsWindowMode winMode            ->
                (,cns) <$> setGraphicsWindowModeConsole winMode cfgs
            ConsoleMsgUpdateRenderConfigWinDisplayIndex        ->
                (,cns) <$> updateRenderConfigWinDisplayIndexConsole cfgs

            ConsoleMsgSetSoundVolume vol            -> (,cns) <$> setSoundVolumeConsole vol cfgs
            ConsoleMsgSetMusicVolume vol            -> (,cns) <$> setMusicVolumeConsole vol cfgs
            ConsoleMsgSetBattleMusic musicType      -> return (setBattleMusicConsole musicType cfgs, cns)
            ConsoleMsgSetExplorationMusic musicType -> return (setExplorationMusicConsole musicType cfgs, cns)

            ConsoleMsgSetEnemyHealth enemyHealthTxt      -> return (setEnemyHealthConsole enemyHealthTxt cfgs, cns)
            ConsoleMsgSetPauseMenuHints isPauseMenuHints ->
                return (setPauseMenuHintsConsole isPauseMenuHints cfgs, cns)

            ConsoleMsgAddProgressTotalGold gold -> return (addProgressTotalGoldConsole gold cfgs, cns)

            ConsoleMsgUnlockWeapon cost typ         -> return (unlockWeaponConsole cost typ cfgs, cns)
            ConsoleMsgUnlockGun cost typ            -> return (unlockGunConsole cost typ cfgs, cns)
            ConsoleMsgUnlockMovementSkill cost typ  -> return (unlockMovementSkillConsole cost typ cfgs, cns)
            ConsoleMsgUnlockSecondarySkill cost typ -> return (unlockSecondarySkillConsole cost typ cfgs, cns)
            ConsoleMsgUnlockMusic cost typ          -> return (unlockMusicConsole cost typ cfgs, cns)

            ConsoleMsgRestoreDefaultSettingsControls -> do
                defaultControlsCfg <- (_controls :: SettingsConfig -> ControlsConfig) <$> loadSettingsConfig
                return (applyControlsConfigConsole defaultControlsCfg cfgs, cns)

            ConsoleMsgRestoreDefaultSettingsRender -> do
                defaultRenderCfg <- (_render :: SettingsConfig -> RenderConfig) <$> loadSettingsConfig
                cfgs'            <- applyRenderConfigConsole defaultRenderCfg cfgs
                maybe (cfgs', cns) (,cns) <$> applyFallbackRenderConfigConsole cfgs'

            ConsoleMsgRestoreDefaultSettingsAudio -> do
                defaultAudioCfg <- (_audio :: SettingsConfig -> AudioConfig) <$> loadSettingsConfig
                (,cns) <$> applyAudioConfigConsole defaultAudioCfg cfgs

            ConsoleMsgRestoreDefaultSettingsGame -> do
                defaultDebugCfg <- (_debug :: SettingsConfig -> DebugConfig) <$> loadSettingsConfig
                let
                    settingsCfg = _settings cfgs
                    cfgs'       = cfgs
                        { _settings = settingsCfg
                            { _debug = (_debug settingsCfg)
                                { _enemiesDamageMultiplier = _enemiesDamageMultiplier defaultDebugCfg
                                , _disablePauseMenuHints   = _disablePauseMenuHints defaultDebugCfg
                                }
                            }
                        }
                return $ (cfgs', cns)

            ConsoleMsgSaveSettings -> writeSaveFilesSettings cfgs <&> \case
                Left errText -> (cfgs, printConsole errText cns)
                Right ()     -> (cfgs, cns)

            ConsoleMsgSaveProgress -> writeSaveFilesProgress cfgs <&> \case
                Left errText -> (cfgs, printConsole errText cns)
                Right ()     -> (cfgs, cns)

            where loadSettingsConfig = loadConfig "settings.cfg"

updateConsole :: World -> Console -> AppEnv ConsoleMsgsPhase ConsoleUpdateResult
updateConsole world console = do
    inputState <- readInputState

    let active = _active console
    active'   <- readSettingsConfig _debug _devConsoleEnabled <&> \case
        True
            | DevConsoleAlias `aliasPressed` inputState -> not active
        _                                               -> active

    let
        inputBuffer = T.filter (/= '`') (_textBuffer inputState)
        prevCmd     = SDL.ScancodeUp `keyPressed` inputState
        nextCmd     = SDL.ScancodeDown `keyPressed` inputState
        backspace   = SDL.ScancodeBackspace `keyPressed` inputState
        clear       = SDL.ScancodeLCtrl `keyHold` inputState && SDL.ScancodeC `keyPressed` inputState

        dspText = _displayText console
        text    = _text (dspText :: DisplayText)

    (configs, console') <- flip runStateT console $ do
        text' <- if
            | not active' -> return text
            | prevCmd     -> do
                (historyIndex, historyText) <- consoleCommandsHistory PrevHistory <$> get
                modify $ \c -> c {_commandsHistoryIndex = historyIndex}
                return historyText
            | nextCmd     -> do
                (historyIndex, historyText) <- consoleCommandsHistory NextHistory <$> get
                modify $ \c -> c {_commandsHistoryIndex = historyIndex}
                return historyText
            | backspace   -> return $ if
                | T.length text <= 1 -> text
                | otherwise          -> T.init text
            | clear       -> do
                modify $ \c -> c {_commandsHistoryIndex = Nothing}
                return ">"
            | otherwise   -> return $ text `T.append` inputBuffer

        modify $ \c -> c
            { _active      = active'
            , _displayText = updateDisplayText text' dspText
            }

        get >>= \c -> do
            cfgs        <- lift readConfigs
            (cfgs', c') <- lift $ processMessages cfgs c
            put c'
            return cfgs'

    if
        | active' && SDL.ScancodeReturn `keyPressed` inputState                    -> runConsolePrompt world console'
        | Just (inputRawData, cmdTxt) <- readCommandBindsInput inputState console' ->
            runCommandBind inputRawData cmdTxt world console'
        | otherwise                                                                ->
            return $ mkConsoleUpdateResult console' world configs

runCommandBind :: InputRawData -> T.Text -> World -> Console -> AppEnv ConsoleMsgsPhase ConsoleUpdateResult
runCommandBind inputRawData cmdTxt world console = do
    keyTxt      <- formatInputRawData inputRawData
    let console' = printConsole ("[running " <> keyTxt <> " bind]") console
    runConsoleCommands [cmdTxt] world console'

readCommandBindsInput :: InputState -> Console -> Maybe (InputRawData, T.Text)
readCommandBindsInput inputState console = listToMaybe
    [ (inputRawData, cmdTxt)
    | (inputRawData, cmdTxt) <- M.toList (_commandBinds console)
    , isPressed inputRawData
    ]
    where
        isPressed :: InputRawData -> Bool
        isPressed = \case
            KeyRawData keyCode ->
                -- below keyHold check shouldn't be necessary but SDL2 randomly continually sends the
                -- key pressed event for function keys sometimes? this prevents endless spamming the bind
                -- for a key in that situation
                let scancode = SDL.Scancode keyCode
                in scancode `keyPressed` inputState && scancode `keyHold` inputState

            GamepadButtonRawData btn -> SDL.Internal.fromNumber btn `gamepadPressed` inputState

            _ -> False

printConsole :: T.Text -> Console -> Console
printConsole text console
    | T.null text = console
    | otherwise   = console {_displayTextLines = textLines'}
        where
            textLines  = _displayTextLines console
            textLines' = (updateDisplayText text $ last textLines):(init textLines)

printConsoleCommandBinds :: forall m. MonadIO m => Console -> m Console
printConsoleCommandBinds console = case M.assocs (_commandBinds console) of
    []       -> return $ printConsole "no binds" console
    cmdBinds -> foldlM printBind console cmdBinds
        where
            printBind :: Console -> (InputRawData, T.Text) -> m Console
            printBind cns (inputRawData, cmdLine) = do
                inputTxt  <- formatInputRawData inputRawData
                let output = inputTxt <> ": \"" <> cmdLine <> "\""
                return $ printConsole output cns

runConsoleCommandsFile :: FilePath -> World -> Console -> AppEnv ConsoleMsgsPhase ConsoleUpdateResult
runConsoleCommandsFile filePath world console = liftIO (doesFileExist filePath) >>= \case
    False ->
        let console' = printConsole (T.pack filePath <> " does not exist") console
        in mkConsoleUpdateResult console' world <$> readConfigs

    True -> catchAppEnv (Right . T.lines . T.decodeUtf8 <$> liftIO (BS.readFile filePath)) (return . Left) >>= \case
        Left e ->
            let console' = printConsole ("error reading " <> T.pack filePath <> ": " <> prettyShow e) console
            in mkConsoleUpdateResult console' world <$> readConfigs

        Right cmdLines ->
            let console' = printConsole ("[running " <> T.pack filePath <> "]") console
            in runConsoleCommands cmdLines world console'

splitCommandLine :: T.Text -> [T.Text]
splitCommandLine cmdLine = txt:txts
    where
        splitSemicolon :: Char -> (Bool, T.Text, [T.Text]) -> (Bool, T.Text, [T.Text])
        splitSemicolon ';' (False, t, ts)    = (False, "", t:ts)
        splitSemicolon '"' (inQuotes, t, ts) = (not inQuotes, '"' `T.cons` t, ts)
        splitSemicolon c (inQuotes, t, ts)   = (inQuotes, c `T.cons` t, ts)

        (_, txt, txts) = T.foldr splitSemicolon (False, "", []) cmdLine

withAppEnvReadData' :: ConsoleUpdateResult -> AppEnv ConsoleMsgsPhase a -> AppEnv ConsoleMsgsPhase a
withAppEnvReadData' cnsUpdateResult appEnv = do
    inputState <- readInputState
    let cfgs    = _configs (cnsUpdateResult :: ConsoleUpdateResult)
    withAppEnvReadData inputState cfgs appEnv

runConsoleCommands :: [T.Text] -> World -> Console -> AppEnv ConsoleMsgsPhase ConsoleUpdateResult
runConsoleCommands cmdLines world console =
    let
        run :: ConsoleUpdateResult -> T.Text -> AppEnv ConsoleMsgsPhase ConsoleUpdateResult
        run cnsUpdateResult textCmd =
            let
                wld = _world cnsUpdateResult
                cns = _console cnsUpdateResult
            in do
                cmdResult <- withAppEnvReadData' cnsUpdateResult $
                    execConsoleCommand textCmd wld cns

                case cmdResult of
                    NoUpdateResult txt -> return $ cnsUpdateResult {_output = _output cnsUpdateResult ++ [txt]}

                    UpdateWorldResult txt w -> return $ cnsUpdateResult
                        { _world  = w
                        , _output = _output cnsUpdateResult ++ [txt]
                        }

                    UpdateConfigsResult txt c -> return $ cnsUpdateResult
                        { _configs = c
                        , _output  = _output cnsUpdateResult ++ [txt]
                        }

                    RunFileResult filePath ->
                        let cns' = L.foldl' (flip printConsole) cns (_output cnsUpdateResult)
                        in runConsoleCommandsFile filePath wld cns'

                    BindCmdResult inputRawData cmdTxt -> do
                        inputTxt <- formatInputRawData inputRawData
                        return $ cnsUpdateResult
                            { _console = cns {_commandBinds = M.insert inputRawData cmdTxt (_commandBinds cns)}
                            , _output  = _output cnsUpdateResult ++ ["bound " <> inputTxt]
                            }

                    PrintCmdBindsResult -> do
                        cns' <- printConsoleCommandBinds cns
                        return $ cnsUpdateResult {_console = cns'}
    in do
        cmdResult       <- mkConsoleUpdateResult console world <$> readConfigs
        let cmdLines'    = concat $ map splitCommandLine cmdLines
        cnsUpdateResult <- foldlM run cmdResult cmdLines'

        return $ cnsUpdateResult
            { _console = L.foldl' (flip printConsole) (_console cnsUpdateResult) (_output cnsUpdateResult)
            , _output  = []
            }

runConsoleCommandsAndFile :: [T.Text] -> FilePath -> World -> Console -> AppEnv ConsoleMsgsPhase ConsoleUpdateResult
runConsoleCommandsAndFile cmdLines filePath world console = do
    cnsUpdateResult <- runConsoleCommands cmdLines world console
    withAppEnvReadData' cnsUpdateResult $
        runConsoleCommandsFile filePath (_world cnsUpdateResult) (_console cnsUpdateResult)
