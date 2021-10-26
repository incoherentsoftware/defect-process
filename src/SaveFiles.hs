module SaveFiles
    ( module SaveFiles.Types
    , doesSaveFilesSettingsExist
    , readSaveFilesSettings
    , writeSaveFilesSettings
    , readSaveFilesProgress
    , writeSaveFilesProgress
    ) where

import Control.Monad.Catch    (MonadCatch, SomeException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor           ((<&>))
import Data.Yaml              (decodeFileEither, encodeFile)
import System.Directory       (doesFileExist)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

import Configs
import Configs.All.Progress
import Configs.All.Settings
import Configs.All.Settings.Audio
import Configs.All.Settings.Controls
import Configs.All.Settings.Debug
import Configs.All.Settings.Render
import Menu.SettingsMenu.Util
import SaveFiles.Types
import Util
import Window.InputState.Alias

saveFilesVersion = "0.9.2" :: T.Text

saveFilesSettingsFilePath = "saves/settings.sav" :: FilePath
saveFilesProgressFilePath = "saves/progress.sav" :: FilePath

doesSaveFilesSettingsExist :: MonadIO m => m Bool
doesSaveFilesSettingsExist = liftIO $ doesFileExist =<< translateResourcePath saveFilesSettingsFilePath

doesSaveFilesProgressExist :: MonadIO m => m Bool
doesSaveFilesProgressExist = liftIO $ doesFileExist =<< translateResourcePath saveFilesProgressFilePath

readSaveFilesSettings :: MonadIO m => Configs -> m (Either T.Text Configs)
readSaveFilesSettings cfgs = liftIO $
    doesSaveFilesSettingsExist >>= \case
        False -> return $ Left ("skipping load settings: " <> T.pack saveFilesSettingsFilePath <> " does not exist")
        True  -> (decodeFileEither =<< translateResourcePath saveFilesSettingsFilePath) <&> \case
            Left e    -> Left $ T.pack saveFilesSettingsFilePath <> ": " <> T.pack (show e)
            Right sfs ->
                let
                    settingsCfg = _settings cfgs

                    controlsCfg  = _controls settingsCfg
                    controlsCfg' = controlsCfg
                        { _inputAliasRawDataMap = HM.union
                            (filterInputAliasRawDataMapRebindable $ _controlsInputAliasRawDataMap sfs)
                            (_inputAliasRawDataMap controlsCfg)
                        }

                    renderCfg = (_render settingsCfg)
                        { _winWidth        = _renderWinWidth sfs
                        , _winHeight       = _renderWinHeight sfs
                        , _winMode         = _renderWinMode sfs
                        , _winDisplayIndex = _renderWinDisplayIndex sfs
                        }

                    audioCfg = (_audio settingsCfg)
                        { _soundVolume      = _audioSoundVolume sfs
                        , _musicVolume      = _audioMusicVolume sfs
                        , _battleMusic      = _audioBattleMusic sfs
                        , _explorationMusic = _audioExplorationMusic sfs
                        }

                    debugCfg = (_debug settingsCfg)
                        { _enemiesDamageMultiplier = enemyHealthPercentToDamageMultiplier $ _gameEnemyHealthPercent sfs
                        , _disablePauseMenuHints   = not $ _gamePauseMenuHints sfs
                        }
                in Right $ cfgs
                    { _settings = settingsCfg
                        { _controls = controlsCfg'
                        , _render   = renderCfg
                        , _audio    = audioCfg
                        , _debug    = debugCfg
                        }
                    }

writeSaveFilesSettings :: MonadIO m => Configs -> m (Either T.Text ())
writeSaveFilesSettings cfgs =
    let
        catch'
            :: MonadCatch m1
            => m1 (Either SomeException ())
            -> (SomeException -> m1 (Either SomeException ()))
            -> m1 (Either SomeException ())
        catch' = catch

        settingsCfg = _settings cfgs
        controlsCfg = _controls settingsCfg
        renderCfg   = _render settingsCfg
        audioCfg    = _audio settingsCfg
        debugCfg    = _debug settingsCfg

        saveFilesSettings = SaveFilesSettings
            { _version                      = saveFilesVersion
            , _controlsInputAliasRawDataMap = filterInputAliasRawDataMapRebindable $ _inputAliasRawDataMap controlsCfg
            , _renderWinWidth               = _winWidth renderCfg
            , _renderWinHeight              = _winHeight renderCfg
            , _renderWinMode                = _winMode renderCfg
            , _renderWinDisplayIndex        = _winDisplayIndex renderCfg
            , _audioSoundVolume             = _soundVolume audioCfg
            , _audioMusicVolume             = _musicVolume audioCfg
            , _audioBattleMusic             = _battleMusic audioCfg
            , _audioExplorationMusic        = _explorationMusic audioCfg
            , _gameEnemyHealthPercent       = enemyDamageMultiplierToHealthPercent $ _enemiesDamageMultiplier debugCfg
            , _gamePauseMenuHints           = not $ _disablePauseMenuHints debugCfg
            }
    in do
        saveFilesSettingsFilePath' <- translateResourcePath saveFilesSettingsFilePath
        liftIO $
            catch' (Right <$> encodeFile saveFilesSettingsFilePath' saveFilesSettings) (return . Left) <&> \case
                Right () -> Right ()
                Left e   -> Left $ T.pack (show e)

readSaveFilesProgress :: MonadIO m => Configs -> m (Either T.Text Configs)
readSaveFilesProgress cfgs = liftIO $
    doesSaveFilesProgressExist >>= \case
        False -> return $ Left ("skipping load progress: " <> T.pack saveFilesProgressFilePath <> " does not exist")
        True  -> (decodeFileEither =<< translateResourcePath saveFilesProgressFilePath) <&> \case
            Left e    -> Left $ T.pack saveFilesProgressFilePath <> ": " <> T.pack (show e)
            Right sfp ->
                let
                    progressCfg = (_progress cfgs :: ProgressConfig)
                        { _totalGold               = _totalGold (sfp :: SaveFilesProgress)
                        , _unlockedWeapons         = _unlockedWeapons (sfp :: SaveFilesProgress)
                        , _unlockedGuns            = _unlockedGuns (sfp :: SaveFilesProgress)
                        , _unlockedMovementSkills  = _unlockedMovementSkills (sfp :: SaveFilesProgress)
                        , _unlockedSecondarySkills = _unlockedSecondarySkills (sfp :: SaveFilesProgress)
                        , _unlockedMusic           = _unlockedMusic (sfp :: SaveFilesProgress)
                        }
                in Right $ cfgs {_progress = progressCfg}

writeSaveFilesProgress :: MonadIO m => Configs -> m (Either T.Text ())
writeSaveFilesProgress cfgs =
    let
        catch'
            :: MonadCatch m1
            => m1 (Either SomeException ())
            -> (SomeException -> m1 (Either SomeException ()))
            -> m1 (Either SomeException ())
        catch' = catch

        progressCfg       = _progress cfgs
        saveFilesProgress = SaveFilesProgress
            { _version                 = saveFilesVersion
            , _totalGold               = _totalGold (progressCfg :: ProgressConfig)
            , _unlockedWeapons         = _unlockedWeapons (progressCfg :: ProgressConfig)
            , _unlockedGuns            = _unlockedGuns (progressCfg :: ProgressConfig)
            , _unlockedMovementSkills  = _unlockedMovementSkills (progressCfg :: ProgressConfig)
            , _unlockedSecondarySkills = _unlockedSecondarySkills (progressCfg :: ProgressConfig)
            , _unlockedMusic           = _unlockedMusic (progressCfg :: ProgressConfig)
            }
    in do
        saveFilesProgressFilePath' <- translateResourcePath saveFilesProgressFilePath
        liftIO $
            catch' (Right <$> encodeFile saveFilesProgressFilePath' saveFilesProgress) (return . Left) <&> \case
                Right () -> Right ()
                Left e   -> Left $ T.pack (show e)
