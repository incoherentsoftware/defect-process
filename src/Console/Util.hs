module Console.Util
    ( setControlsInputAliasConsole
    , setGraphicsResolutionConsole
    , setGraphicsWindowModeConsole
    , updateRenderConfigWinDisplayIndexConsole
    , setSoundVolumeConsole
    , setMusicVolumeConsole
    , setBattleMusicConsole
    , setExplorationMusicConsole
    , addProgressTotalGoldConsole
    , unlockWeaponConsole
    , unlockGunConsole
    , unlockMovementSkillConsole
    , unlockSecondarySkillConsole
    , unlockMusicConsole
    , applyFallbackRenderConfigConsole
    , applyControlsConfigConsole
    , applyRenderConfigConsole
    , applyAudioConfigConsole
    , applyProgressConfigConsole
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, get, modify)
import Data.Maybe             (fromMaybe)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as S

import Audio.Fmod
import Audio.Volume
import Configs
import Configs.All.Progress
import Configs.All.Settings
import Configs.All.Settings.Audio
import Configs.All.Settings.Controls
import Configs.All.Settings.Render
import Player.Gun.Types
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Weapon.Types
import Window.Graphics
import Window.InputState
import World.Audio.LayeredMusic.Types
import World.Util

setControlsInputAliasConsole :: InputAlias -> [InputRawData] -> Maybe InputRawData -> Configs -> Configs
setControlsInputAliasConsole inputAlias prevInputRawDatas inputRawData cfgs = cfgs
    { _settings = settingsCfg
        { _controls = controlsCfg {_inputAliasRawDataMap = rawDatasMap'}
        }
    }
    where
        settingsCfg = _settings cfgs
        controlsCfg = _controls settingsCfg
        rawDatasMap = _inputAliasRawDataMap (controlsCfg :: ControlsConfig)

        rawDatasMap' = case inputRawData of
            -- clear input alias bindings
            Nothing ->
                let filter' = \rds -> filter (`notElem` prevInputRawDatas) rds
                in HM.adjust filter' inputAlias rawDatasMap

            -- add input alias binding
            Just rawData -> flip execState rawDatasMap $ do
                let
                    filterExisting = \ia rds -> if
                        | isInputAliasRebindable ia -> filter (/= rawData) rds
                        | otherwise                 -> rds
                modify $ HM.mapWithKey filterExisting

                rawDatas <- HM.lookupDefault [] inputAlias <$> get
                let
                    rawDatas' = case prevInputRawDatas of
                        (rd0:rd1:_)
                            | rd0 `elem` rawDatas && rd1 `elem` rawDatas -> filter (/= rd0) rawDatas ++ [rawData]
                        _                                                -> rawDatas ++ [rawData]
                modify $ HM.insert inputAlias rawDatas'

setGraphicsResolutionConsole :: (GraphicsReadWrite m, MonadIO m) => Int -> Int -> Configs -> m Configs
setGraphicsResolutionConsole winWidth winHeight cfgs = do
    setGraphicsResolution winWidth winHeight
    (winWidth', winHeight') <- fromMaybe (winWidth, winHeight) <$> getGraphicsResolution
    return $ cfgs
        { _settings =
            let settingsCfg = _settings cfgs
            in settingsCfg
                { _render = (_render settingsCfg)
                    { _winWidth  = winWidth'
                    , _winHeight = winHeight'
                    }
                }
        }

setGraphicsDisplayIndexConsole :: (GraphicsReadWrite m, MonadIO m) => Int -> Configs -> m Configs
setGraphicsDisplayIndexConsole winDisplayIndex cfgs = getGraphicsDisplayCount >>= \case
    displayCount
        | winDisplayIndex >= displayCount -> return cfgs
        | otherwise                       -> do
            setGraphicsDisplayIndex winDisplayIndex
            return $ cfgs
                { _settings =
                    let settingsCfg = _settings cfgs
                    in settingsCfg
                        { _render = (_render settingsCfg) {_winDisplayIndex = winDisplayIndex}
                        }
                }

setGraphicsWindowModeConsole :: (GraphicsReadWrite m, MonadIO m) => WindowMode -> Configs -> m Configs
setGraphicsWindowModeConsole winMode cfgs =
    let
        settingsCfg         = _settings cfgs
        renderCfg           = _render settingsCfg
        renderCfgResolution = (_winWidth renderCfg, _winHeight renderCfg)
    in do
        isResolutionAvailable <- case winMode of
            FullscreenMode        -> do
                availableResolutions <- getGraphicsAvailableResolutions
                currentResolution    <- fromMaybe renderCfgResolution <$> getGraphicsResolution
                return $ currentResolution `elem` availableResolutions
            FullscreenDesktopMode -> return True
            WindowedMode          -> return True

        if
            | isResolutionAvailable -> do
                setGraphicsWindowMode winMode
                (winWidth, winHeight) <- fromMaybe renderCfgResolution <$> getGraphicsResolution

                return $ cfgs
                    { _settings = settingsCfg
                        { _render = renderCfg
                            { _winWidth  = winWidth
                            , _winHeight = winHeight
                            , _winMode   = winMode
                            }
                        }
                    }

            | otherwise -> return cfgs

updateRenderConfigWinDisplayIndexConsole :: (GraphicsRead m, MonadIO m) => Configs -> m Configs
updateRenderConfigWinDisplayIndexConsole cfgs =
    let
        settingsCfg = _settings cfgs
        renderCfg   = _render settingsCfg
    in do
        displayIndex <- getGraphicsDisplayIndex
        return $ cfgs
            { _settings = settingsCfg
                { _render = renderCfg {_winDisplayIndex = displayIndex}
                }
            }

setSoundVolumeConsole :: MonadIO m => Volume -> Configs -> m Configs
setSoundVolumeConsole volume cfgs = do
    setFmodSoundVolume volume
    return $ cfgs
        { _settings =
            let
                settingsCfg = _settings cfgs
                audioCfg    = _audio (settingsCfg :: SettingsConfig)
            in settingsCfg {_audio = audioCfg {_soundVolume = volume}}
        }

setMusicVolumeConsole :: MonadIO m => Volume -> Configs -> m Configs
setMusicVolumeConsole volume cfgs = do
    setFmodMusicVolume volume
    return $ cfgs
        { _settings =
            let
                settingsCfg = _settings cfgs
                audioCfg    = _audio (settingsCfg :: SettingsConfig)
            in settingsCfg {_audio = audioCfg {_musicVolume = volume}}
        }

setBattleMusicConsole :: LayeredMusicType -> Configs -> Configs
setBattleMusicConsole layeredMusicType cfgs = cfgs
    { _settings =
        let
            settingsCfg = _settings cfgs
            audioCfg    = _audio (settingsCfg :: SettingsConfig)
        in settingsCfg {_audio = audioCfg {_battleMusic = layeredMusicType}}
    }

setExplorationMusicConsole :: LayeredMusicType -> Configs -> Configs
setExplorationMusicConsole layeredMusicType cfgs = cfgs
    { _settings =
        let
            settingsCfg = _settings cfgs
            audioCfg    = _audio (settingsCfg :: SettingsConfig)
        in settingsCfg {_audio = audioCfg {_explorationMusic = layeredMusicType}}
    }

addProgressTotalGoldConsole :: GoldValue -> Configs -> Configs
addProgressTotalGoldConsole gold cfgs = cfgs
    { _progress = progressCfg {_totalGold = totalGold + gold}
    }
    where
        progressCfg = _progress cfgs
        totalGold   = _totalGold (progressCfg :: ProgressConfig)

unlockWeaponConsole :: GoldValue -> WeaponType -> Configs -> Configs
unlockWeaponConsole cost wpnType cfgs = cfgs
    { _progress = progressCfg
        { _totalGold       = max (GoldValue 0) (_totalGold (progressCfg :: ProgressConfig) - cost)
        , _unlockedWeapons = wpnType `S.insert` _unlockedWeapons (progressCfg :: ProgressConfig)
        }
    }
    where progressCfg = _progress cfgs

unlockGunConsole :: GoldValue -> GunType -> Configs -> Configs
unlockGunConsole cost gunType cfgs = cfgs
    { _progress = progressCfg
        { _totalGold    = max (GoldValue 0) (_totalGold (progressCfg :: ProgressConfig) - cost)
        , _unlockedGuns = gunType `S.insert` _unlockedGuns (progressCfg :: ProgressConfig)
        }
    }
    where progressCfg = _progress cfgs

unlockMovementSkillConsole :: GoldValue -> MovementSkillType -> Configs -> Configs
unlockMovementSkillConsole cost movementSkillType cfgs = cfgs
    { _progress = progressCfg
        { _totalGold              = max (GoldValue 0) (_totalGold (progressCfg :: ProgressConfig) - cost)
        , _unlockedMovementSkills = unlockedMovementSkills
        }
    }
    where
        progressCfg            = _progress cfgs
        unlockedMovementSkills = movementSkillType `S.insert` _unlockedMovementSkills (progressCfg :: ProgressConfig)

unlockSecondarySkillConsole :: GoldValue -> SecondarySkillType -> Configs -> Configs
unlockSecondarySkillConsole cost secondarySkillType cfgs = cfgs
    { _progress = progressCfg
        { _totalGold               = max (GoldValue 0) (_totalGold (progressCfg :: ProgressConfig) - cost)
        , _unlockedSecondarySkills = unlockedSecondarySkills
        }
    }
    where
        progressCfg             = _progress cfgs
        unlockedSecondarySkills =
            secondarySkillType `S.insert` _unlockedSecondarySkills (progressCfg :: ProgressConfig)

unlockMusicConsole :: GoldValue -> LayeredMusicType -> Configs -> Configs
unlockMusicConsole cost musicType cfgs = cfgs
    { _progress = progressCfg
        { _totalGold     = max (GoldValue 0) (_totalGold (progressCfg :: ProgressConfig) - cost)
        , _unlockedMusic = musicType `S.insert` _unlockedMusic (progressCfg :: ProgressConfig)
        }
    }
    where progressCfg = _progress cfgs

applyFallbackRenderConfigConsole :: (GraphicsReadWrite m, MonadIO m) => Configs -> m (Maybe Configs)
applyFallbackRenderConfigConsole cfgs = getGraphicsDesktopResolution >>= \case
    Nothing                            -> return Nothing
    Just (desktopWidth, desktopHeight) ->
        let
            renderCfg = _render $ _settings cfgs
            winWidth  = _winWidth renderCfg
            winHeight = _winHeight renderCfg
        in if
            | desktopWidth < winWidth || desktopHeight < winHeight ->
                let
                    renderCfg' = renderCfg
                        { _winWidth  = desktopWidth
                        , _winHeight = desktopHeight
                        , _winMode   = FullscreenDesktopMode
                        }
                in Just <$> applyRenderConfigConsole renderCfg' cfgs

            | desktopWidth == winWidth || desktopHeight == winHeight ->
                let renderCfg' = renderCfg {_winMode = FullscreenDesktopMode}
                in Just <$> applyRenderConfigConsole renderCfg' cfgs

            | otherwise -> return Nothing

applyControlsConfigConsole :: ControlsConfig -> Configs -> Configs
applyControlsConfigConsole controlsCfg cfgs = cfgs
    { _settings = (_settings cfgs) {_controls = controlsCfg}
    }

applyRenderConfigConsole :: (GraphicsReadWrite m, MonadIO m) => RenderConfig -> Configs -> m Configs
applyRenderConfigConsole renderCfg cfgs =
    setGraphicsWindowModeConsole WindowedMode cfgs >>=
    setGraphicsDisplayIndexConsole (_winDisplayIndex renderCfg) >>=
    setGraphicsResolutionConsole (_winWidth renderCfg) (_winHeight renderCfg) >>=
    setGraphicsWindowModeConsole (_winMode renderCfg)

applyAudioConfigConsole :: MonadIO m => AudioConfig -> Configs -> m Configs
applyAudioConfigConsole audioCfg cfgs =
    setSoundVolumeConsole (_soundVolume audioCfg) cfgs >>=
    setMusicVolumeConsole (_musicVolume audioCfg) >>=
    return . setBattleMusicConsole (_battleMusic audioCfg) >>=
    return . setExplorationMusicConsole (_explorationMusic audioCfg)

applyProgressConfigConsole :: ProgressConfig -> Configs -> Configs
applyProgressConfigConsole progressCfg cfgs = cfgs {_progress = progressCfg}
