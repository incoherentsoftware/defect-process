module Menu.SettingsMenu.AudioTab
    ( mkSettingsAudioTab
    , updateSettingsAudioTab
    , drawSettingsAudioTab
    , isSettingsAudioTabExpandedComboBox
    ) where

import Control.Monad          (when, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (evalStateT, get, lift, put)
import Data.Functor           ((<&>))
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Audio.Fmod
import Audio.Volume
import Configs
import Configs.All.Settings
import Configs.All.Settings.Audio
import Configs.All.Settings.Menu
import FileCache
import Menu.SettingsMenu.AudioTab.Types
import Menu.SettingsMenu.Types
import Menu.SettingsMenu.Util
import Menu.SoundIndices
import Menu.Util
import Menu.ZIndex
import Msg
import Util
import Window.Graphics
import Window.Graphics.UiControls
import Window.InputState

settingsMenuPack             = \p -> PackResourceFilePath "data/menu/settings-menu.pack" p
controlsBtnImgPath           = settingsMenuPack "controls-button-inactive.image" :: PackResourceFilePath
graphicsBtnImgPath           = settingsMenuPack "graphics-button-inactive.image" :: PackResourceFilePath
audioBtnImgPath              = settingsMenuPack "audio-button.image"             :: PackResourceFilePath
gameBtnImgPath               = settingsMenuPack "game-button-inactive.image"     :: PackResourceFilePath
creditsBtnImgPath            = settingsMenuPack "credits-button-inactive.image"  :: PackResourceFilePath
audioTabBackgroundImgPath    = settingsMenuPack "audio-background.image"         :: PackResourceFilePath
restoreDefaultsButtonImgPath = settingsMenuPack "restore-default-audio.image"    :: PackResourceFilePath

comboBoxValues    = map formatIntPercent [0,10..100]         :: [T.Text]
comboBoxImagePath = settingsMenuPack "audio-combo-box.image" :: PackResourceFilePath

formatIntPercent :: Int -> T.Text
formatIntPercent = (<> "%") . prettyShow

formatVolume :: Volume -> T.Text
formatVolume = formatIntPercent . volumeToInt

mkSettingsAudioTab :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SettingsAudioTab
mkSettingsAudioTab = do
    tabBtns       <-
        mkSettingsTabButtons controlsBtnImgPath graphicsBtnImgPath audioBtnImgPath gameBtnImgPath creditsBtnImgPath
    backgroundImg <- loadPackImage audioTabBackgroundImgPath

    menuCfg  <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    audioCfg <- readConfig _settings _audio

    let
        mkComboBox' = \pos volume ->
            let
                offset           = _settingsAudioTabComboBoxValueOffset menuCfg
                color            = menuOptionBtnColor
                hoverColor       = hoverMenuOptionBtnColor
                defaultValueText = formatVolume volume
            in mkComboBox pos defaultValueText comboBoxValues Font22 color hoverColor offset comboBoxImagePath

    soundComboBox      <- mkComboBox' (_settingsAudioTabSoundComboBoxPos menuCfg) (_soundVolume audioCfg)
    musicComboBox      <- mkComboBox' (_settingsAudioTabMusicComboBoxPos menuCfg) (_musicVolume audioCfg)
    restoreDefaultsBtn <-
        mkImageButton (_settingsAudioTabRestoreDefaultsButtonPos menuCfg) restoreDefaultsButtonImgPath

    return $ SettingsAudioTab
        { _buttons               = tabBtns
        , _backgroundImage       = backgroundImg
        , _soundComboBox         = soundComboBox
        , _musicComboBox         = musicComboBox
        , _restoreDefaultsButton = restoreDefaultsBtn
        , _subSelection          = AudioNoSubSelection
        }

updateSelections
    :: InputRead m
    => SettingsMenuSelection
    -> SettingsAudioTab
    -> m (SettingsMenuSelection, AudioSubSelection)
updateSelections selection audioTab = do
    inputState <- readInputState
    let
        upPressed    = MenuUpAlias `aliasPressed` inputState
        downPressed  = MenuDownAlias `aliasPressed` inputState
        leftPressed  = MenuLeftAlias `aliasPressed` inputState
        rightPressed = MenuRightAlias `aliasPressed` inputState

    return $ case selection of
        SettingsMenuControlsTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, AudioNoSubSelection)
            | downPressed                 -> (SettingsMenuAudioSubSelection, AudioSoundSubSelection)
        SettingsMenuGraphicsTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, AudioNoSubSelection)
            | downPressed                 -> (SettingsMenuAudioSubSelection, AudioSoundSubSelection)
        SettingsMenuAudioTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, AudioNoSubSelection)
            | downPressed                 -> (SettingsMenuAudioSubSelection, AudioSoundSubSelection)
        SettingsMenuGameTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, AudioNoSubSelection)
            | downPressed                 -> (SettingsMenuAudioSubSelection, AudioSoundSubSelection)
        SettingsMenuCreditsTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, AudioNoSubSelection)
            | downPressed                 -> (SettingsMenuAudioSubSelection, AudioSoundSubSelection)
        SettingsMenuCloseSelection
            | upPressed                   -> (SettingsMenuAudioSubSelection, AudioMusicSubSelection)
            | downPressed                 -> (SettingsMenuAudioTabSelection, AudioNoSubSelection)
            | leftPressed || rightPressed -> (SettingsMenuAudioSubSelection, AudioRestoreDefaultsSubSelection)

        SettingsMenuAudioSubSelection -> case _subSelection (audioTab :: SettingsAudioTab) of
            AudioSoundSubSelection
                | upPressed                   -> (SettingsMenuAudioTabSelection, AudioNoSubSelection)
                | downPressed                 -> (SettingsMenuAudioSubSelection, AudioMusicSubSelection)
            AudioMusicSubSelection
                | upPressed                   -> (SettingsMenuAudioSubSelection, AudioSoundSubSelection)
                | downPressed                 -> (SettingsMenuCloseSelection, AudioNoSubSelection)
            AudioRestoreDefaultsSubSelection
                | upPressed                   -> (SettingsMenuAudioSubSelection, AudioMusicSubSelection)
                | downPressed                 -> (SettingsMenuAudioTabSelection, AudioNoSubSelection)
                | leftPressed || rightPressed -> (SettingsMenuCloseSelection, AudioNoSubSelection)
            subSelection                      -> (SettingsMenuAudioSubSelection, subSelection)

        _ -> (selection, AudioNoSubSelection)

writeSetSoundVolumeMsgs :: MsgsWrite MenuMsgsPhase m => T.Text -> m ()
writeSetSoundVolumeMsgs volText = case T.decimal volText of
    Left _            -> return ()
    Right (volInt, _) -> writeMsgs
        [ mkMsg $ ConsoleMsgSetSoundVolume (mkVolume volInt)
        , mkMsg ConsoleMsgSaveSettings
        ]

writeSetMusicVolumeMsgs :: MsgsWrite MenuMsgsPhase m => T.Text -> m ()
writeSetMusicVolumeMsgs volText = case T.decimal volText of
    Left _            -> return ()
    Right (volInt, _) -> writeMsgs
        [ mkMsg $ ConsoleMsgSetMusicVolume (mkVolume volInt)
        , mkMsg ConsoleMsgSaveSettings
        ]

writeRestoreDefaultsMsgs :: MsgsWrite MenuMsgsPhase m => m ()
writeRestoreDefaultsMsgs = writeMsgs
    [ mkMsg ConsoleMsgRestoreDefaultSettingsAudio
    , mkMsg ConsoleMsgSaveSettings
    ]

updateTabButtons :: SettingsMenuData -> SettingsTabButtons -> (SettingsTabChoice, SettingsTabButtons)
updateTabButtons settingsMenuData tabBtns
    | _isPressed (_controlsButton tabBtns :: Button) =
        ( ControlsTabChoice
        , _buttons (_controlsTab settingsMenuData :: SettingsControlsTab)
        )
    | _isPressed (_graphicsButton tabBtns :: Button) =
        ( GraphicsTabChoice
        , _buttons (_graphicsTab settingsMenuData :: SettingsGraphicsTab)
        )
    | _isPressed (_gameButton tabBtns :: Button)     =
        ( GameTabChoice
        , _buttons (_gameTab settingsMenuData :: SettingsGameTab)
        )
    | _isPressed (_creditsButton tabBtns :: Button)  =
        ( CreditsTabChoice
        , _buttons (_creditsTab settingsMenuData :: SettingsCreditsTab)
        )
    | otherwise                                      = (AudioTabChoice, tabBtns)

updateSettingsAudioTab
    :: (ConfigsRead m, InputRead m, MonadIO m, MsgsWrite MenuMsgsPhase m)
    => SettingsMenuSelection
    -> SettingsTabButtons
    -> SettingsMenuData
    -> m (SettingsMenuData -> SettingsMenuData)
updateSettingsAudioTab selection tabBtns settingsMenuData =
    let
        (tabChoice, tabBtns') = updateTabButtons settingsMenuData tabBtns
        audioTab              = _audioTab settingsMenuData
        isAnyExpandedComboBox = isSettingsAudioTabExpandedComboBox audioTab
    in do
        (selection', subSelection) <- if
            | isAnyExpandedComboBox -> return (selection, _subSelection (audioTab :: SettingsAudioTab))
            | otherwise             -> updateSelections selection audioTab

        flip evalStateT (selection', subSelection) $
            let
                updateComboBoxAndSelection = \comboBoxSubSelect valueText comboBox -> do
                    comboBoxStatus <- get <&> \(_, subSelect) -> if
                        | isAnyExpandedComboBox && not (_isExpanded comboBox) -> ComboBoxInactiveStatus
                        | subSelect == comboBoxSubSelect                      -> ComboBoxSelectedActiveStatus
                        | otherwise                                           -> ComboBoxActiveStatus
                    comboBox'      <- lift $ updateComboBox comboBoxStatus valueText comboBox

                    when (_isSelected (comboBox' :: ComboBox) || _isExpanded comboBox') $
                        put (SettingsMenuAudioSubSelection, comboBoxSubSelect)
                    return comboBox'

                updateButtonAndSelection = \btnSubSelect btn -> do
                    btnStatus <- get <&> \(_, subSelect) -> if
                        | isAnyExpandedComboBox     -> ButtonInactiveStatus
                        | subSelect == btnSubSelect -> ButtonSelectedActiveStatus
                        | otherwise                 -> ButtonActiveStatus
                    btn'           <- lift $ updateButton btnStatus btn

                    when (_isSelected (btn' :: Button) || _isPressed (btn' :: Button)) $
                        put (SettingsMenuAudioSubSelection, btnSubSelect)
                    return btn'
            in do
                cfg <- lift $ readConfig _settings _audio

                let soundVolumeText  = formatVolume $ _soundVolume cfg
                soundComboBox       <-
                    updateComboBoxAndSelection AudioSoundSubSelection soundVolumeText (_soundComboBox audioTab)
                let soundVolumeText' = comboBoxValue soundComboBox
                when (soundVolumeText' /= soundVolumeText) $
                    lift $ writeSetSoundVolumeMsgs soundVolumeText'

                let musicVolumeText  = formatVolume $ _musicVolume cfg
                musicComboBox       <-
                    updateComboBoxAndSelection AudioMusicSubSelection musicVolumeText (_musicComboBox audioTab)
                let musicVolumeText' = comboBoxValue musicComboBox
                when (musicVolumeText' /= musicVolumeText) $
                    lift $ writeSetMusicVolumeMsgs musicVolumeText'

                let restoreDefaultsBtn = _restoreDefaultsButton (audioTab :: SettingsAudioTab)
                restoreDefaultsBtn'   <- updateButtonAndSelection AudioRestoreDefaultsSubSelection restoreDefaultsBtn
                when (_isPressed (restoreDefaultsBtn' :: Button)) $
                    lift writeRestoreDefaultsMsgs

                let soundIndices = _soundIndices (settingsMenuData :: SettingsMenuData)
                when (_isPressed (soundComboBox :: ComboBox) || _isPressed (musicComboBox :: ComboBox)) $
                    void $ playFmodSound (_confirmAlt soundIndices)

                when (_isPressed (restoreDefaultsBtn' :: Button)) $
                    void $ playFmodSound (_confirmSmall soundIndices)

                get <&> \(select, subSelect) ->
                    let
                        audioTab' = audioTab
                            { _soundComboBox         = soundComboBox
                            , _musicComboBox         = musicComboBox
                            , _restoreDefaultsButton = restoreDefaultsBtn'
                            , _subSelection          = subSelect
                            }
                    in \smd -> smd
                        { _tabChoice  = tabChoice
                        , _tabButtons = tabBtns'
                        , _audioTab   = audioTab'
                        , _selection  = select
                        }

drawSettingsAudioTab :: (GraphicsReadWrite m, MonadIO m) => SettingsAudioTab -> m ()
drawSettingsAudioTab audioTab = do
    drawImage zeroPos2 RightDir menuOverZIndex (_backgroundImage (audioTab :: SettingsAudioTab))
    drawComboBox menuOverZIndex menuOverExpandedZIndex (_soundComboBox audioTab)
    drawComboBox menuOverZIndex menuOverExpandedZIndex (_musicComboBox audioTab)
    drawButton menuOverZIndex (_restoreDefaultsButton (audioTab :: SettingsAudioTab))

isSettingsAudioTabExpandedComboBox :: SettingsAudioTab -> Bool
isSettingsAudioTabExpandedComboBox audioTab = or $ map _isExpanded
    [ _soundComboBox audioTab
    , _musicComboBox audioTab
    ]
