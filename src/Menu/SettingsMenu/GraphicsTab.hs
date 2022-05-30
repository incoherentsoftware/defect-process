module Menu.SettingsMenu.GraphicsTab
    ( mkSettingsGraphicsTab
    , updateSettingsGraphicsTab
    , drawSettingsGraphicsTab
    , isSettingsGraphicsTabExpandedComboBox
    ) where

import Control.Monad          (when, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (evalStateT, get, lift, put)
import Data.Foldable          (traverse_)
import Data.Functor           ((<&>))
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Menu
import Configs.All.Settings.Render
import FileCache
import Menu.SettingsMenu.GraphicsTab.Types
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

settingsMenuPack               = \p -> PackResourceFilePath "data/menu/settings-menu.pack" p
controlsBtnImgPath             = settingsMenuPack "controls-button-inactive.image"            :: PackResourceFilePath
graphicsBtnImgPath             = settingsMenuPack "graphics-button.image"                     :: PackResourceFilePath
audioBtnImgPath                = settingsMenuPack "audio-button-inactive.image"               :: PackResourceFilePath
gameBtnImgPath                 = settingsMenuPack "game-button-inactive.image"                :: PackResourceFilePath
creditsBtnImgPath              = settingsMenuPack "credits-button-inactive.image"             :: PackResourceFilePath
backgroundImgPath              = settingsMenuPack "graphics-background.image"                 :: PackResourceFilePath
disabledComboBoxOverlayImgPath = settingsMenuPack "graphics-disabled-combo-box-overlay.image" :: PackResourceFilePath
comboBoxImagePath              = settingsMenuPack "graphics-combo-box.image"                  :: PackResourceFilePath
restoreDefaultsButtonImgPath   = settingsMenuPack "restore-default-graphics.image"            :: PackResourceFilePath

-- intentionally leave out Fullscreen for now, seeing cross-platform + high DPI issues in testing
displayModeValues =
    [ "Window"
    , "Fullscreen Borderless"
    ] :: [T.Text]

formatResolutionText :: (Int, Int) -> T.Text
formatResolutionText (width, height) = prettyShow width <> " x " <> prettyShow height

getCurrentResolutionText :: (GraphicsRead m, MonadIO m) => m T.Text
getCurrentResolutionText = formatResolutionText <$> getGraphicsWindowSize

parseResolution :: T.Text -> Maybe (Int, Int)
parseResolution txt = case T.words txt of
    [w, _, h]
        | Right (w', _) <- T.decimal w, Right (h', _) <- T.decimal h -> Just (w', h')
    _                                                                -> Nothing

formatWindowMode :: WindowMode -> T.Text
formatWindowMode = \case
    FullscreenMode        -> "Fullscreen"
    FullscreenDesktopMode -> "Fullscreen Borderless"
    WindowedMode          -> "Window"

parseDisplayMode :: T.Text -> WindowMode
parseDisplayMode txt
    | txt == "Fullscreen Borderless" = FullscreenDesktopMode
    | otherwise                      = WindowedMode

isResolutionComboBoxDisabled :: SettingsGraphicsTab -> Bool
isResolutionComboBoxDisabled graphicsTab = parseDisplayMode displayModeComboBoxValue == FullscreenDesktopMode
    where displayModeComboBoxValue = comboBoxValue $ _displayModeComboBox graphicsTab

mkResolutionComboBox :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m ComboBox
mkResolutionComboBox = do
    menuCfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    let
        pos    = _settingsGraphicsTabResolutionComboBoxPos menuCfg
        offset = _settingsGraphicsTabResolutionComboBoxValueOffset menuCfg

    value  <- getCurrentResolutionText
    values <- map formatResolutionText <$> getGraphicsAvailableResolutions
    mkComboBox pos value values Font22 menuOptionBtnColor hoverMenuOptionBtnColor offset comboBoxImagePath

mkDisplayModeComboBox :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m ComboBox
mkDisplayModeComboBox = do
    menuCfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    let
        pos    = _settingsGraphicsTabDisplayModeComboBoxPos menuCfg
        offset = _settingsGraphicsTabDisplayModeComboBoxValueOffset menuCfg

    value <- formatWindowMode <$> readSettingsConfig _render _winMode
    mkComboBox pos value displayModeValues Font22 menuOptionBtnColor hoverMenuOptionBtnColor offset comboBoxImagePath

mkSettingsGraphicsTab :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SettingsGraphicsTab
mkSettingsGraphicsTab = do
    tabBtns <-
        mkSettingsTabButtons controlsBtnImgPath graphicsBtnImgPath audioBtnImgPath gameBtnImgPath creditsBtnImgPath

    backgroundImg              <- loadPackImage backgroundImgPath
    resolutionComboBox         <- mkResolutionComboBox
    displayModeComboBox        <- mkDisplayModeComboBox
    windowDisplayIndex         <- readSettingsConfig _render _winDisplayIndex
    restoreDefaultsBtnPos      <- readSettingsConfig _menu _settingsGraphicsTabRestoreDefaultsButtonPos
    restoreDefaultsBtn         <- mkImageButtonCentered restoreDefaultsBtnPos restoreDefaultsButtonImgPath
    disabledComboBoxOverlayImg <- loadPackImage disabledComboBoxOverlayImgPath

    return $ SettingsGraphicsTab
        { _buttons                      = tabBtns
        , _backgroundImage              = backgroundImg
        , _resolutionComboBox           = resolutionComboBox
        , _displayModeComboBox          = displayModeComboBox
        , _restoreDefaultsButton        = restoreDefaultsBtn
        , _disabledComboBoxOverlayImage = disabledComboBoxOverlayImg
        , _subSelection                 = GraphicsNoSubSelection
        , _windowDisplayIndex           = windowDisplayIndex
        }

updateSelections
    :: InputRead m
    => SettingsMenuSelection
    -> SettingsGraphicsTab
    -> m (SettingsMenuSelection, GraphicsSubSelection)
updateSelections selection graphicsTab = do
    inputState <- readInputState
    let
        upPressed            = MenuUpAlias `aliasPressed` inputState
        downPressed          = MenuDownAlias `aliasPressed` inputState
        leftPressed          = MenuLeftAlias `aliasPressed` inputState
        rightPressed         = MenuRightAlias `aliasPressed` inputState
        isResolutionDisabled = isResolutionComboBoxDisabled graphicsTab

    return $ case selection of
        SettingsMenuControlsTabSelection
            | upPressed   -> (SettingsMenuCloseSelection, GraphicsNoSubSelection)
            | downPressed -> if
                | isResolutionDisabled -> (SettingsMenuGraphicsSubSelection, GraphicsDisplayModeSubSelection)
                | otherwise            -> (SettingsMenuGraphicsSubSelection, GraphicsResolutionSubSelection)

        SettingsMenuGraphicsTabSelection
            | upPressed   -> (SettingsMenuCloseSelection, GraphicsNoSubSelection)
            | downPressed -> if
                | isResolutionDisabled -> (SettingsMenuGraphicsSubSelection, GraphicsDisplayModeSubSelection)
                | otherwise            -> (SettingsMenuGraphicsSubSelection, GraphicsResolutionSubSelection)

        SettingsMenuAudioTabSelection
            | upPressed   -> (SettingsMenuCloseSelection, GraphicsNoSubSelection)
            | downPressed -> if
                | isResolutionDisabled -> (SettingsMenuGraphicsSubSelection, GraphicsDisplayModeSubSelection)
                | otherwise            -> (SettingsMenuGraphicsSubSelection, GraphicsResolutionSubSelection)
        SettingsMenuGameTabSelection
            | upPressed   -> (SettingsMenuCloseSelection, GraphicsNoSubSelection)
            | downPressed -> if
                | isResolutionDisabled -> (SettingsMenuGraphicsSubSelection, GraphicsDisplayModeSubSelection)
                | otherwise            -> (SettingsMenuGraphicsSubSelection, GraphicsResolutionSubSelection)

        SettingsMenuCreditsTabSelection
            | upPressed   -> (SettingsMenuCloseSelection, GraphicsNoSubSelection)
            | downPressed -> if
                | isResolutionDisabled -> (SettingsMenuGraphicsSubSelection, GraphicsDisplayModeSubSelection)
                | otherwise            -> (SettingsMenuGraphicsSubSelection, GraphicsResolutionSubSelection)

        SettingsMenuCloseSelection
            | upPressed                   -> (SettingsMenuGraphicsSubSelection, GraphicsDisplayModeSubSelection)
            | downPressed                 -> (SettingsMenuGraphicsTabSelection, GraphicsNoSubSelection)
            | leftPressed || rightPressed -> (SettingsMenuGraphicsSubSelection, GraphicsRestoreDefaultsSubSelection)

        SettingsMenuGraphicsSubSelection -> case _subSelection (graphicsTab :: SettingsGraphicsTab) of
            GraphicsResolutionSubSelection
                | upPressed   -> (SettingsMenuGraphicsTabSelection, GraphicsNoSubSelection)
                | downPressed -> (SettingsMenuGraphicsSubSelection, GraphicsDisplayModeSubSelection)

            GraphicsDisplayModeSubSelection
                | upPressed   -> if
                    | isResolutionDisabled -> (SettingsMenuGraphicsTabSelection, GraphicsNoSubSelection)
                    | otherwise            -> (SettingsMenuGraphicsSubSelection, GraphicsResolutionSubSelection)
                | downPressed -> (SettingsMenuCloseSelection, GraphicsNoSubSelection)

            GraphicsRestoreDefaultsSubSelection
                | upPressed                   -> (SettingsMenuGraphicsSubSelection, GraphicsDisplayModeSubSelection)
                | downPressed                 -> (SettingsMenuGraphicsTabSelection, GraphicsNoSubSelection)
                | leftPressed || rightPressed -> (SettingsMenuCloseSelection, GraphicsNoSubSelection)

            subSelection -> (SettingsMenuGraphicsSubSelection, subSelection)

        _ -> (selection, GraphicsNoSubSelection)

writeSetGraphicsResolutionMsgs :: MsgsWrite MenuMsgsPhase m => (Int, Int) -> m ()
writeSetGraphicsResolutionMsgs (winWidth, winHeight) = writeMsgs
    [ mkMsg $ ConsoleMsgSetGraphicsResolution winWidth winHeight
    , mkMsg ConsoleMsgSaveSettings
    ]

writeSetGraphicsDisplayModeMsgs :: MsgsWrite MenuMsgsPhase m => WindowMode -> m ()
writeSetGraphicsDisplayModeMsgs winMode = writeMsgs
    [ mkMsg $ ConsoleMsgSetGraphicsWindowMode winMode
    , mkMsg ConsoleMsgSaveSettings
    ]

writeRestoreDefaultsMsgs :: MsgsWrite MenuMsgsPhase m => m ()
writeRestoreDefaultsMsgs = writeMsgs
    [ mkMsg ConsoleMsgRestoreDefaultSettingsRender
    , mkMsg ConsoleMsgSaveSettings
    ]

updateTabButtons :: SettingsMenuData -> SettingsTabButtons -> (SettingsTabChoice, SettingsTabButtons)
updateTabButtons settingsMenuData tabBtns
    | _isPressed (_controlsButton tabBtns :: Button) =
        ( ControlsTabChoice
        , _buttons (_controlsTab settingsMenuData :: SettingsControlsTab)
        )
    | _isPressed (_audioButton tabBtns :: Button)    =
        ( AudioTabChoice
        , _buttons (_audioTab settingsMenuData :: SettingsAudioTab)
        )
    | _isPressed (_gameButton tabBtns :: Button)     =
        ( GameTabChoice
        , _buttons (_gameTab settingsMenuData :: SettingsGameTab)
        )
    | _isPressed (_creditsButton tabBtns :: Button)  =
        ( CreditsTabChoice
        , _buttons (_creditsTab settingsMenuData :: SettingsCreditsTab)
        )
    | otherwise                                      = (GraphicsTabChoice, tabBtns)

updateSettingsGraphicsTab
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsWrite MenuMsgsPhase m)
    => SettingsMenuSelection
    -> SettingsTabButtons
    -> SettingsMenuData
    -> m (SettingsMenuData -> SettingsMenuData)
updateSettingsGraphicsTab selection tabBtns settingsMenuData =
    let
        (tabChoice, tabBtns') = updateTabButtons settingsMenuData tabBtns
        graphicsTab           = _graphicsTab settingsMenuData
        isAnyExpandedComboBox = isSettingsGraphicsTabExpandedComboBox graphicsTab
    in do
        (selection', subSelection) <- if
            | isAnyExpandedComboBox -> return (selection, _subSelection (graphicsTab :: SettingsGraphicsTab))
            | otherwise             -> updateSelections selection graphicsTab

        flip evalStateT (selection', subSelection) $
            let
                isResolutionComboBoxAndDisabled = \subSelect ->
                    subSelect == GraphicsResolutionSubSelection && isResolutionComboBoxDisabled graphicsTab

                updateComboBoxAndSelection = \comboBoxSubSelect valueText comboBox -> do
                    comboBoxStatus <- get <&> \(_, subSelect) -> if
                        | isResolutionComboBoxAndDisabled comboBoxSubSelect   -> ComboBoxInactiveStatus
                        | isAnyExpandedComboBox && not (_isExpanded comboBox) -> ComboBoxInactiveStatus
                        | subSelect == comboBoxSubSelect                      -> ComboBoxSelectedActiveStatus
                        | otherwise                                           -> ComboBoxActiveStatus
                    comboBox'      <- lift $ updateComboBox comboBoxStatus valueText comboBox

                    when (_isSelected (comboBox' :: ComboBox) || _isExpanded comboBox') $
                        put (SettingsMenuGraphicsSubSelection, comboBoxSubSelect)
                    return comboBox'

                updateButtonAndSelection = \btnSubSelect btn -> do
                    btnStatus <- get <&> \(_, subSelect) -> if
                        | isAnyExpandedComboBox     -> ButtonInactiveStatus
                        | subSelect == btnSubSelect -> ButtonSelectedActiveStatus
                        | otherwise                 -> ButtonActiveStatus
                    btn'      <- lift $ updateButton btnStatus btn

                    when (_isSelected (btn' :: Button) || _isPressed (btn' :: Button)) $
                        put (SettingsMenuGraphicsSubSelection, btnSubSelect)
                    return btn'
            in do
                displayIndex        <- lift getGraphicsDisplayIndex
                resolutionText      <- lift getCurrentResolutionText
                resolutionComboBox' <- if
                    | displayIndex /= _windowDisplayIndex graphicsTab -> lift mkResolutionComboBox
                    | otherwise                                       ->
                        let comboBox = _resolutionComboBox graphicsTab
                        in updateComboBoxAndSelection GraphicsResolutionSubSelection resolutionText comboBox
                let resolutionText'  = comboBoxValue resolutionComboBox'
                when (resolutionText' /= resolutionText) $
                    traverse_ (lift . writeSetGraphicsResolutionMsgs) (parseResolution resolutionText')

                displayModeText        <- lift $ formatWindowMode <$> getGraphicsWindowMode
                let displayModeComboBox = _displayModeComboBox graphicsTab
                displayModeComboBox'   <-
                    updateComboBoxAndSelection GraphicsDisplayModeSubSelection displayModeText displayModeComboBox
                let displayModeText'    = comboBoxValue displayModeComboBox'
                when (displayModeText' /= displayModeText) $
                    lift $ writeSetGraphicsDisplayModeMsgs (parseDisplayMode displayModeText')

                let restoreDefaultsBtn = _restoreDefaultsButton (graphicsTab :: SettingsGraphicsTab)
                restoreDefaultsBtn'   <- updateButtonAndSelection
                    GraphicsRestoreDefaultsSubSelection
                    restoreDefaultsBtn
                when (_isPressed (restoreDefaultsBtn' :: Button)) $
                    lift writeRestoreDefaultsMsgs

                let soundIndices = _soundIndices (settingsMenuData :: SettingsMenuData)
                when (_isPressed (resolutionComboBox' :: ComboBox) || _isPressed (displayModeComboBox' :: ComboBox)) $
                    void $ playFmodSound (_confirmAlt soundIndices)

                when (_isPressed (restoreDefaultsBtn' :: Button)) $
                    void $ playFmodSound (_confirmSmall soundIndices)

                get <&> \(select, subSelect) ->
                    let
                        graphicsTab' = graphicsTab
                            { _resolutionComboBox    = resolutionComboBox'
                            , _displayModeComboBox   = displayModeComboBox'
                            , _restoreDefaultsButton = restoreDefaultsBtn'
                            , _subSelection          = subSelect
                            , _windowDisplayIndex    = displayIndex
                            }
                    in \smd -> smd
                        { _tabChoice   = tabChoice
                        , _tabButtons  = tabBtns'
                        , _graphicsTab = graphicsTab'
                        , _selection   = select
                        }

drawSettingsGraphicsTab :: (GraphicsReadWrite m, MonadIO m) => SettingsGraphicsTab -> m ()
drawSettingsGraphicsTab graphicsTab = do
    drawImage zeroPos2 RightDir menuOverZIndex (_backgroundImage (graphicsTab :: SettingsGraphicsTab))

    let resolutionComboBox = _resolutionComboBox graphicsTab
    drawComboBox menuOverZIndex menuOverExpandedZIndex resolutionComboBox
    when (isResolutionComboBoxDisabled graphicsTab) $
        let pos = _pos (resolutionComboBox :: ComboBox)
        in drawImage pos RightDir menuOverZIndex (_disabledComboBoxOverlayImage graphicsTab)

    drawComboBox menuOverZIndex menuOverExpandedZIndex (_displayModeComboBox graphicsTab)
    drawButton menuOverZIndex (_restoreDefaultsButton (graphicsTab :: SettingsGraphicsTab))

isSettingsGraphicsTabExpandedComboBox :: SettingsGraphicsTab -> Bool
isSettingsGraphicsTabExpandedComboBox graphicsTab = or $ map _isExpanded
    [ _resolutionComboBox graphicsTab
    , _displayModeComboBox graphicsTab
    ]
