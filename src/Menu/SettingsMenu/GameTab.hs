module Menu.SettingsMenu.GameTab
    ( mkSettingsGameTab
    , updateSettingsGameTab
    , drawSettingsGameTab
    , isSettingsGameTabExpandedComboBox
    ) where

import Control.Monad          (when, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (StateT, evalStateT, get, lift, put)
import Data.Functor           ((<&>))
import qualified Data.Text as T

import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Configs.All.Settings.Menu
import FileCache
import Menu.SettingsMenu.GameTab.Types
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
audioBtnImgPath              = settingsMenuPack "audio-button-inactive.image"    :: PackResourceFilePath
gameBtnImgPath               = settingsMenuPack "game-button.image"              :: PackResourceFilePath
creditsBtnImgPath            = settingsMenuPack "credits-button-inactive.image"  :: PackResourceFilePath
backgroundImgPath            = settingsMenuPack "game-background.image"          :: PackResourceFilePath
restoreDefaultsButtonImgPath = settingsMenuPack "restore-default-game.image"     :: PackResourceFilePath

enemyHealthText       = "Enemy Health"     :: T.Text
pauseMenuHintsText    = "Pause Menu Hints" :: T.Text
enemyHealthTextPos    = Pos2 825.0 191.0   :: Pos2
pauseMenuHintsTextPos = Pos2 825.0 272.0   :: Pos2

comboBoxImgPath              = settingsMenuPack "game-combo-box.image"   :: PackResourceFilePath
enemyHealthComboBoxValues    = map formatEnemyHealthPercent [minBound..] :: [T.Text]
pauseMenuHintsComboBoxValues = ["Enabled", "Disabled"]                   :: [T.Text]

formatEnemyHealthPercent :: EnemyHealthPercent -> T.Text
formatEnemyHealthPercent = \case
    EnemyHealth100Percent -> "100%"
    EnemyHealth150Percent -> "150%"
    EnemyHealth200Percent -> "200%"
    EnemyHealth300Percent -> "300%"
    EnemyHealth500Percent -> "500%"

readEnemyHealthPercent :: T.Text -> EnemyHealthPercent
readEnemyHealthPercent = \case
    "150%" -> EnemyHealth150Percent
    "200%" -> EnemyHealth200Percent
    "300%" -> EnemyHealth300Percent
    "500%" -> EnemyHealth500Percent
    _      -> EnemyHealth100Percent

formatEnabled :: Bool -> T.Text
formatEnabled = \case
    True  -> "Enabled"
    False -> "Disabled"

readEnabled :: T.Text -> Bool
readEnabled = \case
    "Disabled" -> False
    _          -> True

mkSettingsGameTab :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SettingsGameTab
mkSettingsGameTab = do
    tabBtns                  <-
        mkSettingsTabButtons controlsBtnImgPath graphicsBtnImgPath audioBtnImgPath gameBtnImgPath creditsBtnImgPath
    backgroundImg            <- loadPackImage backgroundImgPath
    enemyHealthDisplayTxt    <- mkDisplayText enemyHealthText Font32 whiteColor
    pauseMenuHintsDisplayTxt <- mkDisplayText pauseMenuHintsText Font32 whiteColor
    restoreDefaultsBtnPos    <- readSettingsConfig _menu _settingsGameTabRestoreDefaultsButtonPos
    restoreDefaultsBtn       <- mkImageButtonCentered restoreDefaultsBtnPos restoreDefaultsButtonImgPath
    menuCfg                  <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)

    enemyHealthComboBox <- mkComboBox
        (_settingsGameTabEnemyHealthComboBoxPos menuCfg)
        (formatEnemyHealthPercent EnemyHealth100Percent)
        enemyHealthComboBoxValues
        Font32
        menuOptionBtnColor
        hoverMenuOptionBtnColor
        (_settingsGameTabEnemyHealthComboBoxValueOffset menuCfg)
        comboBoxImgPath

    pauseMenuHintsComboBox <- mkComboBox
        (_settingsGameTabPauseMenuHintsComboBoxPos menuCfg)
        (formatEnabled True)
        pauseMenuHintsComboBoxValues
        Font32
        menuOptionBtnColor
        hoverMenuOptionBtnColor
        (_settingsGameTabPauseMenuHintsComboBoxValueOffset menuCfg)
        comboBoxImgPath

    return $ SettingsGameTab
        { _buttons                   = tabBtns
        , _backgroundImage           = backgroundImg
        , _enemyHealthDisplayText    = enemyHealthDisplayTxt
        , _pauseMenuHintsDisplayText = pauseMenuHintsDisplayTxt
        , _enemyHealthComboBox       = enemyHealthComboBox
        , _pauseMenuHintsComboBox    = pauseMenuHintsComboBox
        , _restoreDefaultsButton     = restoreDefaultsBtn
        , _subSelection              = GameNoSubSelection
        }

updateSelections
    :: InputRead m
    => SettingsMenuSelection
    -> SettingsGameTab
    -> m (SettingsMenuSelection, GameSubSelection)
updateSelections selection gameTab = do
    inputState <- readInputState
    let
        upPressed    = MenuUpAlias `aliasPressed` inputState
        downPressed  = MenuDownAlias `aliasPressed` inputState
        leftPressed  = MenuLeftAlias `aliasPressed` inputState
        rightPressed = MenuRightAlias `aliasPressed` inputState

    return $ case selection of
        SettingsMenuControlsTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, GameNoSubSelection)
            | downPressed                 -> (SettingsMenuGameSubSelection, GameEnemyHealthSubSelection)
        SettingsMenuGraphicsTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, GameNoSubSelection)
            | downPressed                 -> (SettingsMenuGameSubSelection, GameEnemyHealthSubSelection)
        SettingsMenuAudioTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, GameNoSubSelection)
            | downPressed                 -> (SettingsMenuGameSubSelection, GameEnemyHealthSubSelection)
        SettingsMenuGameTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, GameNoSubSelection)
            | downPressed                 -> (SettingsMenuGameSubSelection, GameEnemyHealthSubSelection)
        SettingsMenuCreditsTabSelection
            | upPressed                   -> (SettingsMenuCloseSelection, GameNoSubSelection)
            | downPressed                 -> (SettingsMenuGameSubSelection, GameEnemyHealthSubSelection)
        SettingsMenuCloseSelection
            | upPressed                   -> (SettingsMenuGameSubSelection, GamePauseMenuHintsSubSelection)
            | downPressed                 -> (SettingsMenuGameTabSelection, GameNoSubSelection)
            | leftPressed || rightPressed -> (SettingsMenuGameSubSelection, GameRestoreDefaultsSubSelection)

        SettingsMenuGameSubSelection -> case _subSelection (gameTab :: SettingsGameTab) of
            GameEnemyHealthSubSelection
                | upPressed   -> (SettingsMenuGameTabSelection, GameNoSubSelection)
                | downPressed -> (SettingsMenuGameSubSelection, GamePauseMenuHintsSubSelection)

            GamePauseMenuHintsSubSelection
                | upPressed   -> (SettingsMenuGameTabSelection, GameEnemyHealthSubSelection)
                | downPressed -> (SettingsMenuCloseSelection, GameNoSubSelection)

            GameRestoreDefaultsSubSelection
                | upPressed                   -> (SettingsMenuGameSubSelection, GamePauseMenuHintsSubSelection)
                | downPressed                 -> (SettingsMenuGameTabSelection, GameNoSubSelection)
                | leftPressed || rightPressed -> (SettingsMenuCloseSelection, GameNoSubSelection)
            subSelection                      -> (SettingsMenuGameSubSelection, subSelection)

        _ -> (selection, GameNoSubSelection)

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
    | _isPressed (_audioButton tabBtns :: Button)    =
        ( AudioTabChoice
        , _buttons (_audioTab settingsMenuData :: SettingsAudioTab)
        )
    | _isPressed (_creditsButton tabBtns :: Button)  =
        ( CreditsTabChoice
        , _buttons (_creditsTab settingsMenuData :: SettingsCreditsTab)
        )
    | otherwise                                      = (GameTabChoice, tabBtns)

writeSetEnemyHealthMsgs :: MsgsWrite MenuMsgsPhase m => EnemyHealthPercent -> m ()
writeSetEnemyHealthMsgs enemyHealthPercent = writeMsgs
    [ mkMsg $ ConsoleMsgSetEnemyHealth enemyHealthPercent
    , mkMsg ConsoleMsgSaveSettings
    ]

writeSetShowPauseMenuHintsMsgs :: MsgsWrite MenuMsgsPhase m => Bool -> m ()
writeSetShowPauseMenuHintsMsgs isPauseMenuHints = writeMsgs
    [ mkMsg $ ConsoleMsgSetPauseMenuHints isPauseMenuHints
    , mkMsg ConsoleMsgSaveSettings
    ]

writeRestoreDefaultsMsgs :: MsgsWrite MenuMsgsPhase m => m ()
writeRestoreDefaultsMsgs = writeMsgs
    [ mkMsg ConsoleMsgRestoreDefaultSettingsGame
    , mkMsg ConsoleMsgSaveSettings
    ]

updateSettingsGameTab
    :: (ConfigsRead m, InputRead m, MonadIO m, MsgsWrite MenuMsgsPhase m)
    => SettingsMenuSelection
    -> SettingsTabButtons
    -> SettingsMenuData
    -> m (SettingsMenuData -> SettingsMenuData)
updateSettingsGameTab selection tabBtns settingsMenuData =
    let
        (tabChoice, tabBtns') = updateTabButtons settingsMenuData tabBtns
        gameTab               = _gameTab settingsMenuData
        isAnyExpandedComboBox = isSettingsGameTabExpandedComboBox gameTab
    in do
        (selection', subSelection) <- if
            | isAnyExpandedComboBox -> return (selection, _subSelection (gameTab :: SettingsGameTab))
            | otherwise             -> updateSelections selection gameTab

        flip evalStateT (selection', subSelection) $
            let
                updateComboBoxAndSelection
                    :: InputRead m1
                    => GameSubSelection
                    -> T.Text
                    -> ComboBox
                    -> StateT (SettingsMenuSelection, GameSubSelection) m1 ComboBox
                updateComboBoxAndSelection comboBoxSubSelect valueText comboBox = do
                    comboBoxStatus <- get <&> \(_, subSelect) -> if
                        | isAnyExpandedComboBox && not (_isExpanded comboBox) -> ComboBoxInactiveStatus
                        | subSelect == comboBoxSubSelect                      -> ComboBoxSelectedActiveStatus
                        | otherwise                                           -> ComboBoxActiveStatus
                    comboBox'      <- lift $ updateComboBox comboBoxStatus valueText comboBox

                    when (_isSelected (comboBox' :: ComboBox) || _isExpanded comboBox') $
                        put (SettingsMenuGameSubSelection, comboBoxSubSelect)
                    return comboBox'

                updateButtonAndSelection = \btnSubSelect btn -> do
                    btnStatus <- get <&> \(_, subSelect) -> if
                        | isAnyExpandedComboBox     -> ButtonInactiveStatus
                        | subSelect == btnSubSelect -> ButtonSelectedActiveStatus
                        | otherwise                 -> ButtonActiveStatus
                    btn'      <- lift $ updateButton btnStatus btn

                    when (_isSelected (btn' :: Button) || _isPressed (btn' :: Button)) $
                        put (SettingsMenuGameSubSelection, btnSubSelect)
                    return btn'
            in do
                debugCfg <- lift $ readConfig _settings _debug

                let enemyHealthPercent = enemyDamageMultiplierToHealthPercent (_enemiesDamageMultiplier debugCfg)
                enemyHealthComboBox   <- updateComboBoxAndSelection
                    GameEnemyHealthSubSelection
                    (formatEnemyHealthPercent enemyHealthPercent)
                    (_enemyHealthComboBox gameTab)

                let enemyHealthPercent' = readEnemyHealthPercent $ comboBoxValue enemyHealthComboBox
                when (enemyHealthPercent' /= enemyHealthPercent) $
                    lift $ writeSetEnemyHealthMsgs enemyHealthPercent'

                let isPauseMenuHints = not $ _disablePauseMenuHints debugCfg
                pauseMenuHintsComboBox  <- updateComboBoxAndSelection
                    GamePauseMenuHintsSubSelection
                    (formatEnabled isPauseMenuHints)
                    (_pauseMenuHintsComboBox gameTab)

                let isPauseMenuHints' = readEnabled $ comboBoxValue pauseMenuHintsComboBox
                when (isPauseMenuHints' /= isPauseMenuHints) $
                    lift $ writeSetShowPauseMenuHintsMsgs isPauseMenuHints'

                let restoreDefaultsBtn = _restoreDefaultsButton (gameTab :: SettingsGameTab)
                restoreDefaultsBtn'   <- updateButtonAndSelection GameRestoreDefaultsSubSelection restoreDefaultsBtn
                when (_isPressed (restoreDefaultsBtn' :: Button)) $
                    lift writeRestoreDefaultsMsgs

                let soundIndices = _soundIndices (settingsMenuData :: SettingsMenuData)
                when (or [_isPressed (cb :: ComboBox) | cb <- [enemyHealthComboBox, pauseMenuHintsComboBox]]) $
                    void $ playFmodSound (_confirmAlt soundIndices)

                when (_isPressed (restoreDefaultsBtn' :: Button)) $
                    void $ playFmodSound (_confirmSmall soundIndices)

                get <&> \(select, subSelect) ->
                    let
                        gameTab' = gameTab
                            { _enemyHealthComboBox    = enemyHealthComboBox
                            , _pauseMenuHintsComboBox = pauseMenuHintsComboBox
                            , _restoreDefaultsButton  = restoreDefaultsBtn'
                            , _subSelection           = subSelect
                            } :: SettingsGameTab
                    in \smd -> smd
                        { _tabChoice  = tabChoice
                        , _tabButtons = tabBtns'
                        , _gameTab    = gameTab'
                        , _selection  = select
                        }

drawSettingsGameTab :: (GraphicsReadWrite m, MonadIO m) => SettingsGameTab -> m ()
drawSettingsGameTab gameTab = do
    drawImage zeroPos2 RightDir menuOverZIndex (_backgroundImage (gameTab :: SettingsGameTab))
    drawDisplayTextRightAligned enemyHealthTextPos menuOverZIndex (_enemyHealthDisplayText gameTab)
    drawDisplayTextRightAligned pauseMenuHintsTextPos menuOverZIndex (_pauseMenuHintsDisplayText gameTab)
    drawComboBox menuOverZIndex menuOverExpandedZIndex (_enemyHealthComboBox gameTab)
    drawComboBox menuOverZIndex menuOverExpandedZIndex (_pauseMenuHintsComboBox gameTab)
    drawButton menuOverZIndex (_restoreDefaultsButton (gameTab :: SettingsGameTab))

isSettingsGameTabExpandedComboBox :: SettingsGameTab -> Bool
isSettingsGameTabExpandedComboBox gameTab = or $ map _isExpanded
    [ _enemyHealthComboBox gameTab
    , _pauseMenuHintsComboBox gameTab
    ]
