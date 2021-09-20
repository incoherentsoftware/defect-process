module Menu.PauseMenu
    ( mkPauseMenuData
    , drawPauseMenu
    , pauseMenuMain
    ) where

import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (StateT, evalStateT, get, lift, put)
import Data.Foldable          (for_, sequenceA_, traverse_)
import Data.Functor           ((<&>))
import Data.Maybe             (catMaybes, isJust, isNothing)
import Data.Traversable       (for)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import AppEnv
import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Configs.All.Settings.Menu
import FileCache
import Game.Types
import Menu.HelpPopup.Util
import Menu.PauseMenu.HelpEntry as HE
import Menu.PauseMenu.Selection
import Menu.PauseMenu.SlotComboBoxes
import Menu.PauseMenu.Types
import Menu.SettingsMenu
import Menu.SoundIndices
import Menu.Types
import Menu.Util
import Menu.ZIndex
import Msg
import Player.EquipmentInfo
import Player.Gun.Types
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Upgrade
import Player.Weapon.Types
import Util
import Window
import Window.Graphics.UiControls
import World

pauseMenuPack = \p -> PackResourceFilePath "data/menu/pause-menu.pack" p

resumeButtonImgPath       = pauseMenuPack "resume-button.image"         :: PackResourceFilePath
mainMenuButtonImgPath     = pauseMenuPack "main-menu-button.image"      :: PackResourceFilePath
settingsButtonImgPath     = pauseMenuPack "settings-button.image"       :: PackResourceFilePath
bgImagePath               = pauseMenuPack "pause-menu-background.image" :: PackResourceFilePath
emptyIconOverlayImagePath = pauseMenuPack "empty-icon-overlay.image"    :: PackResourceFilePath
infoOverlayLeftImagePath  = pauseMenuPack "info-overlay-left.image"     :: PackResourceFilePath
infoOverlayRightImagePath = pauseMenuPack "info-overlay-right.image"    :: PackResourceFilePath

secondarySkillUpIconOverlayImgPath   = pauseMenuPack "secondary-skill-up-icon-overlay.image"   :: PackResourceFilePath
secondarySkillDownIconOverlayImgPath = pauseMenuPack "secondary-skill-down-icon-overlay.image" :: PackResourceFilePath

secondarySkillNeutralInputOverlayImgPath =
    pauseMenuPack "secondary-skill-neutral-input-overlay.image" :: PackResourceFilePath
secondarySkillUpInputOverlayImgPath      =
    pauseMenuPack "secondary-skill-up-input-overlay.image"      :: PackResourceFilePath
secondarySkillDownInputOverlayImgPath    =
    pauseMenuPack "secondary-skill-down-input-overlay.image"    :: PackResourceFilePath

viewInfoText   = "View Info: {MenuSelectAlias.1}"        :: T.Text
changeSlotText = "Reassign Input: {MenuSlotChangeAlias}" :: T.Text

selectionInfoCenterTextPos@(Pos2 selectionInfoCenterTextX selectionInfoCenterTextY) = Pos2 960.0 834.0 :: Pos2
selectionInfoSpacerWidth                                                            = 30.0             :: Float

infoOverlayLeftImagePos                     = Pos2 99.0 498.0 :: Pos2
infoOverlayRightImagePos                    = Pos2 1225.0 249.0 :: Pos2
emptySecondarySkillUpDownIconOverlayOpacity = Opacity 128       :: Opacity
upgradeCountOverlayOffset                   = Pos2 10.0 10.0    :: Pos2
secondarySkillInputOverlayImageOffset       = Pos2 130.0 126.0  :: Pos2

emptySelectionInfoSelections = S.fromList
    [ PauseMenuResumeSelection
    , PauseMenuMainSelection
    , PauseMenuSettingsSelection
    ] :: S.Set PauseMenuSelection

secondarySkillSelections = S.fromList
    [ PauseMenuSecondarySkillLeftSelection
    , PauseMenuSecondarySkillMidSelection
    , PauseMenuSecondarySkillRightSelection
    ] :: S.Set PauseMenuSelection

mkPauseMenuData
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => m PauseMenuData
mkPauseMenuData = do
    cfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)

    backgroundImg                        <- loadPackImage bgImagePath
    emptyIconOverlayImg                  <- loadPackImage emptyIconOverlayImagePath
    secondarySkillUpIconOverlayImg       <- loadPackImage secondarySkillUpIconOverlayImgPath
    secondarySkillDownIconOverlayImg     <- loadPackImage secondarySkillDownIconOverlayImgPath
    secondarySkillNeutralInputOverlayImg <- loadPackImage secondarySkillNeutralInputOverlayImgPath
    secondarySkillUpInputOverlayImg      <- loadPackImage secondarySkillUpInputOverlayImgPath
    secondarySkillDownInputOverlayImg    <- loadPackImage secondarySkillDownInputOverlayImgPath
    infoOverlayLeftImg                   <- loadPackImage infoOverlayLeftImagePath
    infoOverlayRightImg                  <- loadPackImage infoOverlayRightImagePath

    resumeBtn   <- mkImageButton (_pausedResumeButtonPos cfg) resumeButtonImgPath
    mainMenuBtn <- mkImageButton (_pausedMainMenuButtonPos cfg) mainMenuButtonImgPath
    settingsBtn <- mkImageButton (_pausedSettingsButtonPos cfg) settingsButtonImgPath

    settingsMenuData          <- mkSettingsMenuData
    viewInfoInputDisplayTxt   <- mkInputDisplayText viewInfoText Font22 whiteColor
    changeSlotInputDisplayTxt <- mkInputDisplayText changeSlotText Font22 whiteColor
    slotComboBoxes            <- mkSlotComboBoxes

    generalHelpEntry   <- mkPauseMenuHelpEntry (_pausedGeneralInfoHelpEntryPos cfg) () generalInfoHelpPopupDescription
    targetingHelpEntry <-
        mkPauseMenuHelpEntry (_pausedTargetingInfoHelpEntryPos cfg) () (targetingInfoHelpPopupDescription cfg)

    musicIndex   <- getFmodMusic menuMusicPath
    soundIndices <- mkMenuSoundIndices

    return $ PauseMenuData
        { _backgroundImage                        = backgroundImg
        , _emptyIconOverlayImage                  = emptyIconOverlayImg
        , _infoOverlayLeftImage                   = infoOverlayLeftImg
        , _infoOverlayRightImage                  = infoOverlayRightImg
        , _secondarySkillUpIconOverlayImage       = secondarySkillUpIconOverlayImg
        , _secondarySkillDownIconOverlayImage     = secondarySkillDownIconOverlayImg
        , _secondarySkillNeutralInputOverlayImage = secondarySkillNeutralInputOverlayImg
        , _secondarySkillUpInputOverlayImage      = secondarySkillUpInputOverlayImg
        , _secondarySkillDownInputOverlayImage    = secondarySkillDownInputOverlayImg
        , _upgradeCountOverlayDisplayTexts        = M.empty
        , _resumeButton                           = resumeBtn
        , _mainMenuButton                         = mainMenuBtn
        , _settingsButton                         = settingsBtn
        , _settingsMenuData                       = settingsMenuData
        , _viewInfoInputDisplayText               = viewInfoInputDisplayTxt
        , _changeSlotInputDisplayText             = changeSlotInputDisplayTxt
        , _generalHelpEntry                       = generalHelpEntry
        , _targetingHelpEntry                     = targetingHelpEntry
        , _weaponHelpEntries                      = []
        , _gunHelpEntries                         = []
        , _movementSkillHelpEntries               = []
        , _secondarySkillNeutralHelpEntry         = Nothing
        , _secondarySkillUpHelpEntry              = Nothing
        , _secondarySkillDownHelpEntry            = Nothing
        , _upgradeHelpEntries                     = []
        , _slotComboBoxes                         = slotComboBoxes
        , _equipmentInfo                          = mkEmptyPlayerEquipmentInfo
        , _selection                              = Nothing
        , _musicIndex                             = musicIndex
        , _soundIndices                           = soundIndices
        }

allOverlaysInactive :: PauseMenuData -> Bool
allOverlaysInactive pauseMenuData = and
    [ allInactive [_generalHelpEntry pauseMenuData, _targetingHelpEntry pauseMenuData]
    , allInactive $ _weaponHelpEntries pauseMenuData
    , allInactive $ _gunHelpEntries pauseMenuData
    , allInactive $ _movementSkillHelpEntries pauseMenuData
    , allInactive secondarySkillHelpEntries
    , allInactive $ _upgradeHelpEntries pauseMenuData
    , not $ isSlotComboBoxesExpanded (_slotComboBoxes pauseMenuData)
    , not $ _active (_settingsMenuData pauseMenuData :: SettingsMenuData)
    ]
    where
        allInactive               = \helpEntries -> and $ map (not . HE._active) helpEntries
        secondarySkillHelpEntries = catMaybes
            [ _secondarySkillNeutralHelpEntry pauseMenuData
            , _secondarySkillUpHelpEntry pauseMenuData
            , _secondarySkillDownHelpEntry pauseMenuData
            ]

currentListHelpEntries
    :: (ConfigsRead m, Eq a, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => [a]
    -> [Pos2]
    -> (a -> HelpPopupDescription)
    -> [PauseMenuHelpEntry a]
    -> m [PauseMenuHelpEntry a]
currentListHelpEntries equipmentTypes configHelpEntryPositions menuHelpEntryDescF helpEntries =
    readSettingsConfig _debug _disableMenuHelpEntries >>= \case
        True  -> return []
        False -> do
            let helpEntryTypes = map HE._type helpEntries
            newHelpEntries <- sequenceA
                [ mkPauseMenuHelpEntry zeroPos2 equipmentType (menuHelpEntryDescF equipmentType)
                | equipmentType <- equipmentTypes
                , equipmentType `notElem` helpEntryTypes
                ]
            let helpEntries' = filter ((`elem` equipmentTypes) . HE._type) helpEntries ++ newHelpEntries
            return $ map (uncurry setPauseMenuHelpEntryIconButtonPos) (zip configHelpEntryPositions helpEntries')

currentWeaponHelpEntries
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => PlayerEquipmentInfo
    -> PauseMenuData
    -> m [PauseMenuHelpEntry WeaponType]
currentWeaponHelpEntries equipmentInfo pauseMenuData = do
    cfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    currentListHelpEntries
        (_weaponTypes equipmentInfo)
        (_pausedWeaponHelpEntryPositions cfg)
        (weaponTypeToHelpPopupDescription cfg)
        (_weaponHelpEntries pauseMenuData)

currentGunHelpEntries
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => PlayerEquipmentInfo
    -> PauseMenuData
    -> m [PauseMenuHelpEntry GunType]
currentGunHelpEntries equipmentInfo pauseMenuData = do
    cfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    currentListHelpEntries
        (_gunTypes equipmentInfo)
        (_pausedGunHelpEntryPositions cfg)
        gunTypeToHelpPopupDescription
        (_gunHelpEntries pauseMenuData)

currentMovementSkillHelpEntries
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => PlayerEquipmentInfo
    -> PauseMenuData
    -> m [PauseMenuHelpEntry MovementSkillType]
currentMovementSkillHelpEntries equipmentInfo pauseMenuData = do
    cfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    currentListHelpEntries
        (_movementSkillTypes equipmentInfo)
        (_pausedMovementSkillHelpEntryPositions cfg)
        movementSkillTypeToHelpPopupDescription
        (_movementSkillHelpEntries pauseMenuData)

currentUpgradeHelpEntries
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => PlayerEquipmentInfo
    -> PauseMenuData
    -> m [PauseMenuHelpEntry PlayerUpgradeType]
currentUpgradeHelpEntries equipmentInfo pauseMenuData = do
    cfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    currentListHelpEntries
        (playerEquipmentInfoUpgradeTypes equipmentInfo)
        (_pausedUpgradeHelpEntryPositions cfg)
        upgradeTypeToHelpPopupDescription
        (_upgradeHelpEntries pauseMenuData)

currentSecondarySkillHelpEntries
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => PlayerEquipmentInfo
    -> PauseMenuData
    -> m
        ( Maybe (PauseMenuHelpEntry SecondarySkillType)
        , Maybe (PauseMenuHelpEntry SecondarySkillType)
        , Maybe (PauseMenuHelpEntry SecondarySkillType)
        )
currentSecondarySkillHelpEntries equipmentInfo pauseMenuData =
    let
        neutralHelpEntry = _secondarySkillNeutralHelpEntry pauseMenuData
        upHelpEntry      = _secondarySkillUpHelpEntry pauseMenuData
        downHelpEntry    = _secondarySkillDownHelpEntry pauseMenuData
    in readSettingsConfig _debug _disableMenuHelpEntries >>= \case
        True  -> return (neutralHelpEntry, upHelpEntry, downHelpEntry)
        False -> do
            cfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)

            neutralHelpEntry' <- case _secondarySkillNeutralType equipmentInfo of
                Nothing                                                          -> return Nothing
                Just neutralType
                    | maybe False ((neutralType ==) . HE._type) neutralHelpEntry -> return neutralHelpEntry
                    | otherwise                                                  ->
                        let
                            pos           = _pausedSecondarySkillNeutralHelpEntryPos cfg
                            helpPopupDesc =
                                secondarySkillTypeToHelpPopupDescription neutralType SecondarySkillNeutralSlot
                        in Just <$> mkPauseMenuHelpEntry pos neutralType helpPopupDesc

            upHelpEntry' <- case _secondarySkillUpType equipmentInfo of
                Nothing                                                -> return Nothing
                Just upType
                    | maybe False ((upType ==) . HE._type) upHelpEntry -> return upHelpEntry
                    | otherwise                                        ->
                        let
                            pos           = _pausedSecondarySkillUpHelpEntryPos cfg
                            helpPopupDesc = secondarySkillTypeToHelpPopupDescription upType SecondarySkillUpSlot
                        in Just <$> mkPauseMenuHelpEntry pos upType helpPopupDesc


            downHelpEntry' <- case _secondarySkillDownType equipmentInfo of
                Nothing                                                    -> return Nothing
                Just downType
                    | maybe False ((downType ==) . HE._type) downHelpEntry -> return downHelpEntry
                    | otherwise                                            ->
                        let
                            pos           = _pausedSecondarySkillDownHelpEntryPos cfg
                            helpPopupDesc = secondarySkillTypeToHelpPopupDescription downType SecondarySkillDownSlot
                        in Just <$> mkPauseMenuHelpEntry pos downType helpPopupDesc

            return $ (neutralHelpEntry', upHelpEntry', downHelpEntry')

updateUpgradeCountOverlayDisplayText
    :: (GraphicsRead m, MonadIO m)
    => PlayerEquipmentInfo
    -> M.Map PlayerUpgradeType DisplayText
    -> m (M.Map PlayerUpgradeType DisplayText)
updateUpgradeCountOverlayDisplayText equipmentInfo upgradeCountOverlayDisplayTxts = do
    let upgradeCounts = _upgradeCounts equipmentInfo
    (M.fromList <$>) .
        for (M.toList upgradeCounts) $ \(upgradeType, count) -> do
            let txt = "+" <> T.pack (show count)
            displayTxt <- case upgradeType `M.lookup` upgradeCountOverlayDisplayTxts of
                Just dspTxt -> return $ updateDisplayText txt dspTxt
                Nothing     -> mkDisplayText txt Font22 whiteColor
            return (upgradeType, displayTxt)

updatePauseMenuData
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsReadWrite MenuMsgsPhase m)
    => GameMode
    -> PlayerEquipmentInfo
    -> PauseMenuData
    -> m PauseMenuData
updatePauseMenuData prevGameMode equipmentInfo pauseMenuData = do
    upgradeCountOverlayDisplayTxts <-
        updateUpgradeCountOverlayDisplayText equipmentInfo (_upgradeCountOverlayDisplayTexts pauseMenuData)
    viewInfoInputDisplayTxt        <- updateInputDisplayText $ _viewInfoInputDisplayText pauseMenuData
    changeSlotInputDisplayTxt      <- updateInputDisplayText $ _changeSlotInputDisplayText pauseMenuData
    weaponHelpEntries              <- currentWeaponHelpEntries equipmentInfo pauseMenuData
    gunHelpEntries                 <- currentGunHelpEntries equipmentInfo pauseMenuData
    movementSkillHelpEntries       <- currentMovementSkillHelpEntries equipmentInfo pauseMenuData
    upgradeHelpEntries             <- currentUpgradeHelpEntries equipmentInfo pauseMenuData

    (secondarySkillNeutralHelpEntry, secondarySkillUpHelpEntry, secondarySkillDownHelpEntry) <-
        currentSecondarySkillHelpEntries equipmentInfo pauseMenuData

    let
        pauseMenuData' = pauseMenuData
            { _upgradeCountOverlayDisplayTexts = upgradeCountOverlayDisplayTxts
            , _viewInfoInputDisplayText        = viewInfoInputDisplayTxt
            , _changeSlotInputDisplayText      = changeSlotInputDisplayTxt
            , _weaponHelpEntries               = weaponHelpEntries
            , _gunHelpEntries                  = gunHelpEntries
            , _movementSkillHelpEntries        = movementSkillHelpEntries
            , _secondarySkillNeutralHelpEntry  = secondarySkillNeutralHelpEntry
            , _secondarySkillUpHelpEntry       = secondarySkillUpHelpEntry
            , _secondarySkillDownHelpEntry     = secondarySkillDownHelpEntry
            , _upgradeHelpEntries              = upgradeHelpEntries
            , _equipmentInfo                   = equipmentInfo
            }
        allInactive    = allOverlaysInactive pauseMenuData'

    let readInputState' = (if allInactive then id else inactivateInputState) <$> readInputState
    initialSelection   <- readSelection <$> readInputState' <*> pure prevGameMode <*> pure pauseMenuData'

    flip evalStateT initialSelection $
        let
            buttonStatus :: Monad m => PauseMenuSelection -> StateT PauseMenuSelection m ButtonStatus
            buttonStatus btnSelection = get <&> \selection -> if
                | not allInactive           -> ButtonInactiveStatus
                | btnSelection == selection -> ButtonSelectedActiveStatus
                | otherwise                 -> ButtonActiveStatus

            updateMenuHelpEntry'
                :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
                => PauseMenuSelection
                -> PauseMenuHelpEntry a
                -> StateT PauseMenuSelection m (PauseMenuHelpEntry a)
            updateMenuHelpEntry' btnSelection menuHelpEntry = do
                btnStatus      <- buttonStatus btnSelection
                menuHelpEntry' <- lift $ updatePauseMenuHelpEntry btnStatus menuHelpEntry

                when (isPauseMenuHelpEntryIconButtonSelectedOrActive menuHelpEntry') $
                    put btnSelection
                return menuHelpEntry'

            updateButton' :: InputRead m => PauseMenuSelection -> Button -> StateT PauseMenuSelection m Button
            updateButton' btnSelection btn = do
                btnStatus <- buttonStatus btnSelection
                btn'      <- lift $ updateButton btnStatus btn

                when (_isSelected (btn' :: Button) || _isPressed (btn' :: Button)) $
                    put btnSelection
                return btn'

            helpEntriesLeft  = pauseMenuHelpEntriesLeft
            helpEntriesRight = pauseMenuHelpEntriesRight
        in do
            weaponHelpEntries'        <- sequenceA $ catMaybes
                [ updateMenuHelpEntry' PauseMenuWeaponLeftSelection <$> helpEntriesLeft weaponHelpEntries
                , updateMenuHelpEntry' PauseMenuWeaponRightSelection <$> helpEntriesRight weaponHelpEntries
                ]
            gunHelpEntries'           <- sequenceA $ catMaybes
                [ updateMenuHelpEntry' PauseMenuGunLeftSelection <$> helpEntriesLeft gunHelpEntries
                , updateMenuHelpEntry' PauseMenuGunRightSelection <$> helpEntriesRight gunHelpEntries
                ]
            movementSkillHelpEntries' <- sequenceA $ catMaybes
                [ updateMenuHelpEntry' PauseMenuMovementSkillSelection <$> helpEntriesLeft movementSkillHelpEntries
                ]

            secondarySkillNeutralHelpEntry' <- case secondarySkillNeutralHelpEntry of
                Nothing        -> return Nothing
                Just helpEntry -> Just <$> updateMenuHelpEntry' PauseMenuSecondarySkillLeftSelection helpEntry
            secondarySkillUpHelpEntry'      <- case secondarySkillUpHelpEntry of
                Nothing        -> return Nothing
                Just helpEntry -> Just <$> updateMenuHelpEntry' PauseMenuSecondarySkillMidSelection helpEntry
            secondarySkillDownHelpEntry'    <- case secondarySkillDownHelpEntry of
                Nothing        -> return Nothing
                Just helpEntry -> Just <$> updateMenuHelpEntry' PauseMenuSecondarySkillRightSelection helpEntry

            upgradeHelpEntries' <- for (zip [0..] upgradeHelpEntries) $ \(i, helpEntry) ->
                updateMenuHelpEntry' (PauseMenuUpgradeSelection i) helpEntry

            generalHelpEntry   <- updateMenuHelpEntry' PauseMenuGeneralInfoSelection (_generalHelpEntry pauseMenuData')
            targetingHelpEntry <-
                updateMenuHelpEntry' PauseMenuTargetingInfoSelection (_targetingHelpEntry pauseMenuData')

            slotComboBoxes <- get >>= \selection ->
                let
                    isNeutralHelpEntry = isJust secondarySkillNeutralHelpEntry'
                    isUpHelpEntry      = isJust secondarySkillUpHelpEntry'
                    isDownHelpEntry    = isJust secondarySkillDownHelpEntry'
                    comboBoxes         = _slotComboBoxes pauseMenuData'
                in lift $ updateSlotComboBoxes selection isNeutralHelpEntry isUpHelpEntry isDownHelpEntry comboBoxes
            updateSelectionFromSlotComboBoxes slotComboBoxes

            resumeButton   <- updateButton' PauseMenuResumeSelection (_resumeButton pauseMenuData')
            mainMenuButton <- updateButton' PauseMenuMainSelection (_mainMenuButton pauseMenuData')
            settingsButton <- updateButton' PauseMenuSettingsSelection (_settingsButton pauseMenuData')

            let
                settingsMenuData   = _settingsMenuData pauseMenuData
                settingsMenuActive = _active (_settingsMenuData pauseMenuData :: SettingsMenuData)
            settingsMenuData'   <- if
                | _isPressed (settingsButton :: Button) ->
                    return $ (settingsMenuData :: SettingsMenuData) {_active = True}
                | settingsMenuActive                    -> lift $ updateSettingsMenuData settingsButton settingsMenuData
                | otherwise                             -> return settingsMenuData

            get <&> \selection -> pauseMenuData'
                { _resumeButton                   = resumeButton
                , _mainMenuButton                 = mainMenuButton
                , _settingsButton                 = settingsButton
                , _settingsMenuData               = settingsMenuData'
                , _generalHelpEntry               = generalHelpEntry
                , _targetingHelpEntry             = targetingHelpEntry
                , _weaponHelpEntries              = weaponHelpEntries'
                , _gunHelpEntries                 = gunHelpEntries'
                , _movementSkillHelpEntries       = movementSkillHelpEntries'
                , _secondarySkillNeutralHelpEntry = secondarySkillNeutralHelpEntry'
                , _secondarySkillUpHelpEntry      = secondarySkillUpHelpEntry'
                , _secondarySkillDownHelpEntry    = secondarySkillDownHelpEntry'
                , _upgradeHelpEntries             = upgradeHelpEntries'
                , _slotComboBoxes                 = slotComboBoxes
                , _selection                      = Just selection
                }

drawEmptyIconOverlays
    :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m)
    => PauseMenuData
    -> [Maybe (PauseMenuHelpEntry a)]
    -> (MenuConfig -> [Pos2])
    -> m ()
drawEmptyIconOverlays pauseMenuData helpEntries positionsF = do
    let helpEntries' = helpEntries ++ repeat Nothing
    positions       <- positionsF <$> readConfig _settings (_menu :: SettingsConfig -> MenuConfig)

    for_ (zip helpEntries' positions) $ \(helpEntry, pos) ->
        when (isNothing helpEntry) $
            drawImage pos RightDir menuZIndex (_emptyIconOverlayImage pauseMenuData)

drawSecondarySkillIconOverlays :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => PauseMenuData -> m ()
drawSecondarySkillIconOverlays pauseMenuData = do
    cfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)

    let
        upIconOverlayPos     = _pausedSecondarySkillUpHelpEntryPos cfg
        upIconOverlayOpacity = case _secondarySkillUpHelpEntry pauseMenuData of
            Nothing -> emptySecondarySkillUpDownIconOverlayOpacity
            Just _  -> FullOpacity
        upIconOverlayImg     = _secondarySkillUpIconOverlayImage pauseMenuData
    drawImageWithOpacity upIconOverlayPos RightDir menuZIndex upIconOverlayOpacity upIconOverlayImg

    let
        downIconOverlayPos     = _pausedSecondarySkillDownHelpEntryPos cfg
        downIconOverlayImg     = _secondarySkillDownIconOverlayImage pauseMenuData
        downIconOverlayOpacity = case _secondarySkillDownHelpEntry pauseMenuData of
            Nothing -> emptySecondarySkillUpDownIconOverlayOpacity
            Just _  -> FullOpacity
    drawImageWithOpacity downIconOverlayPos RightDir menuZIndex downIconOverlayOpacity downIconOverlayImg

drawUpgradeCountOverlays :: (GraphicsReadWrite m, MonadIO m) => PauseMenuData -> m ()
drawUpgradeCountOverlays pauseMenuData =
    for_ (_upgradeHelpEntries pauseMenuData) $ \helpEntry ->
        case HE._type helpEntry `M.lookup` _upgradeCountOverlayDisplayTexts pauseMenuData of
            Nothing         -> return ()
            Just displayTxt ->
                let pos = _pos (_iconButton helpEntry :: Button) `vecAdd` upgradeCountOverlayOffset
                in drawDisplayText pos menuZIndex displayTxt

drawSelectionInfoText :: (GraphicsReadWrite m, InputRead m, MonadIO m) => PauseMenuData -> m ()
drawSelectionInfoText pauseMenuData =
    let
        selection                 = _selection (pauseMenuData :: PauseMenuData)
        isEmptySelectionInfo      = maybe True (`S.member` emptySelectionInfoSelections) selection
        isSecondarySkillSelection = maybe False (`S.member` secondarySkillSelections) selection
        isSlotExpanded            = isSlotComboBoxesExpanded $ _slotComboBoxes pauseMenuData
        viewInfoIDT               = _viewInfoInputDisplayText pauseMenuData
        changeSlotIDT             = _changeSlotInputDisplayText pauseMenuData
    in if
        | isEmptySelectionInfo || isSlotExpanded -> return ()

        | isSecondarySkillSelection && not isSlotExpanded -> do
            viewInfoIDTWidth   <- inputDisplayTextWidth viewInfoIDT
            changeSlotIDTWidth <- inputDisplayTextWidth changeSlotIDT
            let
                totalWidth        = viewInfoIDTWidth + selectionInfoSpacerWidth + changeSlotIDTWidth
                viewInfoTextX     = selectionInfoCenterTextX - totalWidth / 2.0 + viewInfoIDTWidth / 2.0
                viewInfoTextPos   = Pos2 viewInfoTextX selectionInfoCenterTextY
                changeSlotTextX   = selectionInfoCenterTextX + totalWidth / 2.0 - changeSlotIDTWidth / 2.0
                changeSlotTextPos = Pos2 changeSlotTextX selectionInfoCenterTextY

            drawInputDisplayTextCentered viewInfoTextPos menuZIndex viewInfoIDT
            drawInputDisplayTextCentered changeSlotTextPos menuZIndex changeSlotIDT

        | otherwise -> drawInputDisplayTextCentered selectionInfoCenterTextPos menuZIndex viewInfoIDT

drawSlotComboBoxes :: (GraphicsReadWrite m, MonadIO m) => PauseMenuData -> m ()
drawSlotComboBoxes pauseMenuData =
    let
        slotComboBoxes      = _slotComboBoxes pauseMenuData
        neutralSlotComboBox = _neutralSlot slotComboBoxes
        upSlotComboBox      = _upSlot slotComboBoxes
        downSlotComboBox    = _downSlot slotComboBoxes
    in do
        drawComboBoxEx menuZIndex menuOverExpandedZIndex False neutralSlotComboBox
        drawComboBoxEx menuZIndex menuOverExpandedZIndex False upSlotComboBox
        drawComboBoxEx menuZIndex menuOverExpandedZIndex False downSlotComboBox

        for_ [neutralSlotComboBox, upSlotComboBox, downSlotComboBox] $ \comboBox ->
            let
                selectionText = _selectionText comboBox
                pos           = _pos (comboBox :: ComboBox) `vecAdd` secondarySkillInputOverlayImageOffset
            in do
                when (_isExpanded comboBox && selectionText == neutralSlotText) $
                    let img = _secondarySkillNeutralInputOverlayImage pauseMenuData
                    in drawImage pos RightDir menuOverExpandedZIndex img
                when (_isExpanded comboBox && selectionText == upSlotText) $
                    let img = _secondarySkillUpInputOverlayImage pauseMenuData
                    in drawImage pos RightDir menuOverExpandedZIndex img
                when (_isExpanded comboBox && selectionText == downSlotText) $
                    let img = _secondarySkillDownInputOverlayImage pauseMenuData
                    in drawImage pos RightDir menuOverExpandedZIndex img

drawPauseMenu :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => Game -> m ()
drawPauseMenu game =
    let
        pauseMenuData = _pauseMenuData $ _menu (game :: Game)
        backgroundImg = _backgroundImage (pauseMenuData :: PauseMenuData)
        world         = _world (game :: Game)
    in do
        cursorVisible <- (== MouseKbInputType) . _lastUsedInputType <$> readInputState
        showCursor cursorVisible
        setCameraSpace CameraScreenSpace

        drawImage zeroPos2 RightDir menuZIndex backgroundImg

        unlessM (readSettingsConfig _debug _disableMenuHelpEntries) $ do
            drawPauseMenuHelpEntry $ _generalHelpEntry pauseMenuData
            drawPauseMenuHelpEntry $ _targetingHelpEntry pauseMenuData

            when (_equipmentInfo pauseMenuData == worldPlayerEquipmentInfo world) $
                let
                    drawEmptyIconOverlays'
                        :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m)
                        => [Maybe (PauseMenuHelpEntry a)]
                        -> (MenuConfig -> [Pos2])
                        -> m ()
                    drawEmptyIconOverlays' = \helpEntries positionsF ->
                        drawEmptyIconOverlays pauseMenuData helpEntries positionsF

                    weaponHelpEntries              = _weaponHelpEntries pauseMenuData
                    gunHelpEntries                 = _gunHelpEntries pauseMenuData
                    movementSkillHelpEntries       = _movementSkillHelpEntries pauseMenuData
                    secondarySkillNeutralHelpEntry = _secondarySkillNeutralHelpEntry pauseMenuData
                    secondarySkillUpHelpEntry      = _secondarySkillUpHelpEntry pauseMenuData
                    secondarySkillDownHelpEntry    = _secondarySkillDownHelpEntry pauseMenuData
                    upgradeHelpEntries             = _upgradeHelpEntries pauseMenuData
                in do
                    sequenceA_ $ catMaybes
                        [ drawPauseMenuHelpEntry <$> pauseMenuHelpEntriesLeft weaponHelpEntries
                        , drawPauseMenuHelpEntry <$> pauseMenuHelpEntriesRight weaponHelpEntries
                        , drawPauseMenuHelpEntry <$> pauseMenuHelpEntriesLeft gunHelpEntries
                        , drawPauseMenuHelpEntry <$> pauseMenuHelpEntriesRight gunHelpEntries
                        , drawPauseMenuHelpEntry <$> pauseMenuHelpEntriesLeft movementSkillHelpEntries
                        , drawPauseMenuHelpEntry <$> secondarySkillNeutralHelpEntry
                        , drawPauseMenuHelpEntry <$> secondarySkillUpHelpEntry
                        , drawPauseMenuHelpEntry <$> secondarySkillDownHelpEntry
                        ]
                    traverse_ drawPauseMenuHelpEntry upgradeHelpEntries

                    drawEmptyIconOverlays' (map Just weaponHelpEntries) _pausedWeaponHelpEntryPositions
                    drawEmptyIconOverlays' (map Just gunHelpEntries) _pausedGunHelpEntryPositions
                    drawEmptyIconOverlays' (map Just movementSkillHelpEntries) _pausedMovementSkillHelpEntryPositions
                    drawEmptyIconOverlays' (map Just upgradeHelpEntries) _pausedUpgradeHelpEntryPositions
                    drawEmptyIconOverlays'
                        [secondarySkillNeutralHelpEntry, secondarySkillUpHelpEntry, secondarySkillDownHelpEntry]
                        (\cfg ->
                            [ _pausedSecondarySkillNeutralHelpEntryPos cfg
                            , _pausedSecondarySkillUpHelpEntryPos cfg
                            , _pausedSecondarySkillDownHelpEntryPos cfg
                            ]
                        )

                    drawSecondarySkillIconOverlays pauseMenuData
                    drawUpgradeCountOverlays pauseMenuData
                    drawSlotComboBoxes pauseMenuData

        drawSelectionInfoText pauseMenuData
        drawSettingsMenuData $ _settingsMenuData pauseMenuData

        when (isWorldPlayerTouchingInfoSign world) $ do
            drawImage infoOverlayLeftImagePos RightDir menuZIndex (_infoOverlayLeftImage pauseMenuData)
            drawImage infoOverlayRightImagePos RightDir menuZIndex (_infoOverlayRightImage pauseMenuData)

        let isSlotExpanded = isSlotComboBoxesExpanded $ _slotComboBoxes pauseMenuData
        when (allOverlaysInactive pauseMenuData || isSlotExpanded) $ do
            drawButton menuZIndex (_mainMenuButton pauseMenuData)
            drawButton menuZIndex (_resumeButton pauseMenuData)
            drawButton menuZIndex (_settingsButton pauseMenuData)

secondarySkillChangedOrder
    :: PauseMenuData
    -> (Maybe SecondarySkillType, Maybe SecondarySkillType, Maybe SecondarySkillType)
secondarySkillChangedOrder pauseMenuData =
    ( HE._type <$> neutralF pauseMenuData
    , HE._type <$> upF pauseMenuData
    , HE._type <$> downF pauseMenuData
    )
    where
        slotComboBoxes = _slotComboBoxes pauseMenuData
        neutralSlot    = _neutralSlot slotComboBoxes
        upSlot         = _upSlot slotComboBoxes
        downSlot       = _downSlot slotComboBoxes

        (neutralF, upF, downF)
            | comboBoxValue neutralSlot == upSlotText   =
                (_secondarySkillUpHelpEntry, _secondarySkillNeutralHelpEntry, _secondarySkillDownHelpEntry)
            | comboBoxValue neutralSlot == downSlotText =
                (_secondarySkillDownHelpEntry, _secondarySkillUpHelpEntry, _secondarySkillNeutralHelpEntry)
            | comboBoxValue upSlot == neutralSlotText   =
                (_secondarySkillUpHelpEntry, _secondarySkillNeutralHelpEntry, _secondarySkillDownHelpEntry)
            | comboBoxValue upSlot == downSlotText      =
                (_secondarySkillNeutralHelpEntry, _secondarySkillDownHelpEntry, _secondarySkillUpHelpEntry)
            | comboBoxValue downSlot == neutralSlotText =
                (_secondarySkillDownHelpEntry, _secondarySkillUpHelpEntry, _secondarySkillNeutralHelpEntry)
            | comboBoxValue downSlot == upSlotText      =
                (_secondarySkillNeutralHelpEntry, _secondarySkillDownHelpEntry, _secondarySkillUpHelpEntry)
            | otherwise                                 =
                (_secondarySkillNeutralHelpEntry, _secondarySkillUpHelpEntry, _secondarySkillDownHelpEntry)

pauseMenuMain :: Game -> AppEnv BaseMsgsPhase Game
pauseMenuMain game =
    let
        menu                       = _menu (game :: Game)
        world                      = _world (game :: Game)
        equipmentInfo              = worldPlayerEquipmentInfo world
        prevPauseMenuData          = _pauseMenuData menu
        prevGameMode               = _prevMode game
        prevAllHelpEntriesInactive = allOverlaysInactive prevPauseMenuData
    in do
        inputState    <- readInputState
        pauseMenuData <- withMsgsPhase @MenuMsgsPhase $
            updatePauseMenuData prevGameMode equipmentInfo prevPauseMenuData

        let
            menuOrBackPressed  = MenuAlias `aliasPressed` inputState || MenuBackAlias `aliasPressed` inputState
            resumeBtnPressed   = _isPressed (_resumeButton pauseMenuData :: Button)
            mainMenuBtnPressed = _isPressed (_mainMenuButton pauseMenuData :: Button)
            settingsBtnPressed = _isPressed (_settingsButton pauseMenuData :: Button)

            gameMode
                | resumeBtnPressed                                = WorldMode
                | menuOrBackPressed && prevAllHelpEntriesInactive = WorldMode
                | mainMenuBtnPressed                              = MainMenuMode
                | otherwise                                       = PauseMenuMode

        when (gameMode == WorldMode) $ do
            void $ pauseFmodAudioWorld False
            setGraphicsCursor _crosshair

        let
            prevGameMode'
                | gameMode /= PauseMenuMode = PauseMenuMode
                | otherwise                 = prevGameMode

            pauseMenuData'
                | gameMode /= PauseMenuMode = pauseMenuData {_selection = Nothing} :: PauseMenuData
                | otherwise                 = pauseMenuData

            (neutralSlotType, upSlotType, downSlotType) = secondarySkillChangedOrder pauseMenuData'

            world' = setWorldPlayerSecondarySkillManagerOrder neutralSlotType upSlotType downSlotType world
            menu'  = menu {_pauseMenuData = pauseMenuData'}

        let prevSettingsMenuData = _settingsMenuData . _pauseMenuData $ _menu (game :: Game)
        quitHotkeyPressed       <- if
            | isSettingsMenuControlsTabWaitingForInput prevSettingsMenuData -> return False
            | otherwise                                                     -> isMenuQuitHotkeyPressed

        when (resumeBtnPressed || mainMenuBtnPressed || settingsBtnPressed) $
            void . playFmodSound . _confirm $ _soundIndices (pauseMenuData' :: PauseMenuData)

        let
            isPrevComboBoxesExpanded = isSlotComboBoxesExpanded $ _slotComboBoxes prevPauseMenuData
            isDifferentSelection     =
                _selection (prevPauseMenuData :: PauseMenuData) /= _selection (pauseMenuData' :: PauseMenuData)
            slotComboBoxes           = _slotComboBoxes pauseMenuData'
            isComboBoxesExpanded     = isSlotComboBoxesExpanded slotComboBoxes
            isComboBoxesJustExpanded = (not isPrevComboBoxesExpanded || isDifferentSelection) && isComboBoxesExpanded
        when (isComboBoxesJustExpanded || isSlotComboBoxesPressed slotComboBoxes) $
            void . playFmodSound . _skillSlotSelect $ _soundIndices (pauseMenuData' :: PauseMenuData)

        void $ playOrResumeFmodMusicMenu (_musicIndex (pauseMenuData' :: PauseMenuData))

        return $ game
            { _mode     = gameMode
            , _prevMode = prevGameMode'
            , _world    = world'
            , _menu     = menu'
            , _quit     = quitHotkeyPressed || _quit game
            }
