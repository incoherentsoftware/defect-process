module Menu.UnlocksMenu
    ( mkUnlocksMenuData
    , unlocksMenuMain
    , drawUnlocksMenu
    , isUnlocksMenuAllAvailableUnlocked
    ) where

import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (StateT, evalStateT, get, lift, put)
import Data.Foldable          (foldlM, for_, traverse_)
import Data.Functor           ((<&>))
import System.FilePath        (dropExtension)
import qualified Data.Set as S
import qualified Data.Text as T

import AppEnv
import Audio.Fmod
import Configs
import Configs.All.Progress
import Configs.All.Settings
import Configs.All.Settings.Menu
import Constants
import FileCache
import Game.Types
import Menu.HelpPopup
import Menu.HelpPopup.Util
import Menu.SoundIndices
import Menu.Types
import Menu.UnlocksMenu.Types
import Menu.Util
import Menu.ZIndex
import Msg
import Player.SecondarySkill.Types
import Util
import Window.Graphics
import Window.Graphics.UiControls.Button
import Window.InputState
import World.Util

unlocksMenuPack           = \p -> PackResourceFilePath "data/menu/unlocks-menu.pack" p
selectedOverlayImagePath  = unlocksMenuPack "item-selected-overlay.image"     :: PackResourceFilePath
mainMenuButtonImagePath   = unlocksMenuPack "main-menu-button.image"          :: PackResourceFilePath
bgImagePath               = unlocksMenuPack "unlocks-menu-background.image"   :: PackResourceFilePath
insufficientGoldImagePath = unlocksMenuPack "insufficient-gold-overlay.image" :: PackResourceFilePath
unlockOverlaySpritePath   = unlocksMenuPack "unlock-overlay.spr"              :: PackResourceFilePath

lockedImageNameSuffix          = "-locked.image"                  :: FileName
insufficientGoldFadeMultiplier = 2.0                              :: Float
maxDisplayedTotalGoldValue     = GoldValue 999999                 :: GoldValue
unlocksText                    = "Unlocks"                        :: T.Text
unavailableText                = "Unavailable"                    :: T.Text
viewInfoText                   = "View Info: {MenuSelectAlias.0}" :: T.Text
unlocksCreditsColor            = Color 158 207 206 255            :: Color

mkLockedCostSymbolDisplayText :: (FileCache m, GraphicsRead m, MonadIO m) => GoldValue -> m SymbolDisplayText
mkLockedCostSymbolDisplayText goldValue =
    mkSymbolDisplayText ("{UnlocksCreditsSymbol} " <> prettyShow goldValue) Font32 unlocksCreditsColor

mkLockedUnlockInputDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, PrettyShow a)
    => a
    -> m InputDisplayText
mkLockedUnlockInputDisplayText entryType =
    mkInputDisplayText ("Unlock " <> prettyShow entryType <> ": {MenuSelectAlias.0}") Font26 whiteColor

mkUnlockedTitleDisplayText :: (GraphicsRead m, MonadIO m, PrettyShow a) => a -> m DisplayText
mkUnlockedTitleDisplayText entryType = mkDisplayText (prettyShow entryType) Font32 whiteColor

formatTotalGoldText :: GoldValue -> T.Text
formatTotalGoldText goldValue = "{UnlocksCreditsLargeSymbol} " <> prettyShow goldValue'
    where goldValue' = min goldValue maxDisplayedTotalGoldValue

updateUnlocksEntryStatus
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => UnlocksEntryStatus
    -> m UnlocksEntryStatus
updateUnlocksEntryStatus = \case
    LockedStatus costSymbolDisplayTxt unlockInputDisplayTxt ->
        LockedStatus costSymbolDisplayTxt <$> updateInputDisplayText unlockInputDisplayTxt
    entryStatus                                             -> return entryStatus

mkUnlocksEntry
    :: forall m a. (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, PrettyShow a)
    => (GoldValue -> a -> ConsoleMsgPayload)
    -> (a -> Maybe HelpPopupDescription)
    -> UnlocksEntryJSON a
    -> m (UnlocksEntry a)
mkUnlocksEntry msgPayloadF helpPopupF unlocksEntryJSON =
    let
        btnPos               = _pos (unlocksEntryJSON :: UnlocksEntryJSON a)
        btnImgFileName       = _imageFileName unlocksEntryJSON
        btnImgFilePath       = unlocksMenuPack btnImgFileName
        cost                 = _cost (unlocksEntryJSON :: UnlocksEntryJSON a)
        lockedOverlayImgPath = unlocksMenuPack $ dropExtension btnImgFileName ++ lockedImageNameSuffix
        entryType            = _type (unlocksEntryJSON :: UnlocksEntryJSON a)

        (idx, upIdx, downIdx, leftIdx, rightIdx) = _indices unlocksEntryJSON
    in do
        status <- case entryType of
            Nothing  -> pure UnavailableStatus
            Just typ -> LockedStatus <$> mkLockedCostSymbolDisplayText cost <*> mkLockedUnlockInputDisplayText typ

        btn              <- mkImageButtonEx btnPos btnImgFilePath btnImgFilePath  -- selected image is manually drawn
        lockedOverlayImg <- loadPackImage lockedOverlayImgPath

        helpPopup <- case helpPopupF =<< entryType of
            Nothing            -> return Nothing
            Just helpPopupDesc -> Just <$> mkHelpPopup helpPopupDesc

        return $ UnlocksEntry
            { _type               = entryType
            , _status             = status
            , _cost               = cost
            , _index              = idx
            , _upIndex            = upIdx
            , _downIndex          = downIdx
            , _leftIndex          = leftIdx
            , _rightIndex         = rightIdx
            , _button             = btn
            , _lockedOverlayImage = lockedOverlayImg
            , _msgPayload         = msgPayloadF cost <$> entryType
            , _helpPopup          = helpPopup
            , _helpPopupActive    = False
            }

drawUnlocksEntry
    :: forall m a. (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m)
    => UnlocksMenuData
    -> UnlocksEntry a
    -> m ()
drawUnlocksEntry unlocksMenuData entry =
    let
        btn            = _button entry
        status         = _status (entry :: UnlocksEntry a)
        pos            = _pos (btn :: Button)
        isLockedStatus = case status of
            LockedStatus _ _ -> True
            _                -> False

        viewInfoInputDisplayTxt = _viewInfoInputDisplayText unlocksMenuData
    in do
        drawButton menuZIndex btn

        when isLockedStatus $
            drawImage pos RightDir menuZIndex (_lockedOverlayImage entry)

        when (_isSelected btn) $ do
            drawImage pos RightDir menuZIndex (_selectedOverlayImage unlocksMenuData)

            menuCfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
            let
                titlePos    = _unlocksTitleTextPos menuCfg
                subTitlePos = _unlocksSubTitleTextPos menuCfg

            case status of
                UnlockedStatus titleDisplayTxt -> do
                    drawDisplayTextCentered titlePos menuZIndex titleDisplayTxt
                    drawInputDisplayTextCentered subTitlePos menuZIndex viewInfoInputDisplayTxt

                LockedStatus costSymbolDisplayTxt unlockInputDisplayTxt -> do
                        drawSymbolDisplayTextCentered titlePos menuZIndex costSymbolDisplayTxt
                        drawInputDisplayTextCentered subTitlePos menuZIndex unlockInputDisplayTxt

                UnavailableStatus ->
                    let unavailablePos = _unlocksUnavailableTextPos menuCfg
                    in drawDisplayTextCentered unavailablePos menuZIndex (_unavailableDisplayText unlocksMenuData)

        case _helpPopup entry of
            Just popup
                | _helpPopupActive entry -> drawHelpPopup popup
            _                            -> return ()

loadUnlocksEntries
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, PrettyShow a)
    => (GoldValue -> a -> ConsoleMsgPayload)
    -> (MenuConfig -> [UnlocksEntryJSON a])
    -> (a -> Maybe HelpPopupDescription)
    -> m [UnlocksEntry a]
loadUnlocksEntries msgPayloadF unlocksEntriesF helpPopupF = do
    entries <- readSettingsConfig (_menu :: SettingsConfig -> MenuConfig) unlocksEntriesF
    traverse (mkUnlocksEntry msgPayloadF helpPopupF) entries

mkUnlocksMenuData :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m UnlocksMenuData
mkUnlocksMenuData = do
    backgroundImg       <- loadPackImage bgImagePath
    totalGoldTxt        <- formatTotalGoldText <$> readConfig _progress _totalGold
    totalGoldDisplayTxt <- mkSymbolDisplayText totalGoldTxt Font44 unlocksCreditsColor

    menuCfg               <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    musicEntries          <- loadUnlocksEntries
        ConsoleMsgUnlockMusic
        _unlocksMusicEntries
        (const $ Just musicHelpPopupDescription)
    weaponEntries         <- loadUnlocksEntries
        ConsoleMsgUnlockWeapon
        _unlocksWeaponEntries
        (Just . weaponTypeToHelpPopupDescription menuCfg)
    gunEntries            <- loadUnlocksEntries
        ConsoleMsgUnlockGun
        _unlocksGunEntries
        (Just . gunTypeToHelpPopupDescription)
    movementSkillEntries  <- loadUnlocksEntries
        ConsoleMsgUnlockMovementSkill
        _unlocksMovementSkillEntries
        (Just . movementSkillTypeToHelpPopupDescription)
    secondarySkillEntries <- loadUnlocksEntries
        ConsoleMsgUnlockSecondarySkill
        _unlocksSecondarySkillEntries
        (\t -> Just $ secondarySkillTypeToHelpPopupDescription t SecondarySkillNeutralSlot)

    selectedOverlayImg      <- loadPackImage selectedOverlayImagePath
    unlocksDisplayTxt       <- mkDisplayText unlocksText Font32 whiteColor
    unavailableDisplayTxt   <- mkDisplayText unavailableText Font32 whiteColor
    viewInfoInputDisplayTxt <- mkInputDisplayText viewInfoText Font26 whiteColor
    mainMenuBtn             <- mkImageButtonCentered (_unlocksMainMenuButtonPos menuCfg) mainMenuButtonImagePath
    insufficientGoldImg     <- loadPackImage insufficientGoldImagePath
    musicIndex              <- getFmodMusic menuMusicPath
    soundIndices            <- mkMenuSoundIndices

    return $ UnlocksMenuData
        { _backgroundImage          = backgroundImg
        , _totalGoldDisplayText     = totalGoldDisplayTxt
        , _musicEntries             = musicEntries
        , _weaponEntries            = weaponEntries
        , _gunEntries               = gunEntries
        , _movementSkillEntries     = movementSkillEntries
        , _secondarySkillEntries    = secondarySkillEntries
        , _selectedOverlayImage     = selectedOverlayImg
        , _unlocksDisplayText       = unlocksDisplayTxt
        , _unavailableDisplayText   = unavailableDisplayTxt
        , _viewInfoInputDisplayText = viewInfoInputDisplayTxt
        , _mainMenuButton           = mainMenuBtn
        , _insufficientGoldImage    = insufficientGoldImg
        , _insufficientGoldOpacity  = Opacity 0.0
        , _unlocksOverlays          = []
        , _selection                = UnlocksMenuMainMenuSelection
        , _musicIndex               = musicIndex
        , _soundIndices             = soundIndices
        }

isAnyUnlocksEntryHelpPopupActive :: UnlocksMenuData -> Bool
isAnyUnlocksEntryHelpPopupActive unlocksMenuData =
    or [_helpPopupActive entry | entry <- _musicEntries unlocksMenuData] ||
    or [_helpPopupActive entry | entry <- _weaponEntries unlocksMenuData] ||
    or [_helpPopupActive entry | entry <- _gunEntries unlocksMenuData] ||
    or [_helpPopupActive entry | entry <- _movementSkillEntries unlocksMenuData] ||
    or [_helpPopupActive entry | entry <- _secondarySkillEntries unlocksMenuData]

readSelection :: Bool -> InputState -> MenuConfig -> Game -> UnlocksMenuSelection
readSelection isAnyHelpPopupActive inputState cfg game = case _selection (unlocksMenuData :: UnlocksMenuData) of
    selection
        | isAnyHelpPopupActive -> selection

    UnlocksMenuSubSelection idx ->
        let
            filterByIndex :: forall a. [UnlocksEntry a] -> [UnlocksEntry a]
            filterByIndex entries = filter (\entry -> _index (entry :: UnlocksEntry a) == idx) entries
        in if
            | upPressed ->
                let
                    idx'
                        | [entry] <- filterByIndex (_musicEntries unlocksMenuData)          = _upIndex entry
                        | [entry] <- filterByIndex (_weaponEntries unlocksMenuData)         = _upIndex entry
                        | [entry] <- filterByIndex (_gunEntries unlocksMenuData)            = _upIndex entry
                        | [entry] <- filterByIndex (_movementSkillEntries unlocksMenuData)  = _upIndex entry
                        | [entry] <- filterByIndex (_secondarySkillEntries unlocksMenuData) = _upIndex entry
                        | otherwise                                                         = idx
                in if idx' < 0 then UnlocksMenuMainMenuSelection else UnlocksMenuSubSelection idx'

            | downPressed ->
                let
                    idx'
                        | [entry] <- filterByIndex (_musicEntries unlocksMenuData)          = _downIndex entry
                        | [entry] <- filterByIndex (_weaponEntries unlocksMenuData)         = _downIndex entry
                        | [entry] <- filterByIndex (_gunEntries unlocksMenuData)            = _downIndex entry
                        | [entry] <- filterByIndex (_movementSkillEntries unlocksMenuData)  = _downIndex entry
                        | [entry] <- filterByIndex (_secondarySkillEntries unlocksMenuData) = _downIndex entry
                        | otherwise                                                         = idx
                in if idx' < 0 then UnlocksMenuMainMenuSelection else UnlocksMenuSubSelection idx'

            | leftPressed -> UnlocksMenuSubSelection $ if
                | [entry] <- filterByIndex (_musicEntries unlocksMenuData)          -> _leftIndex entry
                | [entry] <- filterByIndex (_weaponEntries unlocksMenuData)         -> _leftIndex entry
                | [entry] <- filterByIndex (_gunEntries unlocksMenuData)            -> _leftIndex entry
                | [entry] <- filterByIndex (_movementSkillEntries unlocksMenuData)  -> _leftIndex entry
                | [entry] <- filterByIndex (_secondarySkillEntries unlocksMenuData) -> _leftIndex entry
                | otherwise                                                         -> idx

            | rightPressed -> UnlocksMenuSubSelection $ if
                | [entry] <- filterByIndex (_musicEntries unlocksMenuData)          -> _rightIndex entry
                | [entry] <- filterByIndex (_weaponEntries unlocksMenuData)         -> _rightIndex entry
                | [entry] <- filterByIndex (_gunEntries unlocksMenuData)            -> _rightIndex entry
                | [entry] <- filterByIndex (_movementSkillEntries unlocksMenuData)  -> _rightIndex entry
                | [entry] <- filterByIndex (_secondarySkillEntries unlocksMenuData) -> _rightIndex entry
                | otherwise                                                         -> idx

            | otherwise -> UnlocksMenuSubSelection idx

    UnlocksMenuMainMenuSelection
        | upPressed   -> UnlocksMenuSubSelection $ _unlocksCenterBotEntryIndex cfg
        | downPressed -> UnlocksMenuSubSelection $ _unlocksCenterTopEntryIndex cfg
        | otherwise   -> UnlocksMenuMainMenuSelection

    where
        unlocksMenuData = _unlocksMenuData $ _menu (game :: Game)
        upPressed       = MenuUpAlias `aliasPressed` inputState
        downPressed     = MenuDownAlias `aliasPressed` inputState
        leftPressed     = MenuLeftAlias `aliasPressed` inputState
        rightPressed    = MenuRightAlias `aliasPressed` inputState

writeUnlockMsgs :: MsgsWrite MenuMsgsPhase m => UnlocksEntry a -> m ()
writeUnlockMsgs entry = case _msgPayload entry of
    Nothing         -> return ()
    Just msgPayload ->
        let btnPos = _pos (_button entry :: Button)
        in writeMsgs
            [ mkMsg msgPayload
            , mkMsg ConsoleMsgSaveProgress
            , mkMsg $ MenuMsgShowUnlockOverlay btnPos
            ]

updateUnlocksOverlays :: [UnlocksOverlay] -> [UnlocksOverlay]
updateUnlocksOverlays unlocksOverlays =
    [ unlocksOverlay {_sprite = updateSprite spr}
    | unlocksOverlay <- unlocksOverlays
    , let spr = _sprite unlocksOverlay
    , not $ spriteFinished spr
    ]

updateUnlocksMenuData
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsWrite MenuMsgsPhase m)
    => Game
    -> m UnlocksMenuData
updateUnlocksMenuData game = do
    let
        unlocksMenuData      = _unlocksMenuData $ _menu (game :: Game)
        isAnyHelpPopupActive = isAnyUnlocksEntryHelpPopupActive unlocksMenuData

    inputState          <- readInputState
    menuCfg             <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    let initialSelection = readSelection isAnyHelpPopupActive inputState menuCfg game
    progressCfg         <- _progress <$> readConfigs

    let
        insufficientGoldOpacity =
            decreaseOpacity (insufficientGoldFadeMultiplier * timeStep) (_insufficientGoldOpacity unlocksMenuData)

        totalGold    = _totalGold progressCfg
        totalGoldTxt = formatTotalGoldText totalGold
    totalGoldDisplayTxt <- updateSymbolDisplayText totalGoldTxt (_totalGoldDisplayText unlocksMenuData)

    flip evalStateT initialSelection $
        let
            updateButton' :: InputRead m1 => UnlocksMenuSelection -> Button -> StateT UnlocksMenuSelection m1 Button
            updateButton' btnSelection btn = do
                btnStatus <- get <&> \selection -> if
                    | isAnyHelpPopupActive      -> ButtonInactiveStatus
                    | btnSelection == selection -> ButtonSelectedActiveStatus
                    | otherwise                 -> ButtonActiveStatus
                btn'      <- lift $ updateButton btnStatus btn

                when (_isSelected btn' || _isPressed btn') $
                    put btnSelection
                return btn'

            updateUnlocksEntry
                :: forall m1 a.
                    ( FileCache m1
                    , InputRead m1
                    , GraphicsRead m1
                    , MonadIO m1
                    , MsgsWrite MenuMsgsPhase m1
                    , Ord a
                    , PrettyShow a
                    )
                => (ProgressConfig -> S.Set a)
                -> UnlocksEntry a
                -> StateT UnlocksMenuSelection m1 (UnlocksEntry a)
            updateUnlocksEntry unlockedF entry = do
                let btnSelection = UnlocksMenuSubSelection $ _index (entry :: UnlocksEntry a)
                btn <- updateButton' btnSelection (_button entry)

                (helpPopup, helpPopupActive) <- case _helpPopup entry of
                    Just popup
                        | _helpPopupActive entry -> do
                            popup' <- lift $ updateHelpPopup popup
                            return (Just popup', not $ isHelpPopupCloseButtonPressed popup')
                    hp                           -> return (hp, False)

                let
                    playSound = \soundIndexF ->
                        let soundIndices = _soundIndices (unlocksMenuData :: UnlocksMenuData)
                        in void $ playFmodSound (soundIndexF soundIndices)

                entryStatus      <- lift $ updateUnlocksEntryStatus (_status (entry :: UnlocksEntry a))
                helpPopupActive' <- case (_isPressed btn, entryStatus) of
                    (True, LockedStatus _ _) -> do
                        if
                            | _cost (entry :: UnlocksEntry a) <= totalGold -> do
                                lift $ writeUnlockMsgs entry
                                playSound _unlock
                            | otherwise                                    -> do
                                lift $ writeMsgs [mkMsg MenuMsgUnlocksInsufficientGold]
                                playSound _cantUnlock
                        return False

                    (True, UnlockedStatus _) -> case helpPopup of
                        Nothing -> playSound _cantUnlock *> pure False
                        Just _  -> playSound _confirm *> pure True

                    (True, UnavailableStatus) -> playSound _cantUnlock *> pure False

                    _ ->
                        let isMenuOrBackPressed = or $ map (`aliasPressed` inputState) [MenuAlias, MenuBackAlias]
                        in return $ if
                            | helpPopupActive && isMenuOrBackPressed -> False
                            | otherwise                              -> helpPopupActive

                entryStatus' <- case _type (entry :: UnlocksEntry a) of
                    Just entryType
                        | entryType `S.member` unlockedF progressCfg -> case entryStatus of
                            UnlockedStatus _ -> return entryStatus
                            _                -> lift $ UnlockedStatus <$> mkUnlockedTitleDisplayText entryType
                    _                                                -> return entryStatus

                return $ entry
                    { _status          = entryStatus'
                    , _button          = btn
                    , _helpPopup       = helpPopup
                    , _helpPopupActive = helpPopupActive'
                    }
        in do
            musicEntries          <- traverse (updateUnlocksEntry _unlockedMusic) (_musicEntries unlocksMenuData)
            weaponEntries         <- traverse (updateUnlocksEntry _unlockedWeapons) (_weaponEntries unlocksMenuData)
            gunEntries            <- traverse (updateUnlocksEntry _unlockedGuns) (_gunEntries unlocksMenuData)
            movementSkillEntries  <-
                traverse (updateUnlocksEntry _unlockedMovementSkills) (_movementSkillEntries unlocksMenuData)
            secondarySkillEntries <-
                traverse (updateUnlocksEntry _unlockedSecondarySkills) (_secondarySkillEntries unlocksMenuData)

            viewInfoInputDisplayTxt <- lift $ updateInputDisplayText (_viewInfoInputDisplayText unlocksMenuData)
            mainMenuBtn             <-
                updateButton' UnlocksMenuMainMenuSelection (_mainMenuButton (unlocksMenuData :: UnlocksMenuData))

            get <&> \selection -> unlocksMenuData
                { _totalGoldDisplayText     = totalGoldDisplayTxt
                , _musicEntries             = musicEntries
                , _weaponEntries            = weaponEntries
                , _gunEntries               = gunEntries
                , _movementSkillEntries     = movementSkillEntries
                , _secondarySkillEntries    = secondarySkillEntries
                , _viewInfoInputDisplayText = viewInfoInputDisplayTxt
                , _mainMenuButton           = mainMenuBtn
                , _insufficientGoldOpacity  = insufficientGoldOpacity
                , _unlocksOverlays          = updateUnlocksOverlays $ _unlocksOverlays unlocksMenuData
                , _selection                = selection
                }

processUnlocksMenuMessages
    :: forall m. (FileCache m, GraphicsRead m, MonadIO m, MsgsRead MenuMsgsPhase m)
    => UnlocksMenuData
    -> m UnlocksMenuData
processUnlocksMenuMessages unlocksMenuData = foldlM processMsg unlocksMenuData =<< readMsgs
    where
        processMsg :: UnlocksMenuData -> MenuMsgPayload -> m UnlocksMenuData
        processMsg !umd d = case d of
            MenuMsgUnlocksInsufficientGold -> return $ umd {_insufficientGoldOpacity = Opacity 1.0}
            MenuMsgShowUnlockOverlay pos   -> do
                unlocksOverlay <- UnlocksOverlay pos <$> loadPackSprite unlockOverlaySpritePath
                return $ umd {_unlocksOverlays = unlocksOverlay:_unlocksOverlays umd}
            _                              -> return umd

unlocksMenuMain :: Game -> AppEnv BaseMsgsPhase Game
unlocksMenuMain game = do
    let isPrevAnyHelpPopupActive = isAnyUnlocksEntryHelpPopupActive $ _unlocksMenuData (_menu (game :: Game))

    unlocksMenuData <- withMsgsPhase @MenuMsgsPhase $
        updateUnlocksMenuData game >>= processUnlocksMenuMessages
    inputState      <- readInputState

    let
        mainMenuBtnPressed = _isPressed $ _mainMenuButton (unlocksMenuData :: UnlocksMenuData)
        menuOrBackPressed  = MenuAlias `aliasPressed` inputState || MenuBackAlias `aliasPressed` inputState

        gameMode
            | mainMenuBtnPressed || (menuOrBackPressed && not isPrevAnyHelpPopupActive) = MainMenuMode
            | otherwise                                                                 = UnlocksMenuMode

        prevGameMode
            | gameMode /= UnlocksMenuMode = UnlocksMenuMode
            | otherwise                   = _prevMode game

    when mainMenuBtnPressed $
        void . playFmodSound . _confirm $ _soundIndices (unlocksMenuData :: UnlocksMenuData)
    void $ playOrResumeFmodMusicMenu (_musicIndex (unlocksMenuData :: UnlocksMenuData))

    quitHotkeyPressed <- isMenuQuitHotkeyPressed

    return $ game
        { _mode     = gameMode
        , _prevMode = prevGameMode
        , _menu     = (_menu (game :: Game)) {_unlocksMenuData = unlocksMenuData}
        , _quit     = quitHotkeyPressed || _quit game
        }

drawUnlocksMenu :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => Game -> m ()
drawUnlocksMenu game =
    let
        unlocksMenuData     = _unlocksMenuData $ _menu (game :: Game)
        backgroundImg       = _backgroundImage unlocksMenuData
        unlocksDisplayTxt   = _unlocksDisplayText unlocksMenuData
        totalGoldDisplayTxt = _totalGoldDisplayText unlocksMenuData
        mainMenuBtn         = _mainMenuButton (unlocksMenuData :: UnlocksMenuData)
    in do
        cursorVisible <- (== MouseKbInputType) . _lastUsedInputType <$> readInputState
        showCursor cursorVisible
        setCameraSpace CameraScreenSpace

        menuCfg                    <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
        let unlocksTotalGoldTextPos = _unlocksTotalGoldTextPos menuCfg

        drawImage zeroPos2 RightDir menuZIndex backgroundImg
        drawDisplayTextCentered (_unlocksUnlocksTextPos menuCfg) menuZIndex unlocksDisplayTxt
        drawSymbolDisplayTextRightAligned unlocksTotalGoldTextPos menuZIndex totalGoldDisplayTxt
        drawButton menuZIndex mainMenuBtn

        traverse_ (drawUnlocksEntry unlocksMenuData) (_musicEntries unlocksMenuData)
        traverse_ (drawUnlocksEntry unlocksMenuData) (_weaponEntries unlocksMenuData)
        traverse_ (drawUnlocksEntry unlocksMenuData) (_gunEntries unlocksMenuData)
        traverse_ (drawUnlocksEntry unlocksMenuData) (_movementSkillEntries unlocksMenuData)
        traverse_ (drawUnlocksEntry unlocksMenuData) (_secondarySkillEntries unlocksMenuData)

        for_ (_unlocksOverlays unlocksMenuData) $ \unlocksOverlay ->
            let pos = _pos (unlocksOverlay :: UnlocksOverlay)
            in drawSprite pos RightDir menuZIndex (_sprite unlocksOverlay)

        totalGoldWidth  <- symbolDisplayTextWidth totalGoldDisplayTxt
        totalGoldHeight <- symbolDisplayTextHeight totalGoldDisplayTxt
        let
            insufficientGoldOffsetX = -(totalGoldWidth - symbolDisplayTextImageWidth totalGoldDisplayTxt) / 2.0
            insufficientGoldOffset  = Pos2 insufficientGoldOffsetX (totalGoldHeight / 2.0)
            insufficientGoldPos     = unlocksTotalGoldTextPos `vecAdd` insufficientGoldOffset
            insufficientGoldOpacity = _insufficientGoldOpacity unlocksMenuData
            insufficientGoldImg     = _insufficientGoldImage unlocksMenuData
        drawImageWithOpacity insufficientGoldPos RightDir menuZIndex insufficientGoldOpacity insufficientGoldImg

isUnlocksMenuAllAvailableUnlocked :: ConfigsRead m => Game -> m Bool
isUnlocksMenuAllAvailableUnlocked game = isAllAvailableUnlocked . _progress <$> readConfigs
    where
        isAllAvailableUnlocked :: ProgressConfig -> Bool
        isAllAvailableUnlocked progressCfg =
            let
                isAvailableUnlocked :: forall a. Ord a => (ProgressConfig -> S.Set a) -> UnlocksEntry a -> Bool
                isAvailableUnlocked unlockedF entry = case _type (entry :: UnlocksEntry a) of
                    Just entryType -> entryType `S.member` unlockedF progressCfg
                    Nothing        -> True

                unlocksMenuData = _unlocksMenuData $ _menu (game :: Game)
            in
                and (map (isAvailableUnlocked _unlockedMusic) (_musicEntries unlocksMenuData)) &&
                and (map (isAvailableUnlocked _unlockedWeapons) (_weaponEntries unlocksMenuData)) &&
                and (map (isAvailableUnlocked _unlockedGuns) (_gunEntries unlocksMenuData)) &&
                and (map (isAvailableUnlocked _unlockedMovementSkills) (_movementSkillEntries unlocksMenuData)) &&
                and (map (isAvailableUnlocked _unlockedSecondarySkills) (_secondarySkillEntries unlocksMenuData))
