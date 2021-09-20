module Menu.HelpPopup
    ( HelpPopup(..)
    , mkHelpPopup
    , updateHelpPopup
    , drawHelpPopup
    , isHelpPopupCloseButtonPressed
    ) where

import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (StateT, evalStateT, get, lift, put)
import Data.Foldable          (traverse_)
import Data.Functor           ((<&>))
import Data.Maybe             (isJust, fromMaybe, listToMaybe)
import Data.Traversable       (for)
import System.FilePath        (dropExtension)
import qualified Data.List as L
import qualified Data.Vector as V

import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Menu
import FileCache
import Menu.HelpPopup.Types
import Menu.HelpPopup.Util
import Menu.SoundIndices
import Menu.ZIndex
import Util
import Window.Graphics
import Window.Graphics.UiControls.Button
import Window.InputState

pauseMenuPack                   = \p -> PackResourceFilePath "data/menu/pause-menu.pack" p
closeButtonImgPath              = pauseMenuPack "close-button.image"                :: PackResourceFilePath
showAlternateInputButtonImgPath = pauseMenuPack "show-alternate-input-button.image" :: PackResourceFilePath

screenBackgroundImgPath =
    PackResourceFilePath "data/menu/help/various-help.pack" "background.image" :: PackResourceFilePath

inactiveImageNameSuffix = "-inactive.image" :: FileName
selectedImageNameSuffix = "-selected.image" :: FileName

mkHelpPopupTextOverlay
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => HelpPopupTextOverlayDescription
    -> m HelpPopupTextOverlay
mkHelpPopupTextOverlay popupTextOverlayDesc = do
    let txt         = _text (popupTextOverlayDesc :: HelpPopupTextOverlayDescription)
    inputDisplayTxt <- mkInputDisplayText txt Font22 whiteColor

    return $ HelpPopupTextOverlay
        { _inputDisplayText = inputDisplayTxt
        , _draw             = _draw (popupTextOverlayDesc :: HelpPopupTextOverlayDescription)
        }

updateHelpPopupTextOverlay
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => HelpPopupTextOverlay
    -> m HelpPopupTextOverlay
updateHelpPopupTextOverlay popupTextOverlay = do
    inputDisplayTxt <- updateInputDisplayText $ _inputDisplayText (popupTextOverlay :: HelpPopupTextOverlay)
    return $ popupTextOverlay {_inputDisplayText = inputDisplayTxt}

drawHelpPopupTextOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => HelpPopupTextOverlay -> m ()
drawHelpPopupTextOverlay popupTextOverlay = (_draw (popupTextOverlay :: HelpPopupTextOverlay)) popupTextOverlay

mkHelpPopupAltOverlay
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => PackResourceFilePath
    -> m HelpPopupImageAltOverlay
mkHelpPopupAltOverlay imageOverlayPath = do
    popupImg           <- loadPackImage imageOverlayPath
    btnPos             <- readSettingsConfig _menu _helpPopupAltControlsButtonPos
    showHideOverlayBtn <- mkImageButton btnPos showAlternateInputButtonImgPath

    return $ HelpPopupImageAltOverlay
        { _overlayActive     = False
        , _popupImageOverlay = popupImg
        , _showOverlayButton = showHideOverlayBtn
        , _hideOverlayButton = showHideOverlayBtn
        }

mkHelpPopup
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => HelpPopupDescription
    -> m HelpPopup
mkHelpPopup helpPopupDesc = do
    screenTabs <- case _popupDescription helpPopupDesc of
        Left popupScreenDesc -> do
            backgroundImg <- loadPackImage screenBackgroundImgPath
            img           <- loadPackImage $ _imagePath (popupScreenDesc :: HelpPopupScreenDescription)
            textOverlays  <- traverse mkHelpPopupTextOverlay $
                (_textOverlayDescriptions (popupScreenDesc :: HelpPopupScreenDescription))
            return . Left $ HelpPopupScreen
                { _backgroundImage = backgroundImg
                , _image           = img
                , _textOverlays    = textOverlays
                }

        Right popupTabDescs -> do
            tabInactiveButtons <- for popupTabDescs $ \tabDesc ->
                let
                    tabBtnImgPath = _tabButtonImagePath tabDesc
                    baseFileName  = dropExtension $ _fileName tabBtnImgPath
                    path          = tabBtnImgPath {_fileName = baseFileName ++ inactiveImageNameSuffix}
                    selectedPath  = tabBtnImgPath {_fileName = baseFileName ++ selectedImageNameSuffix}
                in mkImageButtonEx (_tabButtonPos tabDesc) path selectedPath

            tabs <- for (zip [0..] popupTabDescs) $ \(i, tabDesc) -> do
                img           <- loadPackImage $ _imagePath (tabDesc :: HelpPopupTabDescription)
                imgAltOverlay <- case _imageAltOverlayPath tabDesc of
                    Nothing   -> return Nothing
                    Just path -> Just <$> mkHelpPopupAltOverlay path

                tabBtn <- mkImageButton (_tabButtonPos tabDesc) (_tabButtonImagePath tabDesc)
                let
                    tabCount = length popupTabDescs
                    tabBtns  = V.fromList $
                        take tabCount (take i tabInactiveButtons ++ [tabBtn] ++ drop (i + 1) tabInactiveButtons)

                textOverlays <- traverse mkHelpPopupTextOverlay $
                    (_textOverlayDescriptions (tabDesc :: HelpPopupTabDescription))

                return $ HelpPopupTab
                    { _index           = i
                    , _image           = img
                    , _tabButtons      = tabBtns
                    , _imageAltOverlay = imgAltOverlay
                    , _textOverlays    = textOverlays
                    }

            return $ Right tabs

    menuCfg      <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    closeBtn     <- mkImageButton (_helpPopupCloseButtonPos menuCfg) closeButtonImgPath
    soundIndices <- mkMenuSoundIndices

    return $ HelpPopup
        { _screenTabs   = screenTabs
        , _closeButton  = closeBtn
        , _selection    = HelpPopupCloseSelection
        , _soundIndices = soundIndices
        }

relativeLeftTabSelection :: PosX -> HelpPopupTab -> HelpPopupSelection
relativeLeftTabSelection tabBtnX tab = chooseLeft $ reverse tabBtnsList
    where
        tabBtnsList = V.toList $ _tabButtons tab

        chooseLeft :: [Button] -> HelpPopupSelection
        chooseLeft []                    = HelpPopupTabSelection $ maybe tabBtnX (vecX . _pos) (maybeLast tabBtnsList)
        chooseLeft (btn:btns)
            | round btnX < round tabBtnX = HelpPopupTabSelection btnX
            | otherwise                  = chooseLeft btns
            where btnX = vecX $ _pos btn

relativeRightTabSelection :: PosX -> HelpPopupTab -> HelpPopupSelection
relativeRightTabSelection tabBtnX tab = chooseRight $ V.toList tabBtns
    where
        tabBtns = _tabButtons tab

        chooseRight :: [Button] -> HelpPopupSelection
        chooseRight []                   =  HelpPopupTabSelection $ maybe tabBtnX (vecX . _pos) (tabBtns V.!? 0)
        chooseRight (btn:btns)
            | round btnX > round tabBtnX = HelpPopupTabSelection btnX
            | otherwise                  = chooseRight btns
            where btnX = vecX $ _pos btn

currentTabSelection :: HelpPopupTab -> Maybe HelpPopupSelection
currentTabSelection tab = case _tabButtons tab V.!? tabIndex of
    Just tabBtn -> Just $ HelpPopupTabSelection (btnPosX tabBtn)
    Nothing     -> Nothing
    where
        btnPosX  = \btn -> vecX $ _pos btn
        tabIndex = _index (tab :: HelpPopupTab)

readHelpPopupSelection :: InputState -> HelpPopup -> HelpPopupSelection
readHelpPopupSelection inputState helpPopup = case _selection helpPopup of
    HelpPopupTabSelection tabBtnX
        | upPressed || downPressed -> HelpPopupCloseSelection
        | otherwise                -> case _screenTabs helpPopup of
            Right (tab:_)
                | leftPressed  -> relativeLeftTabSelection tabBtnX tab
                | rightPressed -> relativeRightTabSelection tabBtnX tab
            _                  -> HelpPopupTabSelection tabBtnX

    HelpPopupCloseSelection
        | (leftPressed || rightPressed) && showAltControls -> HelpPopupAltControlsSelection
        | upPressed || downPressed                         -> fromMaybe HelpPopupCloseSelection currentTabSelection'

    HelpPopupAltControlsSelection
        | leftPressed || rightPressed -> HelpPopupCloseSelection
        | upPressed || downPressed    -> fromMaybe HelpPopupAltControlsSelection currentTabSelection'

    selection -> selection

    where
        showAltControls = case _screenTabs helpPopup of
            Right (tab:_) -> isJust $ _imageAltOverlay tab
            _             -> False

        upPressed    = MenuUpAlias `aliasPressed` inputState
        downPressed  = MenuDownAlias `aliasPressed` inputState
        leftPressed  = MenuLeftAlias `aliasPressed` inputState
        rightPressed = MenuRightAlias `aliasPressed` inputState

        currentTabSelection' = case _screenTabs helpPopup of
            Right (tab:_) -> currentTabSelection tab
            _             -> Nothing

updateHelpPopupHelpPopupScreen
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => HelpPopupScreen
    -> m HelpPopupScreen
updateHelpPopupHelpPopupScreen screen = do
    textOverlays <- traverse updateHelpPopupTextOverlay (_textOverlays (screen :: HelpPopupScreen))
    return $ (screen :: HelpPopupScreen) {_textOverlays = textOverlays}

updateHelpPopupHelpPopupTab
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => V.Vector Button
    -> Maybe HelpPopupImageAltOverlay
    -> HelpPopupTab
    -> m HelpPopupTab
updateHelpPopupHelpPopupTab tabButtons altOverlay tab = do
    textOverlays <- traverse updateHelpPopupTextOverlay (_textOverlays (tab :: HelpPopupTab))
    return $ tab
        { _tabButtons      = tabButtons
        , _imageAltOverlay = altOverlay
        , _textOverlays    = textOverlays
        }

sortHelpPopupHelpPopupTabs :: [HelpPopupTab] -> [HelpPopupTab]
sortHelpPopupHelpPopupTabs = \case
    []              -> []
    allTabs@(tab:_) ->
        let tabBtnsList = V.toList $ _tabButtons tab
        in case listToMaybe [i | (i, btn) <- zip [0..] tabBtnsList, _isPressed btn] of
            Nothing              -> allTabs
            Just pressedBtnIndex -> flip L.sortBy allTabs $ \t1 t2 -> if
                | _index (t1 :: HelpPopupTab) == pressedBtnIndex -> LT
                | _index (t2 :: HelpPopupTab) == pressedBtnIndex -> GT
                | otherwise                                      -> EQ

tabLeftRightInputPressedSelection :: InputState -> HelpPopupTab -> Maybe HelpPopupSelection
tabLeftRightInputPressedSelection inputState tab = case currentTabSelection tab of
    Just (HelpPopupTabSelection tabBtnX)
        | MenuTabLeftAlias `aliasPressed` inputState  -> Just $ relativeLeftTabSelection tabBtnX tab
        | MenuTabRightAlias `aliasPressed` inputState -> Just $ relativeRightTabSelection tabBtnX tab
    _                                                 -> Nothing

updateHelpPopup :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => HelpPopup -> m HelpPopup
updateHelpPopup helpPopup = do
    inputState          <- readInputState
    let initialSelection = readHelpPopupSelection inputState helpPopup

    flip evalStateT initialSelection $
        let
            updateNonTabButton :: InputRead m1 => HelpPopupSelection -> Button -> StateT HelpPopupSelection m1 Button
            updateNonTabButton popupBtnSelection popupBtn = do
                popupBtnStatus <- get <&> \selection -> case (popupBtnSelection, selection) of
                    (HelpPopupCloseSelection, HelpPopupCloseSelection)             -> ButtonSelectedActiveStatus
                    (HelpPopupAltControlsSelection, HelpPopupAltControlsSelection) -> ButtonSelectedActiveStatus
                    _                                                              -> ButtonActiveStatus
                popupBtn'      <- lift $ updateButton popupBtnStatus popupBtn

                when (_isSelected popupBtn' || _isPressed popupBtn') $
                    put popupBtnSelection
                return popupBtn'

            updateTabButton
                :: InputRead m1
                => Maybe HelpPopupSelection
                -> Button
                -> StateT HelpPopupSelection m1 Button
            updateTabButton tabLeftRightInputSelection tabBtn = do
                let tabBtnX = vecX $ _pos tabBtn
                tabBtn'    <- case tabLeftRightInputSelection of
                    Just (HelpPopupTabSelection posX)
                        | tabBtnX `approxEq` posX -> return $ setButtonSelectedPressed tabBtn
                    _                             -> do
                        btnStatus <- get <&> \case
                            HelpPopupTabSelection posX
                                | tabBtnX `approxEq` posX -> ButtonSelectedActiveStatus
                            _                             -> ButtonActiveStatus
                        lift $ updateButton btnStatus tabBtn

                when (_isSelected tabBtn' || _isPressed tabBtn') $
                    put $ HelpPopupTabSelection (vecX (_pos tabBtn'))
                return tabBtn'

            soundIndices = _soundIndices (helpPopup :: HelpPopup)

            playConfirmSmallSound :: MonadIO m1 => m1 ()
            playConfirmSmallSound = void $ playFmodSound (_confirmSmall soundIndices)

            updateImageAltOverlay
                :: (InputRead m1, MonadIO m1)
                => Maybe HelpPopupImageAltOverlay
                -> StateT HelpPopupSelection m1 (Maybe HelpPopupImageAltOverlay)
            updateImageAltOverlay = \case
                Nothing -> return Nothing

                Just overlay
                    | _overlayActive overlay -> do
                        hideOverlayBtn   <-
                            updateNonTabButton HelpPopupAltControlsSelection (_hideOverlayButton overlay)
                        let overlayActive = not (_isPressed hideOverlayBtn)
                        when (not overlayActive) $
                            playConfirmSmallSound

                        return . Just $ overlay
                            { _overlayActive     = overlayActive
                            , _hideOverlayButton = hideOverlayBtn
                            }

                    | otherwise -> do
                        showOverlayBtn   <-
                            updateNonTabButton HelpPopupAltControlsSelection (_showOverlayButton overlay)
                        let overlayActive = _isPressed showOverlayBtn
                        when overlayActive $
                            playConfirmSmallSound

                        return . Just $ overlay
                            { _overlayActive     = overlayActive
                            , _showOverlayButton = showOverlayBtn
                            }
        in do
            closeBtn <- updateNonTabButton HelpPopupCloseSelection (_closeButton helpPopup)

            screenTabs <- case _screenTabs helpPopup of
                Left screen -> Left <$> lift (updateHelpPopupHelpPopupScreen screen)

                Right (tab:tabs) -> do
                    let tabLeftRightInputSelection = tabLeftRightInputPressedSelection inputState tab

                    tabButtons <- traverse (updateTabButton tabLeftRightInputSelection) (_tabButtons tab)
                    altOverlay <- updateImageAltOverlay $ _imageAltOverlay tab

                    when (V.or (V.map _isPressed tabButtons)) $
                        playConfirmSmallSound

                    tab' <- lift $ updateHelpPopupHelpPopupTab tabButtons altOverlay tab
                    let allTabs = sortHelpPopupHelpPopupTabs $ tab':tabs
                    return $ Right allTabs

                Right [] -> return $ Right []

            when (_isPressed closeBtn) $
                void $ playFmodSound (_confirm soundIndices)

            get <&> \selection -> helpPopup
                { _screenTabs  = screenTabs
                , _closeButton = closeBtn
                , _selection   = selection
                }

drawHelpPopup :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => HelpPopup -> m ()
drawHelpPopup helpPopup = do
    case _screenTabs helpPopup of
        Left screen -> do
            drawImage zeroPos2 RightDir menuOverZIndex (_backgroundImage screen)
            drawImage zeroPos2 RightDir menuOverZIndex (_image (screen :: HelpPopupScreen))
            traverse_ drawHelpPopupTextOverlay (_textOverlays (screen :: HelpPopupScreen))

        Right []      -> return ()
        Right (tab:_) -> do
            drawImage zeroPos2 RightDir menuOverZIndex (_image (tab :: HelpPopupTab))
            traverse_ (drawButton menuOverZIndex) (_tabButtons tab)

            case _imageAltOverlay tab of
                Nothing                         -> return ()
                Just altOverlay
                    | _overlayActive altOverlay -> do
                        drawImage zeroPos2 RightDir menuOverZIndex (_popupImageOverlay altOverlay)
                        drawButton menuOverZIndex (_hideOverlayButton altOverlay)
                    | otherwise                 -> drawButton menuOverZIndex (_showOverlayButton altOverlay)

            traverse_ drawHelpPopupTextOverlay (_textOverlays (tab :: HelpPopupTab))

    drawButton menuOverZIndex (_closeButton helpPopup)

isHelpPopupCloseButtonPressed :: HelpPopup -> Bool
isHelpPopupCloseButtonPressed = _isPressed . _closeButton
