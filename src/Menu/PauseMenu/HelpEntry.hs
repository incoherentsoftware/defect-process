module Menu.PauseMenu.HelpEntry
    ( PauseMenuHelpEntry(..)
    , mkPauseMenuHelpEntry
    , updatePauseMenuHelpEntry
    , drawPauseMenuHelpEntry
    , setPauseMenuHelpEntryIconButtonPos
    , isPauseMenuHelpEntryIconButtonSelectedOrActive
    , pauseMenuHelpEntriesLeft
    , pauseMenuHelpEntriesRight
    ) where

import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (listToMaybe)
import qualified Data.List as L

import Audio.Fmod
import Configs
import FileCache
import Menu.HelpPopup
import Menu.HelpPopup.Util
import Menu.SoundIndices
import Menu.ZIndex
import Util
import Window.Graphics
import Window.Graphics.UiControls.Button
import Window.InputState

data PauseMenuHelpEntry a = PauseMenuHelpEntry
    { _type         :: a
    , _active       :: Bool
    , _iconButton   :: Button
    , _helpPopup    :: HelpPopup
    , _soundIndices :: MenuSoundIndices
    }

mkPauseMenuHelpEntry
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => Pos2
    -> a
    -> HelpPopupDescription
    -> m (PauseMenuHelpEntry a)
mkPauseMenuHelpEntry iconButtonPos menuHelpPopupType menuHelpPopupDesc = do
    iconBtn      <- mkImageButton iconButtonPos (_iconButtonImagePath menuHelpPopupDesc)
    helpPopup    <- mkHelpPopup menuHelpPopupDesc
    soundIndices <- mkMenuSoundIndices

    return $ PauseMenuHelpEntry
        { _type         = menuHelpPopupType
        , _active       = False
        , _iconButton   = iconBtn
        , _helpPopup    = helpPopup
        , _soundIndices = soundIndices
        }

updatePauseMenuHelpEntry
    :: forall m a. (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => ButtonStatus
    -> PauseMenuHelpEntry a
    -> m (PauseMenuHelpEntry a)
updatePauseMenuHelpEntry iconBtnStatus helpEntry = do
    iconButton <- updateButton iconBtnStatus (_iconButton helpEntry)

    let
        isIconBtnPressed = _isPressed iconButton
        prevActive       = _active helpEntry
        helpPopup        = _helpPopup helpEntry

    helpPopup' <- if
        | prevActive -> updateHelpPopup helpPopup
        | otherwise  -> return helpPopup

    inputState <- readInputState
    let
        active
            | prevActive = not $ or
                [ MenuAlias `aliasPressed` inputState
                , MenuBackAlias `aliasPressed` inputState
                , isHelpPopupCloseButtonPressed helpPopup'
                ]
            | otherwise  = isIconBtnPressed

    when isIconBtnPressed $
        void . playFmodSound . _confirm $ _soundIndices (helpEntry :: PauseMenuHelpEntry a)

    return $ helpEntry
        { _active     = active
        , _iconButton = iconButton
        , _helpPopup  = helpPopup'
        }

drawPauseMenuHelpEntry :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => PauseMenuHelpEntry a -> m ()
drawPauseMenuHelpEntry helpEntry = do
    drawButton menuZIndex (_iconButton helpEntry)
    when (_active helpEntry) $
        drawHelpPopup $ _helpPopup helpEntry

setPauseMenuHelpEntryIconButtonPos :: Pos2 -> PauseMenuHelpEntry a -> PauseMenuHelpEntry a
setPauseMenuHelpEntryIconButtonPos pos helpEntry = helpEntry {_iconButton = button}
    where button = (_iconButton helpEntry) {_pos = pos}

isPauseMenuHelpEntryIconButtonSelectedOrActive :: PauseMenuHelpEntry a -> Bool
isPauseMenuHelpEntryIconButtonSelectedOrActive helpEntry = _isSelected (_iconButton helpEntry) || _active helpEntry

sortPauseMenuHelpEntries :: [PauseMenuHelpEntry a] -> [PauseMenuHelpEntry a]
sortPauseMenuHelpEntries helpEntries = L.sortBy cmp helpEntries
    where
        iconButtonPos = \he -> _pos $ _iconButton he
        cmp           = \he1 he2 -> compare (vecX $ iconButtonPos he1) (vecX $ iconButtonPos he2)

pauseMenuHelpEntriesLeft :: [PauseMenuHelpEntry a] -> Maybe (PauseMenuHelpEntry a)
pauseMenuHelpEntriesLeft helpEntries = listToMaybe $ sortPauseMenuHelpEntries helpEntries

pauseMenuHelpEntriesRight :: [PauseMenuHelpEntry a] -> Maybe (PauseMenuHelpEntry a)
pauseMenuHelpEntriesRight helpEntries = case sortPauseMenuHelpEntries helpEntries of
    [_, entryRight] -> Just entryRight
    _               -> Nothing
