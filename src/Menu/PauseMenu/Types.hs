module Menu.PauseMenu.Types
    ( module Menu.PauseMenu.Selection.Types
    , module Menu.PauseMenu.SlotComboBoxes.Types
    , PauseMenuSelection(..)
    , PauseMenuData(..)
    ) where

import qualified Data.Map as M

import Audio.Fmod.Types
import Menu.PauseMenu.HelpEntry
import Menu.PauseMenu.Selection.Types
import Menu.PauseMenu.SlotComboBoxes.Types
import Menu.SettingsMenu.Types
import Menu.SoundIndices.Types
import Player.EquipmentInfo
import Player.Gun.Types
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Upgrade
import Player.Weapon.Types
import Window.Graphics
import Window.Graphics.UiControls

data PauseMenuData = PauseMenuData
    { _backgroundImage                        :: Image
    , _emptyIconOverlayImage                  :: Image
    , _infoOverlayLeftImage                   :: Image
    , _infoOverlayRightImage                  :: Image
    , _secondarySkillUpIconOverlayImage       :: Image
    , _secondarySkillDownIconOverlayImage     :: Image
    , _secondarySkillNeutralInputOverlayImage :: Image
    , _secondarySkillUpInputOverlayImage      :: Image
    , _secondarySkillDownInputOverlayImage    :: Image
    , _upgradeCountOverlayDisplayTexts        :: M.Map PlayerUpgradeType DisplayText
    , _resumeButton                           :: Button
    , _mainMenuButton                         :: Button
    , _settingsButton                         :: Button
    , _settingsMenuData                       :: SettingsMenuData
    , _viewInfoInputDisplayText               :: InputDisplayText
    , _changeSlotInputDisplayText             :: InputDisplayText
    , _generalHelpEntry                       :: PauseMenuHelpEntry ()
    , _targetingHelpEntry                     :: PauseMenuHelpEntry ()
    , _weaponHelpEntries                      :: [PauseMenuHelpEntry WeaponType]
    , _gunHelpEntries                         :: [PauseMenuHelpEntry GunType]
    , _movementSkillHelpEntries               :: [PauseMenuHelpEntry MovementSkillType]
    , _secondarySkillNeutralHelpEntry         :: Maybe (PauseMenuHelpEntry SecondarySkillType)
    , _secondarySkillUpHelpEntry              :: Maybe (PauseMenuHelpEntry SecondarySkillType)
    , _secondarySkillDownHelpEntry            :: Maybe (PauseMenuHelpEntry SecondarySkillType)
    , _upgradeHelpEntries                     :: [PauseMenuHelpEntry PlayerUpgradeType]
    , _slotComboBoxes                         :: PauseMenuSlotComboBoxes
    , _equipmentInfo                          :: PlayerEquipmentInfo
    , _selection                              :: Maybe PauseMenuSelection
    , _musicIndex                             :: FmodMusicIndex
    , _soundIndices                           :: MenuSoundIndices
    }
