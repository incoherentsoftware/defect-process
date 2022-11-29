module Configs.All.Settings.Menu
    ( MenuConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Menu.SettingsMenu.ControlsTab.KeyButtons.JSON
import Menu.UnlocksMenu.JSON
import Util
import World.Audio.LayeredMusic.Types
import {-# SOURCE #-} Player.Gun.Types
import {-# SOURCE #-} Player.MovementSkill.Types
import {-# SOURCE #-} Player.SecondarySkill.Types
import {-# SOURCE #-} Player.Weapon.Types

data MenuConfig = MenuConfig
    { _menuCursorHotspotPos       :: Pos2
    , _mainNewGameButtonPosY      :: PosY
    , _mainContinueButtonPos      :: Pos2
    , _mainUnlocksButtonPosY      :: PosY
    , _mainSettingsButtonPosY     :: PosY
    , _mainQuitButtonPosY         :: PosY
    , _mainPromptImagePos         :: Pos2
    , _mainPromptQuitButtonPos    :: Pos2
    , _mainPromptCancelButtonPos  :: Pos2
    , _mainPromptNewGameButtonPos :: Pos2

    , _settingsControlsButtonPos :: Pos2
    , _settingsGraphicsButtonPos :: Pos2
    , _settingsAudioButtonPos    :: Pos2
    , _settingsGameButtonPos     :: Pos2
    , _settingsCreditsButtonPos  :: Pos2
    , _settingsCloseButtonPos    :: Pos2

    , _settingsControlsTabControlsKeyButtons                :: [ControlsKeyButtonJSON]
    , _settingsControlsTabMouseKbControlsKeyButtons         :: [ControlsKeyButtonJSON]
    , _settingsControlsTabControlsKeyButtonsTopDefaultIndex :: Int
    , _settingsControlsTabControlsKeyButtonsBotDefaultIndex :: Int
    , _settingsControlsTabControlsKeyButtonsValueOffset     :: Pos2
    , _settingsControlsTabShowToggleButtonPos               :: Pos2
    , _settingsControlsTabRestoreDefaultsButtonPos          :: Pos2

    , _settingsGraphicsTabResolutionComboBoxPos          :: Pos2
    , _settingsGraphicsTabDisplayModeComboBoxPos         :: Pos2
    , _settingsGraphicsTabResolutionComboBoxValueOffset  :: Pos2
    , _settingsGraphicsTabDisplayModeComboBoxValueOffset :: Pos2
    , _settingsGraphicsTabRestoreDefaultsButtonPos       :: Pos2

    , _settingsAudioTabSoundComboBoxPos         :: Pos2
    , _settingsAudioTabMusicComboBoxPos         :: Pos2
    , _settingsAudioTabComboBoxValueOffset      :: Pos2
    , _settingsAudioTabRestoreDefaultsButtonPos :: Pos2

    , _settingsGameTabEnemyHealthComboBoxPos            :: Pos2
    , _settingsGameTabPauseMenuHintsComboBoxPos         :: Pos2
    , _settingsGameTabEnemyHealthComboBoxValueOffset    :: Pos2
    , _settingsGameTabPauseMenuHintsComboBoxValueOffset :: Pos2
    , _settingsGameTabRestoreDefaultsButtonPos          :: Pos2

    , _helpPopupCloseButtonPos                 :: Pos2
    , _helpPopupControlsButtonPos              :: Pos2
    , _helpPopupAltControlsButtonPos           :: Pos2
    , _helpPopupTargetingTextPositions         :: (Pos2, Pos2, Pos2, Pos2, Pos2, Pos2, Pos2)
    , _helpPopupSwordBasicTabButtonPos         :: Pos2
    , _helpPopupSwordSpecialTabButtonPos       :: Pos2
    , _helpPopupGauntletsBasicTabButtonPos     :: Pos2
    , _helpPopupGauntletsSpecialTabButtonPos   :: Pos2
    , _helpPopupGauntletsChargedTabButtonPos   :: Pos2
    , _helpPopupScytheBasicTabButtonPos        :: Pos2
    , _helpPopupScytheSpecialTabButtonPos      :: Pos2
    , _helpPopupScytheFloatingTabButtonPos     :: Pos2
    , _helpPopupStaffBasicTabButtonPos         :: Pos2
    , _helpPopupStaffSpecialTabButtonPos       :: Pos2
    , _helpPopupSpiritBladeBasicTabButtonPos   :: Pos2
    , _helpPopupSpiritBladeSpecialTabButtonPos :: Pos2

    , _pausedResumeButtonPos                   :: Pos2
    , _pausedMainMenuButtonPos                 :: Pos2
    , _pausedSettingsButtonPos                 :: Pos2
    , _pausedGeneralInfoHelpEntryPos           :: Pos2
    , _pausedTargetingInfoHelpEntryPos         :: Pos2
    , _pausedWeaponHelpEntryPositions          :: [Pos2]
    , _pausedGunHelpEntryPositions             :: [Pos2]
    , _pausedMovementSkillHelpEntryPositions   :: [Pos2]
    , _pausedSecondarySkillNeutralHelpEntryPos :: Pos2
    , _pausedSecondarySkillUpHelpEntryPos      :: Pos2
    , _pausedSecondarySkillDownHelpEntryPos    :: Pos2
    , _pausedUpgradeHelpEntryPositions         :: [Pos2]

    , _unlocksTotalGoldTextPos      :: Pos2
    , _unlocksMainMenuButtonPos     :: Pos2
    , _unlocksTitleTextPos          :: Pos2
    , _unlocksSubTitleTextPos       :: Pos2
    , _unlocksUnlocksTextPos        :: Pos2
    , _unlocksUnavailableTextPos    :: Pos2
    , _unlocksMusicEntries          :: [UnlocksEntryJSON LayeredMusicType]
    , _unlocksWeaponEntries         :: [UnlocksEntryJSON WeaponType]
    , _unlocksGunEntries            :: [UnlocksEntryJSON GunType]
    , _unlocksMovementSkillEntries  :: [UnlocksEntryJSON MovementSkillType]
    , _unlocksSecondarySkillEntries :: [UnlocksEntryJSON SecondarySkillType]
    , _unlocksCenterTopEntryIndex   :: Int
    , _unlocksCenterBotEntryIndex   :: Int
    }
    deriving Generic

instance FromJSON MenuConfig where
    parseJSON value = genericParseJSON aesonFieldDropUnderscore value
