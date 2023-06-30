module Menu.PauseMenu.Selection
    ( readSelection
    , updateSelectionFromSlotComboBoxes
    ) where

import Control.Monad.State (StateT, put)
import Data.Maybe          (isJust)

import Game.Mode
import Menu.PauseMenu.HelpEntry
import Menu.PauseMenu.Selection.Types
import Menu.PauseMenu.SlotComboBoxes
import Menu.PauseMenu.Types
import Window.Graphics.UiControls
import Window.InputState

readSelection :: InputState -> GameMode -> PauseMenuData -> PauseMenuSelection
readSelection inputState prevGameMode pauseMenuData = case _selection (pauseMenuData :: PauseMenuData) of
    Nothing
        | prevGameMode == MainMenuMode -> PauseMenuMainSelection
        | otherwise                    -> PauseMenuResumeSelection

    Just selection
        | isSlotComboBoxesExpanded (_slotComboBoxes pauseMenuData) -> selection

    Just PauseMenuMainSelection
        | upPressed                   -> PauseMenuTauntingInfoSelection
        | downPressed                 -> PauseMenuGeneralInfoSelection
        | leftPressed || rightPressed -> PauseMenuResumeSelection

    Just PauseMenuSettingsSelection
        | leftPressed                              -> PauseMenuResumeSelection
        | rightPressed                             -> PauseMenuMainSelection
        | upPressed && upgradeSelectionCount > 2   -> PauseMenuUpgradeSelection 2
        | upPressed && upgradeSelectionCount > 1   -> PauseMenuUpgradeSelection 1
        | upPressed && upgradeSelectionCount > 0   -> PauseMenuUpgradeSelection 0
        | downPressed && upgradeSelectionCount > 0 -> PauseMenuUpgradeSelection 0

    Just PauseMenuResumeSelection
        | leftPressed                                 -> PauseMenuMainSelection
        | rightPressed                                -> PauseMenuSettingsSelection
        | upPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | upPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | upPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | upPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | upPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | upPressed && isWeaponHelpEntryLeft          -> PauseMenuWeaponLeftSelection
        | downPressed && isWeaponHelpEntryLeft        -> PauseMenuWeaponLeftSelection
        | downPressed && isGunHelpEntryLeft           -> PauseMenuGunLeftSelection
        | downPressed && isMovementSkillHelpEntry     -> PauseMenuMovementSkillSelection
        | upPressed || downPressed                    -> PauseMenuGeneralInfoSelection

    Just PauseMenuGeneralInfoSelection
        | upPressed                                      -> PauseMenuMainSelection
        | downPressed                                    -> PauseMenuTargetingInfoSelection
        | rightPressed && isWeaponHelpEntryLeft          -> PauseMenuWeaponLeftSelection
        | rightPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | rightPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | rightPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | rightPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | rightPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | rightPressed && upgradeSelectionCount > 0      -> PauseMenuUpgradeSelection 0
        | leftPressed  && upgradeSelectionCount > 0      -> PauseMenuUpgradeSelection 0
        | leftPressed && isWeaponHelpEntryRight          -> PauseMenuWeaponRightSelection
        | leftPressed && isWeaponHelpEntryLeft           -> PauseMenuWeaponLeftSelection
        | leftPressed && isGunHelpEntryRight             -> PauseMenuGunRightSelection
        | leftPressed && isGunHelpEntryLeft              -> PauseMenuGunLeftSelection
        | leftPressed && isMovementSkillHelpEntry        -> PauseMenuMovementSkillSelection
        | leftPressed && isSecondarySkillHelpEntryRight  -> PauseMenuSecondarySkillRightSelection
        | leftPressed && isSecondarySkillHelpEntryMid    -> PauseMenuSecondarySkillMidSelection
        | leftPressed && isSecondarySkillHelpEntryLeft   -> PauseMenuSecondarySkillLeftSelection

    Just PauseMenuTargetingInfoSelection
        | upPressed                                      -> PauseMenuGeneralInfoSelection
        | downPressed                                    -> PauseMenuTauntingInfoSelection
        | rightPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | rightPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | rightPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | rightPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | rightPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | rightPressed && isWeaponHelpEntryLeft          -> PauseMenuWeaponLeftSelection
        | rightPressed && upgradeSelectionCount > 1      -> PauseMenuUpgradeSelection 1
        | rightPressed && upgradeSelectionCount > 0      -> PauseMenuUpgradeSelection 0
        | leftPressed && upgradeSelectionCount > 1       -> PauseMenuUpgradeSelection 1
        | leftPressed && upgradeSelectionCount > 0       -> PauseMenuUpgradeSelection 0
        | leftPressed && isGunHelpEntryRight             -> PauseMenuGunRightSelection
        | leftPressed && isGunHelpEntryLeft              -> PauseMenuGunLeftSelection
        | leftPressed && isMovementSkillHelpEntry        -> PauseMenuMovementSkillSelection
        | leftPressed && isSecondarySkillHelpEntryRight  -> PauseMenuSecondarySkillRightSelection
        | leftPressed && isSecondarySkillHelpEntryMid    -> PauseMenuSecondarySkillMidSelection
        | leftPressed && isSecondarySkillHelpEntryLeft   -> PauseMenuSecondarySkillLeftSelection
        | leftPressed && isWeaponHelpEntryRight          -> PauseMenuWeaponRightSelection
        | leftPressed && isWeaponHelpEntryLeft           -> PauseMenuWeaponLeftSelection

    Just PauseMenuTauntingInfoSelection
        | upPressed                                      -> PauseMenuTargetingInfoSelection
        | downPressed                                    -> PauseMenuMainSelection
        | rightPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | rightPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | rightPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | rightPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | rightPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | rightPressed && isWeaponHelpEntryLeft          -> PauseMenuWeaponLeftSelection
        | rightPressed && upgradeSelectionCount > 2      -> PauseMenuUpgradeSelection 2
        | rightPressed && upgradeSelectionCount > 1      -> PauseMenuUpgradeSelection 1
        | rightPressed && upgradeSelectionCount > 0      -> PauseMenuUpgradeSelection 0
        | leftPressed && upgradeSelectionCount > 2       -> PauseMenuUpgradeSelection 2
        | leftPressed && upgradeSelectionCount > 1       -> PauseMenuUpgradeSelection 1
        | leftPressed && upgradeSelectionCount > 0       -> PauseMenuUpgradeSelection 0
        | leftPressed && isMovementSkillHelpEntry        -> PauseMenuMovementSkillSelection
        | leftPressed && isGunHelpEntryRight             -> PauseMenuGunRightSelection
        | leftPressed && isGunHelpEntryLeft              -> PauseMenuGunLeftSelection
        | leftPressed && isSecondarySkillHelpEntryRight  -> PauseMenuSecondarySkillRightSelection
        | leftPressed && isSecondarySkillHelpEntryMid    -> PauseMenuSecondarySkillMidSelection
        | leftPressed && isSecondarySkillHelpEntryLeft   -> PauseMenuSecondarySkillLeftSelection
        | leftPressed && isWeaponHelpEntryRight          -> PauseMenuWeaponRightSelection
        | leftPressed && isWeaponHelpEntryLeft           -> PauseMenuWeaponLeftSelection

    Just PauseMenuWeaponLeftSelection
        | rightPressed && isWeaponHelpEntryRight        -> PauseMenuWeaponRightSelection
        | rightPressed && upgradeSelectionCount > 0     -> PauseMenuUpgradeSelection 0
        | rightPressed || leftPressed                   -> PauseMenuGeneralInfoSelection
        | upPressed                                     -> PauseMenuResumeSelection
        | downPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | downPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | downPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | downPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | downPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | downPressed                                   -> PauseMenuResumeSelection

    Just PauseMenuWeaponRightSelection
        | leftPressed                                   -> PauseMenuWeaponLeftSelection
        | rightPressed && upgradeSelectionCount > 0     -> PauseMenuUpgradeSelection 0
        | rightPressed                                  -> PauseMenuGeneralInfoSelection
        | upPressed                                     -> PauseMenuResumeSelection
        | downPressed && isGunHelpEntryRight            -> PauseMenuGunRightSelection
        | downPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | downPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | downPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | downPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | downPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | downPressed                                   -> PauseMenuResumeSelection

    Just PauseMenuGunLeftSelection
        | rightPressed && isGunHelpEntryRight           -> PauseMenuGunRightSelection
        | rightPressed && upgradeSelectionCount > 1     -> PauseMenuUpgradeSelection 1
        | rightPressed && upgradeSelectionCount > 0     -> PauseMenuUpgradeSelection 0
        | rightPressed || leftPressed                   -> PauseMenuTargetingInfoSelection
        | upPressed && isWeaponHelpEntryLeft            -> PauseMenuWeaponLeftSelection
        | upPressed                                     -> PauseMenuGeneralInfoSelection
        | downPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | downPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | downPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | downPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | downPressed                                   -> PauseMenuResumeSelection

    Just PauseMenuGunRightSelection
        | leftPressed                                   -> PauseMenuGunLeftSelection
        | rightPressed && upgradeSelectionCount > 1     -> PauseMenuUpgradeSelection 1
        | rightPressed && upgradeSelectionCount > 0     -> PauseMenuUpgradeSelection 0
        | rightPressed                                  -> PauseMenuTargetingInfoSelection
        | upPressed && isWeaponHelpEntryRight           -> PauseMenuWeaponRightSelection
        | upPressed && isWeaponHelpEntryLeft            -> PauseMenuWeaponLeftSelection
        | upPressed                                     -> PauseMenuGeneralInfoSelection
        | downPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | downPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | downPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | downPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | downPressed                                   -> PauseMenuResumeSelection

    Just PauseMenuMovementSkillSelection
        | rightPressed && upgradeSelectionCount > 2     -> PauseMenuUpgradeSelection 2
        | rightPressed && upgradeSelectionCount > 1     -> PauseMenuUpgradeSelection 1
        | rightPressed && upgradeSelectionCount > 0     -> PauseMenuUpgradeSelection 0
        | rightPressed && isGunHelpEntryRight           -> PauseMenuGunRightSelection
        | leftPressed || rightPressed                   -> PauseMenuTauntingInfoSelection
        | upPressed && isGunHelpEntryLeft               -> PauseMenuGunLeftSelection
        | upPressed && isWeaponHelpEntryLeft            -> PauseMenuWeaponLeftSelection
        | downPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | downPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | downPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | upPressed || downPressed                      -> PauseMenuResumeSelection

    Just PauseMenuSecondarySkillLeftSelection
        | rightPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | rightPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | rightPressed && upgradeSelectionCount > 2      -> PauseMenuUpgradeSelection 2
        | rightPressed && upgradeSelectionCount > 1      -> PauseMenuUpgradeSelection 1
        | rightPressed && upgradeSelectionCount > 0      -> PauseMenuUpgradeSelection 0
        | leftPressed || rightPressed                    -> PauseMenuTauntingInfoSelection
        | upPressed && isMovementSkillHelpEntry          -> PauseMenuMovementSkillSelection
        | upPressed && isGunHelpEntryLeft                -> PauseMenuGunLeftSelection
        | upPressed && isWeaponHelpEntryLeft             -> PauseMenuWeaponLeftSelection
        | upPressed || downPressed                       -> PauseMenuResumeSelection

    Just PauseMenuSecondarySkillMidSelection
        | leftPressed && isSecondarySkillHelpEntryLeft   -> PauseMenuSecondarySkillLeftSelection
        | leftPressed && isMovementSkillHelpEntry        -> PauseMenuMovementSkillSelection
        | rightPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | rightPressed && upgradeSelectionCount > 2      -> PauseMenuUpgradeSelection 2
        | rightPressed && upgradeSelectionCount > 1      -> PauseMenuUpgradeSelection 1
        | rightPressed && upgradeSelectionCount > 0      -> PauseMenuUpgradeSelection 0
        | leftPressed || rightPressed                    -> PauseMenuTauntingInfoSelection
        | upPressed && isMovementSkillHelpEntry          -> PauseMenuMovementSkillSelection
        | upPressed && isGunHelpEntryRight               -> PauseMenuGunRightSelection
        | upPressed && isGunHelpEntryLeft                -> PauseMenuGunLeftSelection
        | upPressed && isWeaponHelpEntryRight            -> PauseMenuWeaponRightSelection
        | upPressed && isWeaponHelpEntryLeft             -> PauseMenuWeaponLeftSelection
        | upPressed || downPressed                       -> PauseMenuResumeSelection

    Just PauseMenuSecondarySkillRightSelection
        | leftPressed && isSecondarySkillHelpEntryMid  -> PauseMenuSecondarySkillMidSelection
        | leftPressed && isSecondarySkillHelpEntryLeft -> PauseMenuSecondarySkillLeftSelection
        | leftPressed && isMovementSkillHelpEntry      -> PauseMenuMovementSkillSelection
        | rightPressed && upgradeSelectionCount > 2    -> PauseMenuUpgradeSelection 2
        | rightPressed && upgradeSelectionCount > 1    -> PauseMenuUpgradeSelection 1
        | rightPressed && upgradeSelectionCount > 0    -> PauseMenuUpgradeSelection 0
        | rightPressed || leftPressed                  -> PauseMenuTauntingInfoSelection
        | upPressed && isMovementSkillHelpEntry        -> PauseMenuMovementSkillSelection
        | upPressed && isGunHelpEntryRight             -> PauseMenuGunRightSelection
        | upPressed && isGunHelpEntryLeft              -> PauseMenuGunLeftSelection
        | upPressed && isWeaponHelpEntryRight          -> PauseMenuWeaponRightSelection
        | upPressed && isWeaponHelpEntryLeft           -> PauseMenuWeaponLeftSelection
        | upPressed || downPressed                     -> PauseMenuResumeSelection

    Just (PauseMenuUpgradeSelection 0)
        | leftPressed && isWeaponHelpEntryRight         -> PauseMenuWeaponRightSelection
        | leftPressed && isWeaponHelpEntryLeft          -> PauseMenuWeaponLeftSelection
        | leftPressed && isGunHelpEntryRight            -> PauseMenuGunRightSelection
        | leftPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | leftPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | leftPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | leftPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | leftPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | leftPressed || rightPressed                   -> PauseMenuGeneralInfoSelection
        | upPressed                                     -> PauseMenuSettingsSelection
        | downPressed && upgradeSelectionCount > 1      -> PauseMenuUpgradeSelection 1
        | downPressed                                   -> PauseMenuSettingsSelection

    Just (PauseMenuUpgradeSelection 1)
        | leftPressed && isGunHelpEntryRight            -> PauseMenuGunRightSelection
        | leftPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | leftPressed && isWeaponHelpEntryRight         -> PauseMenuWeaponRightSelection
        | leftPressed && isWeaponHelpEntryLeft          -> PauseMenuWeaponLeftSelection
        | leftPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | leftPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | leftPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | leftPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | leftPressed || rightPressed                   -> PauseMenuTargetingInfoSelection
        | upPressed                                     -> PauseMenuUpgradeSelection 0
        | downPressed && upgradeSelectionCount > 2      -> PauseMenuUpgradeSelection 2
        | downPressed                                   -> PauseMenuSettingsSelection

    Just (PauseMenuUpgradeSelection 2)
        | leftPressed && isMovementSkillHelpEntry       -> PauseMenuMovementSkillSelection
        | leftPressed && isSecondarySkillHelpEntryRight -> PauseMenuSecondarySkillRightSelection
        | leftPressed && isSecondarySkillHelpEntryMid   -> PauseMenuSecondarySkillMidSelection
        | leftPressed && isSecondarySkillHelpEntryLeft  -> PauseMenuSecondarySkillLeftSelection
        | leftPressed && isGunHelpEntryRight            -> PauseMenuGunRightSelection
        | leftPressed && isGunHelpEntryLeft             -> PauseMenuGunLeftSelection
        | leftPressed && isWeaponHelpEntryRight         -> PauseMenuWeaponRightSelection
        | leftPressed && isWeaponHelpEntryLeft          -> PauseMenuWeaponLeftSelection
        | leftPressed || rightPressed                   -> PauseMenuTauntingInfoSelection
        | upPressed                                     -> PauseMenuUpgradeSelection 1
        | downPressed                                   -> PauseMenuSettingsSelection

    Just selection -> selection

    where
        isWeaponHelpEntryLeft          = isJust $ pauseMenuHelpEntriesLeft (_weaponHelpEntries pauseMenuData)
        isWeaponHelpEntryRight         = isJust $ pauseMenuHelpEntriesRight (_weaponHelpEntries pauseMenuData)
        isGunHelpEntryLeft             = isJust $ pauseMenuHelpEntriesLeft (_gunHelpEntries pauseMenuData)
        isGunHelpEntryRight            = isJust $ pauseMenuHelpEntriesRight (_gunHelpEntries pauseMenuData)
        isMovementSkillHelpEntry       = isJust $ pauseMenuHelpEntriesLeft (_movementSkillHelpEntries pauseMenuData)
        isSecondarySkillHelpEntryLeft  = isJust $ _secondarySkillNeutralHelpEntry pauseMenuData
        isSecondarySkillHelpEntryMid   = isJust $ _secondarySkillUpHelpEntry pauseMenuData
        isSecondarySkillHelpEntryRight = isJust $ _secondarySkillDownHelpEntry pauseMenuData

        leftPressed           = MenuLeftAlias `aliasPressed` inputState
        rightPressed          = MenuRightAlias `aliasPressed` inputState
        upPressed             = MenuUpAlias `aliasPressed` inputState
        downPressed           = MenuDownAlias `aliasPressed` inputState
        upgradeSelectionCount = length $ _upgradeHelpEntries pauseMenuData

updateSelectionFromSlotComboBoxes :: Monad m => PauseMenuSlotComboBoxes -> StateT PauseMenuSelection m ()
updateSelectionFromSlotComboBoxes slotComboBoxes
    | _isExpanded neutralSlot                   = put PauseMenuSecondarySkillLeftSelection
    | _isExpanded upSlot                        = put PauseMenuSecondarySkillMidSelection
    | _isExpanded downSlot                      = put PauseMenuSecondarySkillRightSelection
    | comboBoxValue neutralSlot == upSlotText   = put PauseMenuSecondarySkillMidSelection
    | comboBoxValue neutralSlot == downSlotText = put PauseMenuSecondarySkillRightSelection
    | comboBoxValue upSlot == neutralSlotText   = put PauseMenuSecondarySkillLeftSelection
    | comboBoxValue upSlot == downSlotText      = put PauseMenuSecondarySkillRightSelection
    | comboBoxValue downSlot == neutralSlotText = put PauseMenuSecondarySkillLeftSelection
    | comboBoxValue downSlot == upSlotText      = put PauseMenuSecondarySkillMidSelection
    | otherwise                                 = return ()
    where
        neutralSlot = _neutralSlot slotComboBoxes
        upSlot      = _upSlot slotComboBoxes
        downSlot    = _downSlot slotComboBoxes
