module Menu.PauseMenu.Selection.Types
    ( PauseMenuSelection(..)
    ) where

data PauseMenuSelection
    = PauseMenuResumeSelection
    | PauseMenuMainSelection
    | PauseMenuSettingsSelection
    | PauseMenuGeneralInfoSelection
    | PauseMenuTargetingInfoSelection
    | PauseMenuWeaponLeftSelection
    | PauseMenuWeaponRightSelection
    | PauseMenuGunLeftSelection
    | PauseMenuGunRightSelection
    | PauseMenuMovementSkillSelection
    | PauseMenuSecondarySkillLeftSelection
    | PauseMenuSecondarySkillMidSelection
    | PauseMenuSecondarySkillRightSelection
    | PauseMenuUpgradeSelection Int
    deriving (Eq, Ord)
