module Player.BufferedInputState.Types
    ( PlayerInput(..)
    , PlayerInputMap
    , PlayerTapInput
    , PlayerBufferedInputState(..)
    ) where

import qualified Data.Map as M

import Configs.All.Settings.Input
import Util

type PlayerInputMap = M.Map PlayerInput Secs
type PlayerTapInput = (PlayerInput, Secs)

data PlayerInput
    = LeftInput
    | RightInput
    | DownInput
    | DownLeftInput
    | DownRightInput
    | JumpInput
    | JumpDownInput
    | WeaponInput
    | WeaponUpInput
    | WeaponDownInput
    | WeaponHoldInput
    | WeaponReleaseInput
    | WeaponReleaseUpInput
    | WeaponReleaseDownInput
    | ShootInput
    | ShootUpInput
    | ShootDownInput
    | ShootHoldInput
    | ShootReleaseInput
    | ShootReleaseUpInput
    | ShootReleaseDownInput
    | MovementSkillInput
    | SecondarySkillNeutralInput
    | SecondarySkillUpInput
    | SecondarySkillDownInput
    | SwitchWeaponInput
    | SwitchGunInput
    | LockOnCursorInput
    | LockOnClearInput
    | LockOnSwitchTargetInput
    deriving (Eq, Ord, Show)

data PlayerBufferedInputState = PlayerBufferedInputState
    { _inputMap                        :: PlayerInputMap
    , _tapInputs                       :: [PlayerTapInput]
    , _lastDir                         :: Direction
    , _lastHeldStraightDownElapsedSecs :: Secs
    , _config                          :: InputConfig
    }
