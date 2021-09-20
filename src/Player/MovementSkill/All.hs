module Player.MovementSkill.All
    ( module Player.MovementSkill.All.DashSkill
    , allMovementSkillTypes
    , mkMovementSkillFromType
    ) where

import AppEnv
import Player.MovementSkill
import Player.MovementSkill.All.DashSkill
import Util

allMovementSkillTypes = [minBound..] :: [MovementSkillType]

-- NOTE: this is modified from the full source since only dash is included in this repo
mkMovementSkillFromType :: MovementSkillType -> AppEnv p (Some MovementSkill)
mkMovementSkillFromType = \case
    DashSkill     -> mkDashSkill
    TeleportSkill -> mkDashSkill
    GrappleSkill  -> mkDashSkill
