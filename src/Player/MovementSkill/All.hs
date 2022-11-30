module Player.MovementSkill.All
    ( module Player.MovementSkill.All.DashSkill
    , module Player.MovementSkill.All.GrappleSkill
    , module Player.MovementSkill.All.TeleportSkill
    , allMovementSkillTypes
    , mkMovementSkillFromType
    ) where

import AppEnv
import Player.MovementSkill
import Player.MovementSkill.All.DashSkill
import Player.MovementSkill.All.GrappleSkill
import Player.MovementSkill.All.TeleportSkill
import Util

allMovementSkillTypes = [minBound..] :: [MovementSkillType]

mkMovementSkillFromType :: MovementSkillType -> AppEnv p (Some MovementSkill)
mkMovementSkillFromType = \case
    DashSkill     -> mkDashSkill
    TeleportSkill -> mkTeleportSkill
    GrappleSkill  -> mkGrappleSkill
