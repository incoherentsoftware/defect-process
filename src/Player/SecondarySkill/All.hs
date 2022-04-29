module Player.SecondarySkill.All
    ( module Player.SecondarySkill.All.StoneFormSkill
    , allSecondarySkillTypes
    , mkSecondarySkillFromType
    ) where

import AppEnv
import Player.SecondarySkill
import Player.SecondarySkill.All.StoneFormSkill
import Util

allSecondarySkillTypes = [minBound..] :: [SecondarySkillType]

-- NOTE: this is modified from the full source since only stoneForm is included in this repo
mkSecondarySkillFromType :: SecondarySkillType -> AppEnv p (Some SecondarySkill)
mkSecondarySkillFromType = \case
    StoneFormSkill      -> mkStoneFormSkill
    FlightSkill         -> mkStoneFormSkill
    FastFallSkill       -> mkStoneFormSkill
    StasisBlastSkill    -> mkStoneFormSkill
    MarkRecallSkill     -> mkStoneFormSkill
    SummonPlatformSkill -> mkStoneFormSkill
