module Player.SecondarySkill.All
    ( module Player.SecondarySkill.All.FastFallSkill
    , module Player.SecondarySkill.All.FlightSkill
    , module Player.SecondarySkill.All.MarkRecallSkill
    , module Player.SecondarySkill.All.StasisBlastSkill
    , module Player.SecondarySkill.All.StoneFormSkill
    , module Player.SecondarySkill.All.SummonPlatformSkill
    , allSecondarySkillTypes
    , mkSecondarySkillFromType
    ) where

import AppEnv
import Player.SecondarySkill
import Player.SecondarySkill.All.FastFallSkill
import Player.SecondarySkill.All.FlightSkill
import Player.SecondarySkill.All.MarkRecallSkill
import Player.SecondarySkill.All.StasisBlastSkill
import Player.SecondarySkill.All.StoneFormSkill
import Player.SecondarySkill.All.SummonPlatformSkill
import Util

allSecondarySkillTypes = [minBound..] :: [SecondarySkillType]

mkSecondarySkillFromType :: SecondarySkillType -> AppEnv p (Some SecondarySkill)
mkSecondarySkillFromType = \case
    StoneFormSkill      -> mkStoneFormSkill
    FlightSkill         -> mkFlightSkill
    FastFallSkill       -> mkFastFallSkill
    StasisBlastSkill    -> mkStasisBlastSkill
    MarkRecallSkill     -> mkMarkRecallSkill
    SummonPlatformSkill -> mkSummonPlatformSkill
