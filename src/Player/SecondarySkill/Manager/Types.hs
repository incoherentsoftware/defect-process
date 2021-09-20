module Player.SecondarySkill.Manager.Types
    ( SecondarySkillManager(..)
    ) where

import Player.SecondarySkill.Types
import Util

data SecondarySkillManager = SecondarySkillManager
    { _neutralSlot :: Maybe (Some SecondarySkill)
    , _upSlot      :: Maybe (Some SecondarySkill)
    , _downSlot    :: Maybe (Some SecondarySkill)
    }
