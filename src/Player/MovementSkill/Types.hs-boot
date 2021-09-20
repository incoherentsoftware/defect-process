module Player.MovementSkill.Types
    ( MovementSkillType
    ) where

import Data.Aeson.Types (FromJSON)

data MovementSkillType
instance FromJSON MovementSkillType
instance Ord MovementSkillType
