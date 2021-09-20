module Player.SecondarySkill.Types
    ( SecondarySkillType
    ) where

import Data.Aeson.Types (FromJSON)

data SecondarySkillType
instance Ord SecondarySkillType
instance FromJSON SecondarySkillType
