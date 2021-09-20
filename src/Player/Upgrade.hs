module Player.Upgrade
    ( PlayerUpgradeType(..)
    ) where

import qualified Data.Text as T

import Util

data PlayerUpgradeType
    = DoubleJumpUpgradeType
    | MovementSkillUpgradeType
    | MeterUpgradeType
    deriving (Bounded, Enum, Eq, Ord, Show)

instance PrettyShow PlayerUpgradeType where
    prettyShow :: PlayerUpgradeType -> T.Text
    prettyShow = \case
        DoubleJumpUpgradeType    -> "+1 Double Jump Use"
        MovementSkillUpgradeType -> "+1 Primary Skill Use"
        MeterUpgradeType         -> "+1 Meter"
