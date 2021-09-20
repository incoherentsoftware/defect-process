module Player.EquipmentInfo.Types
    ( PlayerEquipmentInfo(..)
    ) where

import qualified Data.Map as M

import Player.Gun.Types
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Upgrade
import Player.Weapon.Types

data PlayerEquipmentInfo = PlayerEquipmentInfo
    { _weaponTypes               :: [WeaponType]
    , _gunTypes                  :: [GunType]
    , _movementSkillTypes        :: [MovementSkillType]
    , _secondarySkillNeutralType :: Maybe SecondarySkillType
    , _secondarySkillUpType      :: Maybe SecondarySkillType
    , _secondarySkillDownType    :: Maybe SecondarySkillType
    , _upgradeCounts             :: M.Map PlayerUpgradeType Int
    }
    deriving Eq
