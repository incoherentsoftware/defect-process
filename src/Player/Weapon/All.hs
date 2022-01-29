module Player.Weapon.All
    ( module Player.Weapon.All.Sword
    , allWeaponTypes
    , mkWeaponFromType
    ) where

import AppEnv
import Player.Weapon.All.Sword
import Player.Weapon.Types
import Util

allWeaponTypes = [minBound..] :: [WeaponType]

-- NOTE: this is modified from the full source since only sword is included in this repo
mkWeaponFromType :: WeaponType -> AppEnv p (Some Weapon)
mkWeaponFromType = \case
    SwordWeapon       -> mkSwordWeapon
    GauntletsWeapon   -> mkSwordWeapon
    ScytheWeapon      -> mkSwordWeapon
    StaffWeapon       -> mkSwordWeapon
    SpiritBladeWeapon -> mkSwordWeapon
