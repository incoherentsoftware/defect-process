module Player.Weapon.All
    ( module Player.Weapon.All.Gauntlets
    , module Player.Weapon.All.Scythe
    , module Player.Weapon.All.SpiritBlade
    , module Player.Weapon.All.Staff
    , module Player.Weapon.All.Sword
    , allWeaponTypes
    , mkWeaponFromType
    ) where

import AppEnv
import Player.Weapon.All.Gauntlets
import Player.Weapon.All.Scythe
import Player.Weapon.All.SpiritBlade
import Player.Weapon.All.Staff
import Player.Weapon.All.Sword
import Player.Weapon.Types
import Util

allWeaponTypes = [minBound..] :: [WeaponType]

mkWeaponFromType :: WeaponType -> AppEnv p (Some Weapon)
mkWeaponFromType = \case
    SwordWeapon       -> mkSwordWeapon
    GauntletsWeapon   -> mkGauntletsWeapon
    ScytheWeapon      -> mkScytheWeapon
    StaffWeapon       -> mkStaffWeapon
    SpiritBladeWeapon -> mkSpiritBladeWeapon
