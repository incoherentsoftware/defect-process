module Player.Weapon.Manager.Types
    ( WeaponManager(..)
    ) where

import Player.Weapon.Types
import Util

data WeaponManager = WeaponManager
    { _weapons :: [Some Weapon]
    }
