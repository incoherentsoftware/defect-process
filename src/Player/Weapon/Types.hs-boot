module Player.Weapon.Types
    ( WeaponType
    ) where

import Data.Aeson.Types (FromJSON)

data WeaponType
instance FromJSON WeaponType
instance Ord WeaponType
