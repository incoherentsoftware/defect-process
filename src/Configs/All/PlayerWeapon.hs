module Configs.All.PlayerWeapon
    ( PlayerWeaponConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Configs.All.PlayerWeapon.Gauntlets
import Configs.All.PlayerWeapon.Scythe
import Configs.All.PlayerWeapon.Staff
import Configs.All.PlayerWeapon.Sword
import Util

data PlayerWeaponConfig = PlayerWeaponConfig
    { _sword     :: SwordConfig
    , _gauntlets :: GauntletsConfig
    , _scythe    :: ScytheConfig
    , _staff     :: StaffConfig
    }
    deriving Generic

instance FromJSON PlayerWeaponConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
