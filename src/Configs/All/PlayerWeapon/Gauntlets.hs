module Configs.All.PlayerWeapon.Gauntlets
    ( GauntletsConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data GauntletsConfig = GauntletsConfig
    { _evasiveKickProjectileOffset      :: Pos2
    , _allowReleasableAttacksWithoutHit :: Bool
    }
    deriving Generic

instance FromJSON GauntletsConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
