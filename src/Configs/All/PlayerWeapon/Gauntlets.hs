module Configs.All.PlayerWeapon.Gauntlets
    ( GauntletsConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Map as M

import Util

data GauntletsConfig = GauntletsConfig
    { _evasiveKickProjectileOffset      :: Pos2
    , _allowReleasableAttacksWithoutHit :: Bool
    , _useChargeFlashIndicator          :: Bool
    , _chargeOverlaySprOffset           :: Pos2
    , _chargeOverlaySprOffsetMap        :: M.Map String [Pos2]
    }
    deriving Generic

instance FromJSON GauntletsConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
