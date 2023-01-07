module Player.Gun.Types
    ( GunType(..)
    , GunThink
    , GunUpdateDynamic
    , GunDrawOverlay
    , GunUpdate
    , Gun(..)
    ) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Dynamic     (Dynamic)
import GHC.Generics     (Generic)
import qualified Data.Text as T

import AppEnv.Types
import Msg.Types
import Player.Gun.DrawOverlayStatus
import Player.Gun.FireDrawData
import Player.Gun.Status
import Util
import {-# SOURCE #-} Player.Types

data GunType
    = RevolverGun
    | ShotgunGun
    | GrenadeLauncherGun
    | ShardGun
    | SpikeGun
    | RicochetGun
    deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance PrettyShow GunType where
    prettyShow :: GunType -> T.Text
    prettyShow = \case
        RevolverGun        -> "Revolver"
        ShotgunGun         -> "Shotgun"
        GrenadeLauncherGun -> "Grenade Launcher"
        ShardGun           -> "Shards"
        SpikeGun           -> "Spikes"
        RicochetGun        -> "Ricochet Beam"

type GunThink d m       = GunStatus -> Player -> Gun d -> m [Msg ThinkPlayerMsgsPhase]
type GunDrawOverlay d m = GunDrawOverlayStatus -> Player -> Gun d -> m ()
type GunUpdate d m      = Gun d -> m (Gun d)
type GunUpdateDynamic d = Dynamic -> Gun d -> Gun d

data Gun d = Gun
    { _data          :: d
    , _type          :: GunType
    , _fireDrawData  :: Maybe GunFireDrawData
    , _drawOverlay   :: GunDrawOverlay d (AppEnv DrawMsgsPhase)
    , _think         :: GunThink d (AppEnv ThinkPlayerMsgsPhase)
    , _update        :: GunUpdate d (AppEnv UpdatePlayerMsgsPhase)
    , _updateDynamic :: GunUpdateDynamic d
    }
