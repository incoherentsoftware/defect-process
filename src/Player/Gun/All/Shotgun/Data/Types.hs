module Player.Gun.All.Shotgun.Data.Types
    ( ShotgunStatus(..)
    , ShotgunData(..)
    ) where

import Configs.All.PlayerGun.Shotgun
import Msg
import Player.Gun.MuzzleFlash.Types

data ShotgunStatus
    = ReadyStatus
    | AfterFireStatus
    | PumpStatus
    deriving Eq

data ShotgunData = ShotgunData
    { _status            :: ShotgunStatus
    , _cooldown          :: Float
    , _burnShotOwnerId   :: MsgId
    , _normalMuzzleFlash :: MuzzleFlash
    , _burnMuzzleFlash   :: MuzzleFlash
    , _config            :: ShotgunConfig
    }
