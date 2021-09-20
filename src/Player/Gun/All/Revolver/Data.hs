module Player.Gun.All.Revolver.Data
    ( RevolverStatus(..)
    , RevolverData(..)
    , mkRevolverData
    , decreaseRevolverDataCooldown
    ) where

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.Revolver
import Constants

data RevolverStatus
    = ReadyStatus
    | BetweenShotsStatus
    | ReloadStatus
    deriving (Eq, Show)

data RevolverData = RevolverData
    { _status   :: RevolverStatus
    , _bullets  :: Int
    , _cooldown :: Float
    , _config   :: RevolverConfig
    }

mkRevolverData :: ConfigsRead m => m RevolverData
mkRevolverData = do
    cfg <- readConfig _playerGun _revolver
    return $ RevolverData
        { _status   = ReadyStatus
        , _bullets  = _maxBullets cfg
        , _cooldown = 0.0
        , _config   = cfg
        }

decreaseRevolverDataCooldown :: RevolverData -> RevolverData
decreaseRevolverDataCooldown revolverData = revolverData {_cooldown = cooldown'}
    where cooldown' = _cooldown revolverData - timeStep
