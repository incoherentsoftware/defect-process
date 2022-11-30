module Player.Gun.All
    ( module Player.Gun.All.Revolver
    , module Player.Gun.All.RicochetGun
    , module Player.Gun.All.Shotgun
    , module Player.Gun.All.ShardGun
    , module Player.Gun.All.GrenadeLauncher
    , module Player.Gun.All.SpikeGun
    , allGunTypes
    , mkGunFromType
    ) where

import AppEnv
import Player.Gun
import Player.Gun.All.GrenadeLauncher
import Player.Gun.All.Revolver
import Player.Gun.All.RicochetGun
import Player.Gun.All.ShardGun
import Player.Gun.All.Shotgun
import Player.Gun.All.SpikeGun
import Util

allGunTypes = [minBound..] :: [GunType]

mkGunFromType :: GunType -> AppEnv p (Some Gun)
mkGunFromType gunType = case gunType of
    RevolverGun        -> mkRevolverGun
    ShotgunGun         -> mkShotgunGun
    ShardGun           -> mkShardGun
    GrenadeLauncherGun -> mkGrenadeLauncherGun
    SpikeGun           -> mkSpikeGun
    RicochetGun        -> mkRicochetGun
