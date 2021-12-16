module Player.Gun.All
    ( module Player.Gun.All.Revolver
    , allGunTypes
    , mkGunFromType
    ) where

import AppEnv
import Player.Gun
import Player.Gun.All.Revolver
import Util

allGunTypes = [minBound..] :: [GunType]

-- NOTE: this is modified from the full source since only revolver is included in this repo
mkGunFromType :: GunType -> AppEnv p (Some Gun)
mkGunFromType gunType = case gunType of
    RevolverGun        -> mkRevolverGun
    ShotgunGun         -> mkRevolverGun
    ShardGun           -> mkRevolverGun
    GrenadeLauncherGun -> mkRevolverGun
    SpikeGun           -> mkRevolverGun
    RicochetGun        -> mkRevolverGun
