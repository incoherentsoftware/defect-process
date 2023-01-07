module Player.Gun.All.SpikeGun.Util
    ( calculateSpikeBarragePos
    , calculateSpikeBarrageSpikePos
    , calculateSpikeBarrageSpikeAngle
    , mkSpikeGunDematerializeParticle
    , mkSpikeGunBarrageSpikeAppearParticle
    ) where

import Control.Monad.IO.Class (MonadIO)

import Collision.Hitbox
import Configs
import Configs.All.Player
import Configs.All.PlayerGun.SpikeGun
import FileCache
import Particle
import Particle.All.Simple
import Player.AimBody
import Player.Gun.All.SpikeGun.Data
import Player.Gun.All.SpikeGun.SpikeBarrage.Data
import Player.Types
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath                    = \f -> PackResourceFilePath "data/player/player-guns.pack" f
spikesDematerialize5SprPath = packPath "spikes-dematerialize5-.spr" :: PackResourceFilePath
spikesDematerialize4SprPath = packPath "spikes-dematerialize4-.spr" :: PackResourceFilePath
spikesDematerialize3SprPath = packPath "spikes-dematerialize3-.spr" :: PackResourceFilePath
spikesDematerialize2SprPath = packPath "spikes-dematerialize2-.spr" :: PackResourceFilePath
spikesDematerialize1SprPath = packPath "spikes-dematerialize1-.spr" :: PackResourceFilePath
spikesAppearEffectSprPath   = packPath "spike-appear-effect.spr"    :: PackResourceFilePath

calculateSpikeBarragePos :: SpikeGunData -> Player -> Pos2
calculateSpikeBarragePos spikeGunData player = playerPos `vecAdd` summonOffset'
    where
        playerPos     = _pos (player :: Player)
        dir           = _dir (player :: Player)
        cfg           = _config (spikeGunData :: SpikeGunData)
        summonOffset' = vecFlip (_summonOffset cfg) dir

calculateSpikeBarrageSpikePos :: Projectile SpikeBarrageData -> PlayerConfig -> Pos2
calculateSpikeBarrageSpikePos spikeBarrage playerCfg = calculateShoulderPos playerCfg playerPos
    where
        spikeBarrageData = P._data spikeBarrage
        cfg              = _config (spikeBarrageData :: SpikeBarrageData)
        dir              = _dir (spikeBarrageData :: SpikeBarrageData)
        summonOffset     = vecFlip (_summonOffset cfg) dir
        spikeBarragePos  = hitboxTopLeft $ projectileHitbox spikeBarrage
        playerPos        = spikeBarragePos `vecSub` summonOffset

calculateSpikeBarrageSpikeAngle :: Pos2 -> Pos2 -> Radians
calculateSpikeBarrageSpikeAngle (Pos2 x y) (Pos2 targetX targetY) = atan2 (targetY - y) (targetX - x)

mkSpikeGunDematerializeParticle
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => SpikeGunData
    -> Player
    -> m (Some Particle)
mkSpikeGunDematerializeParticle spikeGunData player = loadSimpleParticle pos RightDir playerGunOverlayZIndex sprPath
    where
        player' = player {_dir = RightDir} :: Player
        pos     = calculateSpikeBarragePos spikeGunData player'
        sprPath = case _numSpikes (spikeGunData :: SpikeGunData) of
            5 -> spikesDematerialize5SprPath
            4 -> spikesDematerialize4SprPath
            3 -> spikesDematerialize3SprPath
            2 -> spikesDematerialize2SprPath
            _ -> spikesDematerialize1SprPath

mkSpikeGunBarrageSpikeAppearParticle
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => Projectile SpikeBarrageData
    -> m (Some Particle)
mkSpikeGunBarrageSpikeAppearParticle spikeBarrage = do
    playerCfg <- _player <$> readConfigs
    let
        spikePos         = calculateSpikeBarrageSpikePos spikeBarrage playerCfg
        spikeBarrageData = P._data spikeBarrage
        angle            = calculateSpikeBarrageSpikeAngle spikePos (_targetPos spikeBarrageData)
    loadSimpleParticleRotated spikePos RightDir worldEffectZIndex angle spikesAppearEffectSprPath
