module Player.Gun.All.SpikeGun.SpikeBarrage.Spike
    ( mkSpike
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.PlayerGun.SpikeGun
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Id
import Msg
import Particle
import Particle.All.Simple
import Player.Gun.All.SpikeGun.SpikeBarrage.Data
import Player.Gun.All.SpikeGun.Util
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath               = \f -> PackResourceFilePath "data/player/player-guns.pack" f
spikeShatterEffectPath = packPath "spike-shatter.spr"               :: PackResourceFilePath
spikeHitEffectPath     = packPath "spike-hit-effect.spr"            :: PackResourceFilePath
spikeHitSoundFilePath  = "event:/SFX Events/Player/Guns/spike-hit"  :: FilePath
spikeMissSoundFilePath = "event:/SFX Events/Player/Guns/spike-miss" :: FilePath

spikeLength = 82.0 :: Float

debugHitboxColor = Color 255 0 0 155 :: Color

data SpikeData = SpikeData
    { _angle       :: Radians
    , _spikeSpr    :: Sprite
    , _spikeAttack :: Attack
    }

mkSpikeData :: SpikeBarrageData -> Pos2 -> Pos2 -> Attack -> SpikeData
mkSpikeData spikeBarrageData spikePos targetPos spikeAtk = SpikeData
    { _angle       = angle
    , _spikeSpr    = spikeSpr
    , _spikeAttack = spikeAtk
    }
    where
        angle    = calculateSpikeBarrageSpikeAngle spikePos targetPos
        spikeSpr = _spikeSpr (spikeBarrageData :: SpikeBarrageData)

mkSpikeHitbox :: Pos2 -> Pos2 -> Hitbox
mkSpikeHitbox pos targetPos = lineHitbox pos endPos
    where
        targetVec = vecNormalize $ targetPos `vecSub` pos
        endPos    = pos `vecAdd` (targetVec `vecMul` spikeLength)

mkSpike :: (ConfigsRead m, MonadIO m) => Projectile SpikeBarrageData -> m (Some Projectile)
mkSpike spikeBarrage = do
    playerCfg <- _player <$> readConfigs
    let
        shoulderPos           = calculateSpikeBarrageSpikePos spikeBarrage playerCfg
        spikeBarrageData      = P._data spikeBarrage
        dir                   = _dir (spikeBarrageData :: SpikeBarrageData)
        targetPos             = _targetPos spikeBarrageData
        atkDesc               = _spikeAtkDesc spikeBarrageData
        targetVec             = vecNormalize $ targetPos `vecSub` shoulderPos
        cfg                   = _config (spikeBarrageData :: SpikeBarrageData)
        barrageSpikeSpeed     = _barrageSpikeSpeed cfg
        barrageSpikeAliveSecs = _barrageSpikeAliveSecs cfg

    msgId <- newId
    atk   <- mkAttack shoulderPos dir atkDesc

    let
        hbx       = mkSpikeHitbox shoulderPos targetPos
        spikeData = mkSpikeData spikeBarrageData shoulderPos targetPos atk
        vel       = toVel2 $ targetVec `vecMul` barrageSpikeSpeed

    return . Some $ (mkProjectile spikeData msgId hbx barrageSpikeAliveSecs)
        { _vel                  = vel
        , _update               = updateSpike
        , _draw                 = drawSpike
        , _registeredCollisions = S.fromList
            [ ProjRegisteredEnemyCollision
            , ProjRegisteredSurfaceCollision
            , ProjRegisteredRoomItemCollision
            ]
        , _processCollisions    = processSpikeCollisions
        }

mkSpikeShatter :: (FileCache m, GraphicsRead m, MonadIO m) => Projectile SpikeData -> m (Some Particle)
mkSpikeShatter spike = loadSimpleParticleRotated pos RightDir worldEffectZIndex angle spikeShatterEffectPath
    where
        pos       = hitboxStartVertex $ projectileHitbox spike
        spikeData = P._data spike
        angle     = _angle (spikeData :: SpikeData)

processSpikeCollisions :: ProjectileProcessCollisions SpikeData
processSpikeCollisions collisions spike = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjEnemyCollision enemy              -> spikeEntityCollision enemy spike
        ProjSurfaceCollision _ _              -> spikeSurfaceCollision spike
        ProjRoomItemCollision (Some roomItem) -> spikeEntityCollision roomItem spike
        _                                     -> []

spikeSurfaceCollision :: Projectile SpikeData -> [Msg ThinkCollisionMsgsPhase]
spikeSurfaceCollision spike =
    [ mkMsgTo (ProjectileMsgSetTtl 0.0) spikeId
    , mkMsg $ ParticleMsgAddM (mkSpikeShatter spike)
    , mkMsg $ AudioMsgPlaySound spikeMissSoundFilePath spikeCenterPos
    ]
        where
            spikeId        = P._msgId spike
            spikeCenterPos = hitboxCenter $ projectileHitbox spike

spikeEntityCollision :: CollisionEntity e => e -> Projectile SpikeData -> [Msg ThinkCollisionMsgsPhase]
spikeEntityCollision enemy spike =
    [ mkMsgTo (HurtMsgAttackHit spikeAtkHit) enemyId
    , mkMsgTo (ProjectileMsgSetTtl 0.0) spikeId
    , mkMsg $ ParticleMsgAddM (mkSpikeShatter spike)
    , mkMsg $ ParticleMsgAddM mkSpikeHitEffect
    , mkMsg $ AudioMsgPlaySound spikeHitSoundFilePath spikeCenterPos
    ]
        where
            enemyId          = collisionEntityMsgId enemy
            spikeId          = P._msgId spike
            spikeAtk         = _spikeAttack $ P._data spike
            spikeCenterPos   = hitboxCenter $ projectileHitbox spike
            spikeAtkHit      = mkAttackHitEx spikeCenterPos spikeAtk
            mkSpikeHitEffect = loadSimpleParticle spikeCenterPos RightDir playerAttackEffectZIndex spikeHitEffectPath

updateSpike :: Monad m => ProjectileUpdate SpikeData m
updateSpike spike = return $ spike {P._hitbox = const hitbox'}
    where
        vel     = P._vel spike
        hitbox  = projectileHitbox spike
        hitbox' = moveHitbox (toPos2 $ vel `vecMul` timeStep) hitbox

drawSpike :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw SpikeData m
drawSpike spike =
    let
        pos       = hitboxStartVertex $ projectileHitbox spike
        vel       = P._vel spike
        hbx       = (P._hitbox spike) spike
        spikeData = P._data spike
        angle     = _angle (spikeData :: SpikeData)
        spr       = _spikeSpr (spikeData :: SpikeData)
    in do
        whenM (readSettingsConfig _debug _drawEntityHitboxes) $
            drawHitbox debugHitboxColor debugHitboxZIndex hbx

        pos' <- graphicsLerpPos pos vel
        drawSpriteRotated pos' RightDir worldEffectZIndex angle spr
