module Player.Gun.All.SpikeGun.SpikeAlt
    ( mkSpikeAlt
    , mkSpikeAltEx
    , spikeAltUpdatePosMsg
    , spikeAltShootMsgs
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.SpikeGun
import Constants
import FileCache
import Id
import Msg
import Particle
import Particle.All.AttackSpecks
import Particle.All.Simple
import Player.Gun as G
import Player.Gun.All.SpikeGun.Data
import Player.Util
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex
import {-# SOURCE #-} Player.Gun.All.SpikeGun.SpikeRing

packPath                   = \f -> PackResourceFilePath "data/player/player-guns.pack" f
spikeAltShatterEffectPaths = NE.fromList $ map packPath
    [ "spike-alt-shatter-a.spr"
    , "spike-alt-shatter-b.spr"
    , "spike-alt-shatter-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath
spikeAltHitEffectPaths     = NE.fromList $ map packPath
    [ "spike-hit-effect-a.spr"
    , "spike-hit-effect-b.spr"
    , "spike-hit-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

spikeAltReleaseSoundPath  = "event:/SFX Events/Player/Guns/spike-release" :: FilePath
spikeAltHitSoundFilePath  = "event:/SFX Events/Player/Guns/spike-hit"     :: FilePath
spikeAltMissSoundFilePath = "event:/SFX Events/Player/Guns/spike-miss"    :: FilePath

spikeAltHitboxLength   = 57.0 :: Float
spikeAltShootAliveSecs = 2.0  :: Secs

isDrawDebug      = False             :: Bool
debugHitboxColor = Color 255 0 0 255 :: Color

data SpikeAltData = SpikeAltData
    { _angle       :: Radians
    , _angleOffset :: Radians
    , _atkMsgId    :: MsgId
    , _lerpOffset  :: Pos2
    , _sprite      :: Sprite
    , _config      :: SpikeGunConfig
    }

spikeAltHitbox :: Pos2 -> Radians -> SpikeGunConfig -> Hitbox
spikeAltHitbox ringPos angle cfg = lineHitbox startPos endPos
    where
        vec      = Vec2 (cos angle) (sin angle)
        offset   = toPos2 $ vec `vecMul` _spikeAltOffset cfg
        startPos = ringPos `vecAdd` offset
        endPos   = startPos `vecAdd` (toPos2 $ vec `vecMul` spikeAltHitboxLength)

mkSpikeAlt :: (ConfigsRead m, MonadIO m) => Pos2 -> Radians -> Radians -> SpikeGunSprites -> m (Some Projectile)
mkSpikeAlt ringPos ringAngle angleOffset sprs = mkSpikeAltEx ringPos ringAngle angleOffset sprs =<< newId

mkSpikeAltEx
    :: (ConfigsRead m, MonadIO m)
    => Pos2
    -> Radians
    -> Radians
    -> SpikeGunSprites
    -> MsgId
    -> m (Some Projectile)
mkSpikeAltEx ringPos ringAngle angleOffset sprs msgId = do
    atkMsgId <- newId
    cfg      <- readConfig _playerGun _spikeGun

    let
        spikeAltAngle = ringAngle + angleOffset
        hbx           = spikeAltHitbox ringPos spikeAltAngle cfg

        spikeAltData = SpikeAltData
            { _angle       = spikeAltAngle
            , _angleOffset = angleOffset
            , _atkMsgId    = atkMsgId
            , _lerpOffset  = zeroPos2
            , _sprite      = _spikeAlt sprs
            , _config      = cfg
            }

    return . Some $ (mkProjectile spikeAltData msgId hbx maxSecs)
        { _update               = updateSpikeAlt
        , _draw                 = drawSpikeAlt
        , _registeredCollisions = S.fromList
            [ ProjRegisteredEnemyCollision
            , ProjRegisteredRoomItemCollision
            ]
        , _processCollisions    = processSpikeAltCollisions
        }

processSpikeAltCollisions :: ProjectileProcessCollisions SpikeAltData
processSpikeAltCollisions collisions spikeAlt = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjEnemyCollision enemy              -> spikeAltEntityCollision enemy spikeAlt
        ProjSurfaceCollision _ _              -> spikeAltSurfaceCollision spikeAlt
        ProjRoomItemCollision (Some roomItem) -> spikeAltEntityCollision roomItem spikeAlt
        _                                     -> []

mkSpikeAltShatter :: (FileCache m, GraphicsRead m, MonadIO m) => Projectile SpikeAltData -> m (Some Particle)
mkSpikeAltShatter spikeAlt =
    let
        pos          = hitboxStartVertex $ projectileHitbox spikeAlt
        spikeAltData = P._data spikeAlt
        angle        = _angle (spikeAltData :: SpikeAltData)
    in do
        spikeAltShatterEffectPath <- randomChoice spikeAltShatterEffectPaths
        loadSimpleParticleRotated pos RightDir worldEffectZIndex angle spikeAltShatterEffectPath

updateSpikeRingOnReleaseMsg :: MsgId -> Msg ThinkCollisionMsgsPhase
updateSpikeRingOnReleaseMsg msgId = mkMsg $ PlayerMsgUpdateGun update
    where
        update = \g ->
            let
                gData  = G._data g
                gRing  = _ring gData
                gRing' = gRing
                    { _ringSpikeAltAngleOffsets =
                        M.filterWithKey (\saId _ -> saId /= msgId) (_ringSpikeAltAngleOffsets gRing)
                    }
            in g {G._data = gData {_ring = gRing'}}

spikeAltSurfaceCollision :: Projectile SpikeAltData -> [Msg ThinkCollisionMsgsPhase]
spikeAltSurfaceCollision spikeAlt =
    [ mkMsgTo (ProjectileMsgSetTtl 0.0) spikeAltId
    , mkMsg $ ParticleMsgAddM (mkSpikeAltShatter spikeAlt)
    , updateSpikeRingOnReleaseMsg spikeAltId
    , mkMsg $ AudioMsgPlaySound spikeAltMissSoundFilePath spikeAltCenterPos
    ]
        where
            spikeAltId        = P._msgId spikeAlt
            spikeAltCenterPos = hitboxCenter $ projectileHitbox spikeAlt

spikeAltAttackHit :: Pos2 -> Projectile SpikeAltData -> AttackHit
spikeAltAttackHit intersectPos spikeAlt = (mkAttackHitEmpty atkMsgId intersectPos)
    { _hitbox          = Just $ projectileHitbox spikeAlt
    , _damage          = spikeAltDmg
    , _stagger         = damageToStagger spikeAltDmg
    , _isRanged        = True
    , _specksType      = Just SpikeSpecksType
    , _specksDirection = Just SpecksAnyDir
    }
    where
        spikeAltData = P._data spikeAlt
        atkMsgId     = _atkMsgId spikeAltData
        spikeAltDmg  = _spikeAltDamage $ _config (spikeAltData :: SpikeAltData)

spikeAltEntityCollision :: CollisionEntity e => e -> Projectile SpikeAltData -> [Msg ThinkCollisionMsgsPhase]
spikeAltEntityCollision enemy spikeAlt =
    [ mkMsgTo (HurtMsgAttackHit spikeAltAtkHit) enemyId
    , mkMsgTo (ProjectileMsgSetTtl 0.0) spikeAltId
    , mkMsg $ ParticleMsgAddM (mkSpikeAltShatter spikeAlt)
    , mkMsg $ ParticleMsgAddM mkSpikeAltHitEffect
    , updateSpikeRingOnReleaseMsg spikeAltId
    , mkMsg $ AudioMsgPlaySound spikeAltHitSoundFilePath spikeAltCenterPos
    ]
        where
            enemyId           = collisionEntityMsgId enemy
            spikeAltId        = P._msgId spikeAlt
            spikeAltCenterPos = hitboxCenter $ projectileHitbox spikeAlt
            spikeAltAtkHit    = spikeAltAttackHit spikeAltCenterPos spikeAlt

            mkSpikeAltHitEffect = do
                hitEffectPath <- randomChoice spikeAltHitEffectPaths
                loadSimpleParticle spikeAltCenterPos RightDir playerAttackEffectZIndex hitEffectPath

updateSpikeAlt :: (GraphicsRead m, MonadIO m, MsgsRead UpdateProjectileMsgsPhase m) => ProjectileUpdate SpikeAltData m
updateSpikeAlt spikeAlt = do
    lerpOffset <- readPlayerLerpOffset
    return $ spikeAlt
        { P._data = (P._data spikeAlt) {_lerpOffset = lerpOffset}
        }

drawSpikeAlt :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw SpikeAltData m
drawSpikeAlt spikeAlt =
    let
        spikeAltData  = P._data spikeAlt
        spikeAltAngle = _angle (spikeAltData :: SpikeAltData)
        lerpOffset    = _lerpOffset spikeAltData
        spikeAltPos   = hitboxStartVertex (projectileHitbox spikeAlt) `vecAdd` lerpOffset
        spr           = _sprite (spikeAltData :: SpikeAltData)
    in do
        drawSpriteRotated spikeAltPos RightDir playerGunOverlayZIndex spikeAltAngle spr

        when isDrawDebug $
            let endPos = hitboxEndVertex (projectileHitbox spikeAlt) `vecAdd` lerpOffset
            in drawLine spikeAltPos endPos debugHitboxColor playerGunOverlayZIndex

spikeAltUpdatePosMsg :: Pos2 -> Radians -> MsgId -> Msg ThinkPlayerMsgsPhase
spikeAltUpdatePosMsg ringPos ringAngle msgId = mkMsgTo (ProjectileMsgUpdate update) msgId
    where
        update = \spikeAlt ->
            let
                spikeAltData = P._data spikeAlt
                angle        = ringAngle + _angleOffset spikeAltData
                cfg          = _config (spikeAltData :: SpikeAltData)
            in spikeAlt
                { P._data   = spikeAltData {_angle = angle} :: SpikeAltData
                , P._hitbox = const $ spikeAltHitbox ringPos angle cfg
                }

updateSpikeAltShoot :: Monad m => ProjectileUpdate SpikeAltData m
updateSpikeAltShoot spikeAlt = return $ spikeAlt {P._hitbox = const hbx'}
    where
        hbx       = projectileHitbox spikeAlt
        vel       = P._vel spikeAlt
        posOffset = toPos2 $ vel `vecMul` timeStep
        hbx'      = moveHitbox posOffset hbx

spikeAltShootMsgs :: Pos2 -> MsgId -> [Msg ThinkPlayerMsgsPhase]
spikeAltShootMsgs playerPos msgId =
    [ mkMsgTo (ProjectileMsgUpdate update) msgId
    , mkMsg $ AudioMsgPlaySound spikeAltReleaseSoundPath playerPos
    ]
        where
            update = \spikeAlt ->
                let
                    spikeAltData = P._data spikeAlt
                    angle        = _angle (spikeAltData :: SpikeAltData)
                    vec          = Vec2 (cos angle) (sin angle)
                    cfg          = _config (spikeAltData :: SpikeAltData)
                    vel          = toVel2 $ vec `vecMul` _spikeAltSpeed cfg
                in spikeAlt
                    { _data                 = spikeAltData {_lerpOffset = zeroPos2}
                    , _vel                  = vel
                    , _ttl                  = spikeAltShootAliveSecs
                    , _update               = updateSpikeAltShoot
                    , _registeredCollisions = S.fromList
                        [ ProjRegisteredEnemyCollision
                        , ProjRegisteredSurfaceCollision
                        , ProjRegisteredRoomItemCollision
                        ]
                    }
