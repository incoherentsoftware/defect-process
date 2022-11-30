module Player.Gun.All.ShardGun.Shot
    ( mkShotLine
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as S

import Attack.Hit
import Attack.Util
import Collision
import Configs
import Configs.All.PlayerGun.ShardGun
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Id
import Msg
import Particle.All.AttackSpecks.Types
import Particle.All.Simple
import Player
import Player.Gun.All.ShardGun.Data
import Player.Gun.All.ShardGun.Shard
import Player.Gun.All.ShardGun.Util
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

debugHitboxColor = Color 255 0 0 255 :: Color

shardHitSoundFilePath = "event:/SFX Events/Player/Guns/shard-hit" :: FilePath

packPath             = \f -> PackResourceFilePath "data/player/player-guns.pack" f
shardMissEffectPath  = packPath "shard-miss-effect.spr"       :: PackResourceFilePath
shardWhiffEffectPath = shardMissEffectPath                    :: PackResourceFilePath
shardHitEffectPath   = packPath "shard-impale-hit-effect.spr" :: PackResourceFilePath

mkShotLine :: MonadIO m => ShardGunData -> Player -> m (Some Projectile)
mkShotLine shardGunData player =
    let
        cfg           = _config (shardGunData :: ShardGunData)
        pos           = playerShoulderPos player `vecAdd` _shotStartShoulderOffset cfg
        targetPos     = playerAimTarget player (_shootRange cfg)
        hbx           = lineHitbox pos targetPos
        shotAliveSecs = _shotAliveSecs cfg
    in do
        msgId <- newId
        return . Some $ (mkProjectile shardGunData msgId hbx shotAliveSecs)
            { _ownerId              = _gunMsgId (shardGunData :: ShardGunData)
            , _registeredCollisions = S.fromList
                [ ProjRegisteredEnemyCollision
                , ProjRegisteredSurfaceCollision
                , ProjRegisteredRoomItemCollision
                ]
            , _think                = thinkShot targetPos
            , _draw                 = drawShotLine
            , _processCollisions    = processShotCollisions
            }

thinkShot :: Monad m => Pos2 -> ProjectileThink ShardGunData m
thinkShot targetPos shot
    | P._ttl shot - timeStep <= 0.0 =
        let mkWhiffEffect = loadSimpleParticle targetPos RightDir worldEffectZIndex shardWhiffEffectPath
        in return [mkMsg $ ParticleMsgAddM mkWhiffEffect]
    | otherwise                     = return []

processShotCollisions :: ProjectileProcessCollisions ShardGunData
processShotCollisions projCollisions shot = processCollisions $ sortProjectileCollisions projCollisions shot
    where
        processCollisions :: [(Pos2, ProjectileCollision)] -> [Msg ThinkCollisionMsgsPhase]
        processCollisions []                                     = []
        processCollisions ((intersectPos, collision):collisions) = case collision of
            -- shot stops at first thing hit
            ProjEnemyCollision enemy              -> shotEntityCollision enemy shot intersectPos
            ProjSurfaceCollision _ _              -> shotSurfaceCollision shot intersectPos
            ProjRoomItemCollision (Some roomItem) -> shotEntityCollision roomItem shot intersectPos
            _                                     -> processCollisions collisions

shotSurfaceCollision :: Projectile ShardGunData -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
shotSurfaceCollision shot intersectPos =
    [ mkMsgTo (ProjectileMsgSetHitbox hitbox) shotId
    , mkMsgTo ProjectileMsgRemoveCollision shotId
    , mkMsgTo ProjectileMsgRemoveThink shotId
    , mkMsg $ ParticleMsgAddM mkMissParticle
    ]
        where
            shotId         = P._msgId shot
            startPos       = hitboxStartVertex $ projectileHitbox shot
            hitbox         = lineHitbox startPos intersectPos
            mkMissParticle = loadSimpleParticle intersectPos RightDir playerAttackEffectZIndex shardMissEffectPath

shotEntityCollision :: CollisionEntity e => e -> Projectile ShardGunData -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
shotEntityCollision entity shot intersectPos =
    [ mkMsgTo (ProjectileMsgSetHitbox hitbox) shotId
    , mkMsg $ NewUpdateProjectileMsgAddM (mkShard intersectPos shot entity)
    , mkMsgTo ProjectileMsgRemoveCollision shotId
    , mkMsgTo ProjectileMsgRemoveThink shotId
    , mkMsgTo (HurtMsgAttackHit shotHit) entityId
    , mkMsg $ ParticleMsgAddM mkShotHitEffect
    , mkMsg $ AudioMsgPlaySoundUnique shardHitSoundFilePath hashedShotId intersectPos
    ]
        where
            startPos     = hitboxStartVertex $ projectileHitbox shot
            hitbox       = lineHitbox startPos intersectPos
            shotId       = P._msgId shot
            hashedShotId = hashId shotId
            entityId     = collisionEntityMsgId entity
            cfg          = _config (P._data shot :: ShardGunData)

            shotDamage = _shotDamage cfg
            shotHit    = (mkAttackHitEmpty shotId intersectPos)
                { _damage            = shotDamage
                , _stagger           = damageToStagger shotDamage
                , _isRanged          = True
                , _hitEffectType     = WeakHitEffect
                , _specksType        = Just ShardSpecksType
                , _specksDirection   = Just SpecksAnyDir
                }

            hitEffectPos    = calculateShardImpalePos intersectPos entity shot cfg
            mkShotHitEffect = loadSimpleParticle hitEffectPos RightDir playerAttackEffectZIndex shardHitEffectPath

drawShotLine :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw ShardGunData m
drawShotLine shot =
    whenM (readSettingsConfig _debug _drawEntityHitboxes) $
        let points = hitboxVertices $ projectileHitbox shot
        in drawLines points debugHitboxColor debugHitboxZIndex
