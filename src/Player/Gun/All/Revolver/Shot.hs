module Player.Gun.All.Revolver.Shot
    ( mkShotProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as S

import Attack.Hit
import Collision
import Configs
import Configs.All.PlayerGun.Revolver
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Id
import Msg
import Particle
import Particle.All.Simple
import Player
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

gunsPack        = \p -> PackResourceFilePath "data/player/player-guns.pack" p
missEffectPath  = gunsPack "bullet-miss-effect.spr"  :: PackResourceFilePath
hitEffectPath   = gunsPack "bullet-hit-effect.spr"   :: PackResourceFilePath
whiffEffectPath = gunsPack "bullet-whiff-effect.spr" :: PackResourceFilePath

enemyHitSoundPath   = "event:/SFX Events/Player/Guns/gun-hit"         :: FilePath
surfaceHitSoundPath = "event:/SFX Events/Player/Guns/gun-surface-hit" :: FilePath

debugHitboxColor = Color 155 155 155 255 :: Color

mkShotProjectile :: MonadIO m => Player -> RevolverConfig -> m (Some Projectile)
mkShotProjectile player cfg =
    let
        plId          = _msgId (player :: Player)
        pos           = playerShoulderPos player
        targetPos     = playerAimTarget player (_shootRange cfg)
        shotAliveSecs = _shotAliveSecs cfg
    in do
        msgId  <- newId
        let hbx = lineHitbox pos targetPos
        return . Some $ (mkProjectile cfg msgId hbx shotAliveSecs)
            { _ownerId              = plId
            , _registeredCollisions = S.fromList
                [ ProjRegisteredEnemyCollision
                , ProjRegisteredSurfaceCollision
                , ProjRegisteredRoomItemCollision
                ]
            , _think                = thinkShot targetPos
            , _draw                 = drawShotLine
            , _processCollisions    = processShotCollisions
            }

thinkShot :: Monad m => Pos2 -> ProjectileThink RevolverConfig m
thinkShot targetPos shot
    | P._ttl shot - timeStep <= 0.0 =
        let mkWhiffEffect = loadSimpleParticle targetPos RightDir worldEffectZIndex whiffEffectPath
        in return [mkMsg $ ParticleMsgAddM mkWhiffEffect]
    | otherwise                     = return []

processShotCollisions :: ProjectileProcessCollisions RevolverConfig
processShotCollisions projCollisions shot = processCollisions $ sortProjectileCollisions projCollisions shot
    where
        processCollisions :: [(Pos2, ProjectileCollision)] -> [Msg ThinkCollisionMsgsPhase]
        processCollisions []                                     = []
        processCollisions ((intersectPos, collision):collisions) = case collision of
            -- shot stops at first thing hit
            ProjEnemyCollision enemy              -> shotEntityCollision enemy shot intersectPos
            ProjSurfaceCollision hbx _            -> shotSurfaceCollision hbx shot intersectPos
            ProjRoomItemCollision (Some roomItem) -> shotEntityCollision roomItem shot intersectPos
            _                                     -> processCollisions collisions

shotSurfaceCollision :: Hitbox -> Projectile RevolverConfig -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
shotSurfaceCollision nonEnemyHbx shot intersectPos =
    [ mkMsgTo (ProjectileMsgSetHitbox hitbox) shotId
    , mkMsgTo ProjectileMsgRemoveCollision shotId
    , mkMsgTo ProjectileMsgRemoveThink shotId
    , mkMsg $ ParticleMsgAddM mkMissEffect
    , mkMsg $ AudioMsgPlaySound surfaceHitSoundPath intersectPos
    ]
        where
            shotId   = P._msgId shot
            startPos = hitboxStartVertex $ projectileHitbox shot
            hitbox   = lineHitbox startPos intersectPos

            (effectDir, effectAngle) = particleClosestDirAngle intersectPos nonEnemyHbx
            mkMissEffect             =
                loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle missEffectPath

shotEntityCollision :: CollisionEntity e => e -> Projectile RevolverConfig -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
shotEntityCollision entity shot intersectPos =
    [ mkMsgTo (ProjectileMsgSetHitbox hitbox) shotId
    , mkMsgTo ProjectileMsgRemoveCollision shotId
    , mkMsgTo ProjectileMsgRemoveThink shotId
    , mkMsg $ ParticleMsgAddM mkImpactEffect
    , mkMsgTo (HurtMsgAttackHit shotHit) entityId
    , mkMsg $ AudioMsgPlaySoundUnique enemyHitSoundPath hashedShotId intersectPos
    ]
        where
            shotId       = P._msgId shot
            startPos     = hitboxStartVertex $ projectileHitbox shot
            hitbox       = lineHitbox startPos intersectPos
            hashedShotId = hashId shotId
            entityId     = collisionEntityMsgId entity

            entityHbx                = collisionEntityHitbox entity
            (effectDir, effectAngle) = particleClosestDirAngle intersectPos entityHbx
            mkImpactEffect           =
                loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle hitEffectPath

            cfg     = P._data shot
            shotHit = (mkAttackHitEmpty shotId intersectPos)
                { _vel               = _shotHitVel cfg
                , _damage            = _shotDamage cfg
                , _stagger           = _shotStagger cfg
                , _hitstunMultiplier = _shotHitstunMultiplier cfg
                , _isRanged          = True
                }

drawShotLine :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw RevolverConfig m
drawShotLine projectile = whenM (readSettingsConfig _debug _drawEntityHitboxes) $
    let points = hitboxVertices $ projectileHitbox projectile
    in drawLines points debugHitboxColor debugHitboxZIndex
