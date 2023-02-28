module Player.Gun.All.Shotgun.BurnShot
    ( mkBurnShotProjectiles
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

import Attack.Hit
import Attack.Util
import Collision
import Configs.All.PlayerGun.Shotgun
import Constants
import FileCache
import Id
import Msg
import Particle
import Particle.All.Simple
import Player
import Player.Gun.All.Shotgun.BurnFlame
import Projectile as P
import Util
import World.ZIndex

packPath         = \f -> PackResourceFilePath "data/player/player-guns.pack" f
missEffectPaths  = NE.fromList $ map packPath
    [ "bullet-miss-effect-a.spr"
    , "bullet-miss-effect-b.spr"
    , "bullet-miss-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath
whiffEffectPaths = NE.fromList $ map packPath
    [ "bullet-whiff-effect-a.spr"
    , "bullet-whiff-effect-b.spr"
    , "bullet-whiff-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath
hitEffectPaths   = NE.fromList $ map packPath
    [ "bullet-hit-effect-a.spr"
    , "bullet-hit-effect-b.spr"
    , "bullet-hit-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

enemyHitSoundPath   = "event:/SFX Events/Player/Guns/gun-hit"         :: FilePath
surfaceHitSoundPath = "event:/SFX Events/Player/Guns/gun-surface-hit" :: FilePath

firePos :: Pos2 -> Pos2 -> ShotgunConfig -> [(Pos2, Pos2)]
firePos pos targetPos cfg = zip (repeat pos) targetPosSpread
    where
        targetPos'      = targetPos `vecSub` pos
        shotgunSpread   = map toRadians (_shotgunSpreadDegrees cfg)
        targetPosSpread = flip map shotgunSpread $ \angle -> pos `vecAdd` vecRotate targetPos' angle

mkBurnShotProjectiles :: MonadIO m => Player -> MsgId -> ShotgunConfig -> m [Some Projectile]
mkBurnShotProjectiles player burnShotOwnerId cfg = traverse mkShotProjectile startEnds
    where
        targetPos    = playerAimTarget player (_shootRange cfg)
        shotStartPos = playerShoulderPos player `vecAdd`  _shotStartShoulderOffset cfg
        startEnds    = firePos shotStartPos targetPos cfg

        mkShotProjectile :: MonadIO m => (Pos2, Pos2) -> m (Some Projectile)
        mkShotProjectile (startPos, endPos) = do
            msgId  <- newId
            let hbx = lineHitbox startPos endPos

            return . Some $ (mkProjectile cfg msgId hbx (_shotAliveSecs cfg))
                { _ownerId              = burnShotOwnerId
                , _registeredCollisions = S.fromList
                    [ ProjRegisteredEnemyCollision
                    , ProjRegisteredSurfaceCollision
                    , ProjRegisteredRoomItemCollision
                    ]
                , _think                = thinkShot endPos
                , _processCollisions    = processShotCollisions
                }

thinkShot :: Monad m => Pos2 -> ProjectileThink ShotgunConfig m
thinkShot targetPos shot
    | P._ttl shot - timeStep <= 0.0 =
        let
            mkWhiffEffect = do
                whiffEffectPath <- randomChoice whiffEffectPaths
                loadSimpleParticle targetPos RightDir worldEffectZIndex whiffEffectPath
        in return [mkMsg $ ParticleMsgAddM mkWhiffEffect]
    | otherwise                     = return []

processShotCollisions :: ProjectileProcessCollisions ShotgunConfig
processShotCollisions projCollisions shot =
    processCollisions $ sortProjectileCollisions projCollisions shot
        where
            processCollisions :: [(Pos2, ProjectileCollision)] -> [Msg ThinkCollisionMsgsPhase]
            processCollisions []                                     = []
            processCollisions ((intersectPos, collision):collisions) = case collision of
                -- shot passes through enemies
                ProjEnemyCollision enemy              ->
                    shotEntityCollision enemy shot intersectPos ++ processCollisions'
                -- shot stops if it hits surface/room item
                ProjSurfaceCollision hbx _            -> shotSurfaceCollision hbx shot intersectPos
                ProjRoomItemCollision (Some roomItem) -> shotEntityCollision roomItem shot intersectPos
                _                                     -> processCollisions'
                where processCollisions' = processCollisions collisions

shotSurfaceCollision :: Hitbox -> Projectile ShotgunConfig -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
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
        mkMissEffect             = do
            missEffectPath <- randomChoice missEffectPaths
            loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle missEffectPath

shotEntityCollision :: CollisionEntity e => e -> Projectile ShotgunConfig -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
shotEntityCollision entity shot intersectPos =
    [ mkMsgTo (ProjectileMsgSetHitbox hitbox) shotId
    , mkMsgTo ProjectileMsgRemoveCollision shotId
    , mkMsgTo ProjectileMsgRemoveThink shotId
    , mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsgTo (HurtMsgAttackHit shotHit) entityId
    , mkMsg $ AudioMsgPlaySoundUnique enemyHitSoundPath (hashId shotId) intersectPos
    , mkMsg $ WorldMsgHitlag (_burnShotHitlag cfg)
    , mkMsg $ NewUpdateProjectileMsgAddM (mkBurnFlame shot entity)
    ]
    where
        shotId   = P._msgId shot
        startPos = hitboxStartVertex $ projectileHitbox shot
        hitbox   = lineHitbox startPos intersectPos
        cfg      = P._data shot
        entityId = collisionEntityMsgId entity

        entityHbx                = collisionEntityHitbox entity
        (effectDir, effectAngle) = particleClosestDirAngle intersectPos entityHbx
        mkHitEffect              = do
            hitEffectPath <- randomChoice hitEffectPaths
            loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle hitEffectPath

        shotDamage = _burnShotDamage cfg
        shotHit    = (mkAttackHitEmpty shotId intersectPos)
            { _damage   = shotDamage
            , _stagger  = damageToStagger shotDamage
            , _isRanged = True
            }
