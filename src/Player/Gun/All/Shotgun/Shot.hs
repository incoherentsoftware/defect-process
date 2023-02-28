module Player.Gun.All.Shotgun.Shot
    ( mkShotProjectiles
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Attack.Hit
import Attack.Util
import Collision
import Configs
import Configs.All.PlayerGun.Shotgun
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Id
import Msg
import Particle
import Particle.All.AttackSpecks.Types
import Particle.All.Simple
import Player
import Projectile as P
import Util
import Window.Graphics
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

hitEffectPaths = NE.fromList
    [ packPath "bullet-hit-effect-a.spr"
    , packPath "bullet-hit-effect-b.spr"
    , packPath "bullet-hit-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

enemyHitSoundPath   = "event:/SFX Events/Player/Guns/gun-hit"         :: FilePath
surfaceHitSoundPath = "event:/SFX Events/Player/Guns/gun-surface-hit" :: FilePath

rightUpwardsKnockbackRadians = toRadians 40.0  :: Radians
leftUpwardsKnockbackRadians  = toRadians 140.0 :: Radians

firePos :: Pos2 -> Pos2 -> ShotgunConfig -> [(Pos2, Pos2)]
firePos pos targetPos cfg = zip (repeat pos) targetPosSpread
    where
        targetPos'      = targetPos `vecSub` pos
        shotgunSpread   = map toRadians (_shotgunSpreadDegrees cfg)
        targetPosSpread = flip map shotgunSpread $ \angle -> pos `vecAdd` vecRotate targetPos' angle

adjustShotKnockbackVec :: Vec2 -> Vec2
adjustShotKnockbackVec knockbackVec
    | abs knockbackRad < rightUpwardsKnockbackRadians = unitVec (-rightUpwardsKnockbackRadians)
    | abs knockbackRad > leftUpwardsKnockbackRadians  = unitVec (-leftUpwardsKnockbackRadians)
    | otherwise                                       = knockbackVec
    where
        unitVec      = \rad -> Vec2 (cos rad) (sin rad)
        knockbackRad = atan2 (vecY knockbackVec) (vecX knockbackVec)

mkShotProjectiles :: MonadIO m => Player -> ShotgunConfig -> m [Some Projectile]
mkShotProjectiles player cfg = traverse mkShotProjectile startEnds
    where
        targetPos    = playerAimTarget player (_shootRange cfg)
        shotStartPos = playerShoulderPos player `vecAdd`  _shotStartShoulderOffset cfg
        startEnds    = firePos shotStartPos targetPos cfg

        mkShotProjectile :: MonadIO m => (Pos2, Pos2) -> m (Some Projectile)
        mkShotProjectile (startPos, endPos) = do
            msgId <- newId
            let
                hbx          = lineHitbox startPos endPos
                knockbackVec = adjustShotKnockbackVec $ toVec2 (targetPos `vecSub` shotStartPos)

            return . Some $ (mkProjectile cfg msgId hbx (_shotAliveSecs cfg))
                { _ownerId              = _msgId (player :: Player)
                , _registeredCollisions = S.fromList
                    [ ProjRegisteredEnemyCollision
                    , ProjRegisteredSurfaceCollision
                    , ProjRegisteredRoomItemCollision
                    ]
                , _think                = thinkShot endPos
                , _draw                 = drawShot
                , _processCollisions    = processShotCollisions knockbackVec
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

processShotCollisions :: Vec2 -> ProjectileProcessCollisions ShotgunConfig
processShotCollisions knockbackVec projCollisions shot =
    processCollisions $ sortProjectileCollisions projCollisions shot
        where
            processCollisions :: [(Pos2, ProjectileCollision)] -> [Msg ThinkCollisionMsgsPhase]
            processCollisions []                                     = []
            processCollisions ((intersectPos, collision):collisions) = case collision of
                -- shot passes through enemies
                ProjEnemyCollision enemy              ->
                    shotEntityCollision enemy shot intersectPos knockbackVec ++ processCollisions'
                -- shot stops if it hits surface/room item
                ProjSurfaceCollision hbx _            -> shotSurfaceCollision hbx shot intersectPos
                ProjRoomItemCollision (Some roomItem) -> shotEntityCollision roomItem shot intersectPos knockbackVec
                _                                     -> processCollisions'
                where processCollisions' = processCollisions collisions

hitKnockbackVel :: Vec2 -> Float -> ShotgunConfig -> Vel2
hitKnockbackVel knockbackVec dist cfg
    | dist <= _knockbackDistThreshold cfg = toVel2 $ (vecNormalize knockbackVec) `vecMul` _knockbackMagnitude cfg
    | otherwise                           = zeroVel2

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

shotEntityCollision
    :: CollisionEntity e
    => e
    -> Projectile ShotgunConfig
    -> Pos2
    -> Vec2
    -> [Msg ThinkCollisionMsgsPhase]
shotEntityCollision entity shot intersectPos knockbackVec =
    [ mkMsgTo (ProjectileMsgSetHitbox hitbox) shotId
    , mkMsgTo ProjectileMsgRemoveCollision shotId
    , mkMsgTo ProjectileMsgRemoveThink shotId
    , mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsgTo (HurtMsgAttackHit shotHit) entityId
    , mkMsg $ AudioMsgPlaySoundUnique enemyHitSoundPath (hashId shotId) intersectPos
    , mkMsg $ WorldMsgHitlag (_shotHitlag cfg)
    ]
    where
        shotId     = P._msgId shot
        startPos   = hitboxStartVertex $ projectileHitbox shot
        hitbox     = lineHitbox startPos intersectPos
        targetDist = vecDist intersectPos startPos
        cfg        = P._data shot
        hitVel     = hitKnockbackVel knockbackVec targetDist cfg
        entityId   = collisionEntityMsgId entity

        entityHbx                = collisionEntityHitbox entity
        (effectDir, effectAngle) = particleClosestDirAngle intersectPos entityHbx
        mkHitEffect              = do
            hitEffectPath <- randomChoice hitEffectPaths
            loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle hitEffectPath

        shotHitEffectType
            | targetDist <= _knockbackDistThreshold cfg = StrongHitEffect
            | otherwise                                 = WeakHitEffect

        shotDamage = _shotDamage cfg
        shotHit    = (mkAttackHitEmpty shotId intersectPos)
            { _vel             = hitVel
            , _damage          = shotDamage
            , _stagger         = damageToStagger shotDamage
            , _isRanged        = True
            , _hitEffectType   = shotHitEffectType
            , _specksType      = Just BulletSpecksType
            , _specksDirection = Just SpecksAnyDir
            }

drawShot :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw ShotgunConfig m
drawShot shot = whenM (readSettingsConfig _debug _drawEntityHitboxes) $
    let
        points           = hitboxVertices $ projectileHitbox shot
        debugHitboxColor = Color 0 155 0 255
    in drawLines points debugHitboxColor debugHitboxZIndex
