module Player.Gun.All.Revolver.Shot
    ( mkShotProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
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
import Particle.All.AttackSpecks.Types
import Particle.All.Simple
import Player
import Player.Gun.All.Revolver.Util
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

gunsPack = \p -> PackResourceFilePath "data/player/player-guns.pack" p

missEffectPaths = NE.fromList
    [ gunsPack "bullet-miss-effect-a.spr"
    , gunsPack "bullet-miss-effect-b.spr"
    , gunsPack "bullet-miss-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

whiffEffectPaths = NE.fromList
    [ gunsPack "bullet-whiff-effect-a.spr"
    , gunsPack "bullet-whiff-effect-b.spr"
    , gunsPack "bullet-whiff-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

hitEffectPaths = NE.fromList
    [ gunsPack "bullet-hit-effect-a.spr"
    , gunsPack "bullet-hit-effect-b.spr"
    , gunsPack "bullet-hit-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

enemyHitSoundPath   = "event:/SFX Events/Player/Guns/gun-hit"         :: FilePath
surfaceHitSoundPath = "event:/SFX Events/Player/Guns/gun-surface-hit" :: FilePath

debugHitboxColor = Color 155 155 155 255 :: Color

data ShotData = ShotData
    { _type   :: RevolverShotType
    , _config :: RevolverConfig
    }

mkShotProjectile :: MonadIO m => RevolverShotType -> Player -> RevolverConfig -> m (Some Projectile)
mkShotProjectile shotType player cfg =
    let
        plId          = _msgId (player :: Player)
        pos           = playerShoulderPos player
        targetPos     = playerAimTarget player (_shootRange cfg)
        shotAliveSecs = _shotAliveSecs cfg
    in do
        msgId <- newId
        let
            shotData = ShotData
                { _type   = shotType
                , _config = cfg
                }
            hbx      = lineHitbox pos targetPos

        return . Some $ (mkProjectile shotData msgId hbx shotAliveSecs)
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

thinkShot :: Monad m => Pos2 -> ProjectileThink ShotData m
thinkShot targetPos shot = return $ if
    | P._ttl shot - timeStep <= 0.0 ->
        let
            mkWhiffEffect = do
                whiffEffectPath <- randomChoice whiffEffectPaths
                loadSimpleParticle targetPos RightDir worldEffectZIndex whiffEffectPath
        in [mkMsg $ ParticleMsgAddM mkWhiffEffect]
    | otherwise                     -> []

processShotCollisions :: ProjectileProcessCollisions ShotData
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

shotSurfaceCollision :: Hitbox -> Projectile ShotData -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
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

shotEntityCollision :: CollisionEntity e => e -> Projectile ShotData -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
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
            mkImpactEffect           = do
                hitEffectPath <- randomChoice hitEffectPaths
                loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle hitEffectPath

            shotData   = P._data shot
            cfg        = _config (shotData :: ShotData)
            shotDamage = case _type shotData of
                RevolverNormalShotType     -> _normalShotDamage cfg
                RevolverContinuousShotType -> _continuousShotDamage cfg

            shotHit = (mkAttackHitEmpty shotId intersectPos)
                { _vel               = _shotHitVel cfg
                , _damage            = shotDamage
                , _stagger           = _shotStagger cfg
                , _hitstunMultiplier = _shotHitstunMultiplier cfg
                , _isRanged          = True
                , _specksType        = Just BulletSpecksType
                , _specksDirection   = Just SpecksAnyDir
                }

drawShotLine :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw ShotData m
drawShotLine projectile = whenM (readSettingsConfig _debug _drawEntityHitboxes) $
    let points = hitboxVertices $ projectileHitbox projectile
    in drawLines points debugHitboxColor debugHitboxZIndex
