module Enemy.All.Flying.Projectile
    ( mkFlyingProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs.All.Enemy.Flying
import Constants
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

projHitEffectPath =
    PackResourceFilePath "data/enemies/flying-enemy.pack" "attack-projectile-hit.spr" :: PackResourceFilePath
projHitSoundPath  = "event:/SFX Events/Enemy/Flying/attack-projectile-hit"            :: FilePath

data FlyingProjectileData = FlyingProjectileData
    { _attack :: Attack
    }

mkFlyingProjectile :: MonadIO m => Attack -> Pos2 -> FlyingEnemyConfig -> m (Some Projectile)
mkFlyingProjectile attack playerPos cfg =
    let
        flyingProjData = FlyingProjectileData {_attack = attack}
        aimVec         = toVec2 . vecNormalize $ playerPos `vecSub` _pos (attack :: Attack)
        vel            = toVel2 $ aimVec `vecMul` _attackProjectileSpeed cfg
    in do
        msgId  <- newId
        let hbx = fromMaybe (DummyHitbox playerPos) (attackHitbox attack)
        return . Some $ (mkProjectile flyingProjData msgId hbx maxSecs)
            { _vel                  = vel
            , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
            , _update               = updateFlyingProjectile
            , _draw                 = drawFlyingProjectile
            , _processCollisions    = processCollisions
            }

updateFlyingProjectile :: Monad m => ProjectileUpdate FlyingProjectileData m
updateFlyingProjectile flyingProjectile = return $ flyingProjectile
    { _data   = flyingProjData'
    , _hitbox = const $ fromMaybe (DummyHitbox pos') (attackHitbox attack')
    , _ttl    = ttl
    }
    where
        flyingProjData = _data flyingProjectile
        attack         = _attack (flyingProjData :: FlyingProjectileData)
        pos            = _pos (attack :: Attack)
        vel            = P._vel flyingProjectile
        pos'           = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        dir            = _dir (attack :: Attack)

        attack'         = updateAttack pos' dir attack
        ttl             = if _done attack' then 0.0 else _ttl flyingProjectile
        flyingProjData' = flyingProjData {_attack = attack'} :: FlyingProjectileData

processCollisions :: ProjectileProcessCollisions FlyingProjectileData
processCollisions collisions flyingProjectile = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player -> playerCollision player flyingProjectile
        _                          -> []

playerCollision :: CollisionEntity e => e -> Projectile FlyingProjectileData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player flyingProjectile =
    [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
    , mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsgTo (ProjectileMsgSetTtl 0.0) flyingProjId
    , mkMsg $ AudioMsgPlaySound projHitSoundPath pos
    ]
        where
            playerId      = collisionEntityMsgId player
            atk           = _attack (_data flyingProjectile :: FlyingProjectileData)
            atkHit        = mkAttackHit atk
            pos           = _pos (atk :: Attack)
            dir           = _dir (atk :: Attack)
            mkHitEffect = loadSimpleParticle pos dir enemyAttackProjectileZIndex projHitEffectPath
            flyingProjId  = P._msgId flyingProjectile

drawFlyingProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw FlyingProjectileData m
drawFlyingProjectile flyingProjectile =
    let
        attack = _attack (_data flyingProjectile :: FlyingProjectileData)
        spr    = attackSprite attack
        pos    = _pos (attack :: Attack)
        vel    = P._vel flyingProjectile
        dir    = _dir (attack :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr
