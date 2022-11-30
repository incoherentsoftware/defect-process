module Enemy.All.Wall.WallProjectile
    ( mkWallProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs.All.Enemy
import Configs.All.Enemy.Wall
import Constants
import Enemy.All.Wall.AttackDescriptions
import Enemy.All.Wall.Data
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

projHitEffectPath =
    PackResourceFilePath "data/enemies/wall-enemy.pack" "attack-projectile-hit.spr"       :: PackResourceFilePath
projDisappearPath =
    PackResourceFilePath "data/enemies/wall-enemy.pack" "attack-projectile-disappear.spr" :: PackResourceFilePath
projHitSoundPath  = "event:/SFX Events/Enemy/Wall/attack-projectile-hit"                  :: FilePath

data WallProjectileData = WallProjectileData
    { _attack :: Attack
    }

mkWallProjectile :: MonadIO m => Pos2 -> Direction -> WallEnemyData -> m (Some Projectile)
mkWallProjectile enemyPos dir enemyData =
    let
        releaseWallProjOffset  = _releaseWallProjOffset $ _wall (_config enemyData)
        releaseWallProjOffset' = releaseWallProjOffset `vecFlip` dir
        pos                    = enemyPos `vecAdd` releaseWallProjOffset'
        atkDesc                = _wallProj $ _attackDescs enemyData
    in do
        atk <- mkAttack pos dir atkDesc
        let
            vel          = attackVelToVel2 (attackVel atk) zeroVel2
            wallProjData = WallProjectileData {_attack = atk}

        msgId  <- newId
        let hbx = fromMaybe (DummyHitbox pos) (attackHitbox atk)
        return . Some $ (mkProjectile wallProjData msgId hbx maxSecs)
            { _vel                  = vel
            , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
            , _update               = updateWallProjectile
            , _draw                 = drawWallProjectile
            , _processCollisions    = processCollisions
            }

updateWallProjectile :: MsgsWrite UpdateProjectileMsgsPhase m => ProjectileUpdate WallProjectileData m
updateWallProjectile wallProj =
    let
        wallProjData = _data wallProj
        atk          = _attack (wallProjData :: WallProjectileData)
        pos          = _pos (atk :: Attack)
        vel          = attackVelToVel2 (attackVel atk) zeroVel2
        pos'         = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        dir          = _dir (atk :: Attack)

        atk'          = updateAttack pos' dir atk
        atkDone       = _done atk'
        ttl           = if atkDone then 0.0 else _ttl wallProj
        wallProjData' = wallProjData {_attack = atk'} :: WallProjectileData
    in do
        if
            | atkDone   ->
                let mkParticle = loadSimpleParticleWithVel pos dir vel enemyAttackProjectileZIndex projDisappearPath
                in writeMsgs [mkMsg $ ParticleMsgAddM mkParticle]
            | otherwise -> writeMsgs $ attackSoundMessages atk

        return $ wallProj
            { _data   = wallProjData'
            , _vel    = vel
            , _hitbox = const $ fromMaybe (DummyHitbox pos') (attackHitbox atk')
            , _ttl    = ttl
            }

processCollisions :: ProjectileProcessCollisions WallProjectileData
processCollisions collisions wallProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player -> playerCollision player wallProj
        _                          -> []

playerCollision :: CollisionEntity e => e -> Projectile WallProjectileData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player wallProj =
    [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
    , mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsgTo (ProjectileMsgSetTtl 0.0) wallProjId
    , mkMsg $ AudioMsgPlaySound projHitSoundPath pos
    ]
        where
            playerId    = collisionEntityMsgId player
            atk         = _attack (_data wallProj :: WallProjectileData)
            atkHit      = mkAttackHit atk
            pos         = _pos (atk :: Attack)
            dir         = _dir (atk :: Attack)
            mkHitEffect = loadSimpleParticle pos dir enemyAttackProjectileZIndex projHitEffectPath
            wallProjId  = P._msgId wallProj

drawWallProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw WallProjectileData m
drawWallProjectile wallProj =
    let
        attack = _attack (_data wallProj :: WallProjectileData)
        spr    = attackSprite attack
        pos    = _pos (attack :: Attack)
        vel    = P._vel wallProj
        dir    = _dir (attack :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr
