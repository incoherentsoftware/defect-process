module Enemy.All.Spear.Projectile
    ( mkSpearProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Spear
import Constants
import Enemy.All.Spear.AttackDescriptions
import Enemy.All.Spear.Data
import Enemy.TauntedData
import Enemy.Util
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

projHitEffectPath =
    PackResourceFilePath "data/enemies/spear-enemy.pack" "attack-projectile-hit.spr" :: PackResourceFilePath
projHitSoundPath  = "event:/SFX Events/Enemy/Spear/attack-projectile-hit"            :: FilePath

data SpearProjectileData = SpearProjectileData
    { _attack      :: Attack
    , _spearConfig :: SpearEnemyConfig
    }

mkSpearProjectileData :: Attack -> SpearEnemyConfig -> SpearProjectileData
mkSpearProjectileData atk spearCfg = SpearProjectileData
    { _attack      = atk
    , _spearConfig = spearCfg
    }

spearProjectileSurfaceHitbox :: Projectile SpearProjectileData -> Hitbox
spearProjectileSurfaceHitbox spearProj = rectHitbox pos width height
    where
        spearProjData = _data spearProj
        atk           = _attack spearProjData
        spearCfg      = _spearConfig spearProjData
        width         = _spearProjSurfaceWidth spearCfg
        height        = _spearProjSurfaceHeight spearCfg

        offset  = _spearProjSurfaceOffset spearCfg
        offset' = case _dir (atk :: Attack) of
            RightDir -> offset
            LeftDir  -> vecFlip offset LeftDir `vecAdd` Pos2 (-width) 0.0
        pos     = _pos (atk :: Attack) `vecAdd` offset'

spearProjectileSurface :: ProjectileSurface SpearProjectileData
spearProjectileSurface spearProj = Just $ mkPlatformSurface (spearProjectileSurfaceHitbox spearProj)

mkSpearProjectile
    :: (ConfigsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> SpearEnemyData
    -> EnemyTauntedStatus
    -> m (Some Projectile)
mkSpearProjectile enemyPos dir enemyData tauntedStatus =
    let
        spearCfg               = _spear $ _config enemyData
        releaseSpearProjOffset = _releaseSpearProjOffset spearCfg `vecFlip` dir
        pos                    = enemyPos `vecAdd` releaseSpearProjOffset
        atkDesc                = _spearProj $ _attackDescs enemyData
    in do
        atk <- mkEnemyAttack pos dir atkDesc tauntedStatus
        let
            vel           = attackVelToVel2 (attackVel atk) zeroVel2
            spearProjData = mkSpearProjectileData atk spearCfg

        msgId  <- newId
        let hbx = fromMaybe (DummyHitbox pos) (attackHitbox atk)
        return . Some $ (mkProjectile spearProjData msgId hbx maxSecs)
            { _vel                  = vel
            , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
            , _surface              = spearProjectileSurface
            , _think                = thinkSpearProjectile
            , _update               = updateSpearProjectile
            , _draw                 = drawSpearProjectile
            , _processCollisions    = processCollisions
            , _voluntaryClear       = voluntaryClearData
            }

thinkSpearProjectile :: Monad m => ProjectileThink SpearProjectileData m
thinkSpearProjectile spearProj = return $ movingPlatformMsg:thinkAttack atk
    where
        surfaceHbx          = spearProjectileSurfaceHitbox spearProj
        atk                 = _attack (_data spearProj :: SpearProjectileData)
        vel                 = attackVelToVel2 (attackVel atk) zeroVel2
        projectedOffset     = toPos2 $ vel `vecMul` timeStep
        projectedSurfaceHbx = moveHitbox projectedOffset surfaceHbx
        movingPlatformMsg   = mkMsg $ CollisionMsgMovingPlatform surfaceHbx projectedSurfaceHbx

updateSpearProjectile :: Monad m => ProjectileUpdate SpearProjectileData m
updateSpearProjectile spearProj = return $ spearProj
    { _data   = spearProjData'
    , _vel    = vel
    , _hitbox = const $ fromMaybe (DummyHitbox pos') (attackHitbox atk')
    , _ttl    = ttl
    }
    where
        spearProjData = _data spearProj
        atk           = _attack (spearProjData :: SpearProjectileData)
        pos           = _pos (atk :: Attack)
        vel           = attackVelToVel2 (attackVel atk) zeroVel2
        pos'          = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        dir           = _dir (atk :: Attack)

        atk'           = updateAttack pos' dir atk
        ttl            = if _done atk' then 0.0 else _ttl spearProj
        spearProjData' = spearProjData {_attack = atk'} :: SpearProjectileData

processCollisions :: ProjectileProcessCollisions SpearProjectileData
processCollisions collisions spearProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player -> playerCollision player spearProj
        _                          -> []

playerCollision :: CollisionEntity e => e -> Projectile SpearProjectileData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player spearProj =
    [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
    , mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsgTo (ProjectileMsgSetTtl 0.0) spearProjId
    , mkMsg $ AudioMsgPlaySound projHitSoundPath pos
    ]
        where
            playerId    = collisionEntityMsgId player
            atk         = _attack (_data spearProj :: SpearProjectileData)
            atkHit      = mkAttackHit atk
            pos         = _pos (atk :: Attack)
            dir         = _dir (atk :: Attack)
            mkHitEffect = loadSimpleParticle pos dir enemyAttackProjectileZIndex projHitEffectPath
            spearProjId = P._msgId spearProj

drawSpearProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw SpearProjectileData m
drawSpearProjectile spearProj =
    let
        attack = _attack (_data spearProj :: SpearProjectileData)
        spr    = attackSprite attack
        pos    = _pos (attack :: Attack)
        vel    = P._vel spearProj
        dir    = _dir (attack :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr

voluntaryClearData :: ProjectileVoluntaryClear SpearProjectileData
voluntaryClearData spearProj = case attackImage atk of
    Nothing  -> Nothing
    Just img -> Just $ ProjectileVoluntaryClearData
        { _pos    = _pos (atk :: Attack)
        , _dir    = _dir (atk :: Attack)
        , _zIndex = enemyAttackProjectileZIndex
        , _image  = img
        }
    where atk = _attack (P._data spearProj :: SpearProjectileData)
