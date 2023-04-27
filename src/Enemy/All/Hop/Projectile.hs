module Enemy.All.Hop.Projectile
    ( mkHopProjectile
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Hop
import Constants
import Enemy as E
import Enemy.All.Hop.AttackDescriptions
import Enemy.All.Hop.Behavior
import Enemy.All.Hop.Data
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

projHitEffectPath =
    PackResourceFilePath "data/enemies/hop-enemy.pack" "attack-projectile-hit.spr" :: PackResourceFilePath
projHitSoundPath  = "event:/SFX Events/Enemy/Hop/attack-projectile-hit"            :: FilePath

data HopProjectileData = HopProjectileData
    { _attack :: Attack
    }

mkHopProjectile :: (ConfigsRead m, MonadIO m) => Enemy HopEnemyData -> m (Some Projectile)
mkHopProjectile enemy =
    let
        enemyPos  = E._pos enemy
        dir       = E._dir enemy
        enemyData = E._data enemy
        cfg       = _hop $ _config enemyData

        releaseProjOffset  = case _behavior enemyData of
            AttackHopLongBehavior      -> _hopLongReleaseProjOffset cfg
            AttackHopShortLandBehavior -> _hopShortLandReleaseProjOffset cfg
            _                          -> zeroPos2
        releaseProjOffset' = releaseProjOffset `vecFlip` dir

        pos     = enemyPos `vecAdd` releaseProjOffset'
        atkDesc = _attackProj $ _attackDescs enemyData
    in do
        atk <- mkEnemyAttack pos dir atkDesc (enemyTauntedStatus enemy)
        let
            vel         = attackVelToVel2 (attackVel atk) zeroVel2
            hopProjData = HopProjectileData {_attack = atk}

        msgId  <- newId
        let hbx = fromMaybe (DummyHitbox pos) (attackHitbox atk)
        return . Some $ (mkProjectile hopProjData msgId hbx maxSecs)
            { _vel                  = vel
            , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
            , _update               = updateHopProjectile
            , _draw                 = drawHopProjectile
            , _processCollisions    = processCollisions
            , _voluntaryClear       = voluntaryClearData
            }

updateHopProjectile :: MsgsWrite UpdateProjectileMsgsPhase m => ProjectileUpdate HopProjectileData m
updateHopProjectile hopProj =
    let
        hopProjData = P._data hopProj
        atk         = _attack (hopProjData :: HopProjectileData)
        pos         = _pos (atk :: Attack)
        vel         = attackVelToVel2 (attackVel atk) zeroVel2
        pos'        = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        dir         = _dir (atk :: Attack)

        atk'         = updateAttack pos' dir atk
        ttl          = if _done atk' then 0.0 else _ttl hopProj
        hopProjData' = hopProjData {_attack = atk'} :: HopProjectileData
    in do
        when (ttl > 0.0) $
            writeMsgs $ attackSoundMessages atk'

        return $ hopProj
            { _data   = hopProjData'
            , _vel    = vel
            , _hitbox = const $ fromMaybe (DummyHitbox pos') (attackHitbox atk')
            , _ttl    = ttl
            }

processCollisions :: ProjectileProcessCollisions HopProjectileData
processCollisions collisions hopProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player -> playerCollision player hopProj
        _                          -> []

playerCollision :: CollisionEntity e => e -> Projectile HopProjectileData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player hopProj =
    [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
    , mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsgTo (ProjectileMsgSetTtl 0.0) hopProjId
    , mkMsg $ AudioMsgPlaySound projHitSoundPath pos
    ]
        where
            playerId      = collisionEntityMsgId player
            atk           = _attack (P._data hopProj :: HopProjectileData)
            atkHit        = mkAttackHit atk
            pos           = _pos (atk :: Attack)
            dir           = _dir (atk :: Attack)
            mkHitEffect = loadSimpleParticle pos dir enemyAttackProjectileZIndex projHitEffectPath
            hopProjId     = P._msgId hopProj

drawHopProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw HopProjectileData m
drawHopProjectile hopProj =
    let
        attack = _attack (P._data hopProj :: HopProjectileData)
        spr    = attackSprite attack
        pos    = _pos (attack :: Attack)
        vel    = P._vel hopProj
        dir    = _dir (attack :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr

voluntaryClearData :: ProjectileVoluntaryClear HopProjectileData
voluntaryClearData hopProj = case attackImage atk of
    Nothing  -> Nothing
    Just img -> Just $ ProjectileVoluntaryClearData
        { _pos    = _pos (atk :: Attack)
        , _dir    = _dir (atk :: Attack)
        , _zIndex = enemyAttackProjectileZIndex
        , _image  = img
        }
    where atk = _attack (P._data hopProj :: HopProjectileData)
