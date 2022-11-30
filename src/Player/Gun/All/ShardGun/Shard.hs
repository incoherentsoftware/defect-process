module Player.Gun.All.ShardGun.Shard
    ( mkShard
    , setShardExplode
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Attack.Projectile
import Collision
import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.ShardGun
import FileCache
import Id
import Msg
import Particle.All.Simple
import Player.Gun.All.ShardGun.Data
import Player.Gun.All.ShardGun.Util
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

explosionHitEffectPath    =
    PackResourceFilePath "data/player/player-guns.pack" "shard-explosion-hit-effect.spr" :: PackResourceFilePath
shardExplodeSoundFilePath = "event:/SFX Events/Player/Guns/shard-explode"                :: FilePath

data ShardData = ShardData
    { _enemyMsgId       :: MsgId
    , _shotHitbox       :: Hitbox
    , _intersectPos     :: Pos2
    , _enemyOffset      :: Pos2
    , _impaleImage      :: Image
    , _explosionAtkDesc :: AttackDescription
    }

mkShardData :: CollisionEntity e => Pos2 -> e -> Projectile ShardGunData -> ShardGunConfig -> ShardData
mkShardData intersectPos enemy shot cfg = ShardData
    { _enemyMsgId       = collisionEntityMsgId enemy
    , _shotHitbox       = shotHbx
    , _intersectPos     = intersectPos'
    , _enemyOffset      = enemyOffset
    , _impaleImage      = _shardImpale $ _images (shardGunData :: ShardGunData)
    , _explosionAtkDesc = _shardExplosionAtkDesc $ _attackDescription shardGunData
    }
    where
        shotHbx       = projectileHitbox shot
        intersectPos' = calculateShardImpalePos intersectPos enemy shot cfg
        enemyPos      = hitboxBotCenter $ collisionEntityHitbox enemy
        enemyOffset   = intersectPos' `vecSub` enemyPos
        shardGunData  = P._data shot

mkShard
    :: (CollisionEntity e, ConfigsRead m, MonadIO m)
    => Pos2
    -> Projectile ShardGunData
    -> e
    -> m (Some Projectile)
mkShard intersectPos shot enemy = do
    cfg <- readConfig _playerGun _shardGun
    let
        shardData            = mkShardData intersectPos enemy shot cfg
        dummyHbx             = dummyHitbox $ _intersectPos shardData
        impaleShardAliveSecs = _impaleShardAliveSecs cfg

    msgId <- newId
    return . Some $ (mkProjectile shardData msgId dummyHbx impaleShardAliveSecs)
        { _ownerId = _ownerId shot
        , _update  = updateShard
        , _draw    = drawShard
        }

setShardExplode :: Projectile ShardData -> Projectile ShardData
setShardExplode shard = shard {_think = thinkExplode}
    where
        thinkExplode :: Monad m => ProjectileThink ShardData m
        thinkExplode s = return
            [ mkMsg $ NewUpdateProjectileMsgAddM explosion
            , mkMsgTo (ProjectileMsgSetTtl 0.0) sId
            , mkMsg $ ParticleMsgAddM mkExplosionHitEffect
            , mkMsg $ AudioMsgPlaySound shardExplodeSoundFilePath pos
            ]
            where
                sId                  = P._msgId s
                pos                  = hitboxTopLeft $ projectileHitbox s
                explosionAtkDesc     = _explosionAtkDesc $ P._data shard
                explosion            = mkPlayerAttackProjectile pos RightDir explosionAtkDesc
                mkExplosionHitEffect = loadSimpleParticle pos RightDir playerAttackEffectZIndex explosionHitEffectPath

updateShard :: MsgsRead UpdateProjectileMsgsPhase m => ProjectileUpdate ShardData m
updateShard shard = update <$> readMsgs <*> pure shard
    where
        update :: [InfoMsgPayload] -> Projectile ShardData -> Projectile ShardData
        update [] shardProj     = shardProj {_ttl = 0.0}  -- remove shard if enemy disappears
        update (d:ds) shardProj = case d of
            InfoMsgEnemyPos enHbx enId
                | enId == _enemyMsgId shardData ->
                    let
                        hitbox         = projectileHitbox shardProj
                        hitboxTopLeft' = hitboxBotCenter enHbx `vecAdd` _enemyOffset shardData
                        hitbox'        = setHitboxTopLeft hitboxTopLeft' hitbox
                    in shardProj {_hitbox = const hitbox'}
            _                                   -> update ds shardProj
            where shardData = P._data shard

drawShard :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw ShardData m
drawShard shard = drawImage shardPos RightDir worldProjectileZIndex impaleImg
    where
        shardPos  = hitboxTopLeft $ projectileHitbox shard
        shardData = P._data shard
        impaleImg = _impaleImage shardData
