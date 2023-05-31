module Player.Gun.All.ShardGun.Shard
    ( mkShard
    , setShardExplode
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE

import Attack
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
import Player.Util
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath                = \f -> PackResourceFilePath "data/player/player-guns.pack" f
explosionHitEffectPaths = NE.fromList $ map packPath
    [ "shard-explosion-hit-effect-a.spr"
    , "shard-explosion-hit-effect-b.spr"
    , "shard-explosion-hit-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

shardExplodeSoundFilePath = "event:/SFX Events/Player/Guns/shard-explode" :: FilePath

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

setShardExplode :: Pos2 -> Projectile ShardData -> Projectile ShardData
setShardExplode (Pos2 playerX _) shard = shard {_think = thinkExplode}
    where
        thinkExplode :: Monad m => ProjectileThink ShardData m
        thinkExplode s = return
            [ mkMsg $ NewUpdateProjectileMsgAddM explosion
            , mkMsgTo (ProjectileMsgSetTtl 0.0) sId
            , mkMsg $ ParticleMsgAddM mkExplosionHitEffect
            , mkMsg $ AudioMsgPlaySound shardExplodeSoundFilePath pos
            ]
            where
                sId                    = P._msgId s
                pos@(Pos2 shardX _)    = hitboxTopLeft $ projectileHitbox s
                dir
                    | shardX < playerX = LeftDir
                    | otherwise        = RightDir

                explosionAtkDesc     = _explosionAtkDesc $ P._data shard
                explosion            = mkPlayerAttackProjectile pos dir explosionAtkDesc
                mkExplosionHitEffect = do
                    hitEffectPath <- randomChoice explosionHitEffectPaths
                    loadSimpleParticle pos dir playerAttackEffectZIndex hitEffectPath

updateShard :: MsgsRead UpdateProjectileMsgsPhase m => ProjectileUpdate ShardData m
updateShard shard = update <$> readMsgs <*> pure shard
    where
        update :: [InfoMsgPayload] -> Projectile ShardData -> Projectile ShardData
        update [] shardProj     = shardProj {_ttl = 0.0}  -- remove shard if enemy disappears
        update (d:ds) shardProj = case d of
            InfoMsgEnemyPos enHbx enId
                | enId == _enemyMsgId shardData ->
                    let
                        ttl
                            | isDummyHitbox enHbx = 0.0  -- remove shard if enemy phased (dummy hitbox)
                            | otherwise           = _ttl shardProj

                        hitbox         = projectileHitbox shardProj
                        hitboxTopLeft' = hitboxBotCenter enHbx `vecAdd` _enemyOffset shardData
                        hitbox'        = setHitboxTopLeft hitboxTopLeft' hitbox
                    in shardProj
                        { _ttl    = ttl
                        , _hitbox = const hitbox'
                        }
            _                                   -> update ds shardProj
            where shardData = P._data shard

drawShard :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw ShardData m
drawShard shard = drawImage shardPos RightDir worldProjectileZIndex impaleImg
    where
        shardPos  = hitboxTopLeft $ projectileHitbox shard
        shardData = P._data shard
        impaleImg = _impaleImage shardData
