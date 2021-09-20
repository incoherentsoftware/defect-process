module Particle.All.EnemyHurt
    ( enemyHurtPackPath
    , mkEnemyHurtParticle
    , mkEnemyHurtParticleEx
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Attack.Hit
import Collision.Hitbox
import Enemy
import FileCache
import Particle as P
import Util
import Window.Graphics
import World.ZIndex

fakeAliveSecs = 999.0 :: Secs

enemyHurtPackPath         = "data/particles/particles-enemy.pack"                        :: FilePath
enemyHurtParticlePath     = PackResourceFilePath enemyHurtPackPath "enemy-hurt.spr"      :: PackResourceFilePath
enemyHurtWeakParticlePath = PackResourceFilePath enemyHurtPackPath "enemy-hurt-weak.spr" :: PackResourceFilePath

data ParticleData = ParticleData
    { _dir    :: Direction
    , _scale  :: DrawScale
    , _sprite :: Sprite
    }

mkEnemyHurtParticle
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Enemy d
    -> AttackHit
    -> EnemyHurtEffectData
    -> m (Some Particle)
mkEnemyHurtParticle enemy atkHit hurtEffectData = mkEnemyHurtParticleEx enemy atkHit hurtEffectData hitEffectType
    where hitEffectType = _hitEffectType (atkHit :: AttackHit)

mkEnemyHurtParticleEx
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Enemy d
    -> AttackHit
    -> EnemyHurtEffectData
    -> AttackHitEffectType
    -> m (Some Particle)
mkEnemyHurtParticleEx enemy _ hurtEffectData hitEffectType = do
    spr <- loadPackSprite $ case hitEffectType of
        NormalHitEffect -> enemyHurtParticlePath
        StrongHitEffect -> enemyHurtParticlePath
        WeakHitEffect   -> enemyHurtWeakParticlePath

    let
        scale        = case hitEffectType of
            NormalHitEffect -> _drawScale (hurtEffectData :: EnemyHurtEffectData)
            StrongHitEffect -> _strongDrawScale hurtEffectData
            WeakHitEffect   -> _drawScale (hurtEffectData :: EnemyHurtEffectData)  -- reuse normal hit scale for now
        particleData = ParticleData
            { _dir    = RightDir
            , _scale  = scale
            , _sprite = spr
            }

        pos = hitboxCenter $ enemyHitbox enemy

    return . Some $ (mkParticle particleData pos fakeAliveSecs)
        { _draw   = draw
        , _update = update
        }

draw :: (GraphicsReadWrite m, MonadIO m) => ParticleDraw ParticleData m
draw particle = drawSpriteEx pos dir enemyHurtParticleZIndex angle FullOpacity scale spr
    where
        pos          = P._pos particle
        particleData = P._data particle
        dir          = _dir (particleData :: ParticleData)
        angle        = P._angle particle
        spr          = _sprite (particleData :: ParticleData)
        scale        = _scale (particleData :: ParticleData)

update :: ParticleUpdate ParticleData
update particle = particle
    { _data = particleData'
    , _ttl  = ttl
    }
    where
        particleData  = P._data particle
        spr           = updateSprite $ _sprite (particleData :: ParticleData)
        particleData' = particleData {_sprite = spr} :: ParticleData

        ttl
            | spriteFinished spr = 0.0
            | otherwise          = fakeAliveSecs
