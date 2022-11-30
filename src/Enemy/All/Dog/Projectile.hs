module Enemy.All.Dog.Projectile
    ( mkDogProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Dog
import Constants
import Enemy as E
import Enemy.All.Dog.AttackDescriptions
import Enemy.All.Dog.Data
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

projHitEffectPath =
    PackResourceFilePath "data/enemies/dog-enemy.pack" "attack-projectile-hit.spr" :: PackResourceFilePath
projHitSoundPath  = "event:/SFX Events/Enemy/Dog/attack-projectile-hit"            :: FilePath

data DogProjectileData = DogProjectileData
    { _attack :: Attack
    , _config :: DogEnemyConfig
    }

mkDogProjectile :: (ConfigsRead m, MonadIO m) => Enemy DogEnemyData -> m (Some Projectile)
mkDogProjectile enemy =
    let
        cfg                            = _dog $ _config (E._data enemy :: DogEnemyData)
        Pos2 shootOffsetX shootOffsetY = _shootOffset cfg
        dir                            = E._dir enemy
        dirNeg                         = directionNeg dir
        shootOffset                    = Pos2 (shootOffsetX * dirNeg) shootOffsetY

        pos              = E._pos enemy `vecAdd` shootOffset
        enemyData        = E._data enemy
        shootProjAtkDesc = _shootProjectile $ _attackDescs enemyData
    in do
        atk <- mkAttack pos dir shootProjAtkDesc

        let
            dogProjData = DogProjectileData
                { _attack = atk
                , _config = cfg
                }

            angle              = _shootProjectileAngle cfg
            aimVec             = vecNormalize $ Vec2 (cos angle * dirNeg) (sin angle)
            vel                = toVel2 $ aimVec `vecMul` _shootProjectileSpeed cfg
            hbx                = fromMaybe (dummyHitbox pos) (attackHitbox atk)
            shootProjAliveSecs = _shootProjectileAliveSecs cfg

        msgId <- newId
        return . Some $ (mkProjectile dogProjData msgId hbx shootProjAliveSecs)
            { _vel                  = vel
            , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
            , _update               = updateDogProjectile
            , _draw                 = drawDogProjectile
            , _processCollisions    = processCollisions
            }

updateDogProjectile :: Monad m => ProjectileUpdate DogProjectileData m
updateDogProjectile dogProjectile = return $ dogProjectile
    { _data   = dogProjData'
    , _hitbox = const $ fromMaybe (dummyHitbox pos) (attackHitbox atk')
    , _ttl    = ttl
    , _vel    = vel
    }
    where
        dogProjData    = P._data dogProjectile
        atk            = _attack (dogProjData :: DogProjectileData)
        Vel2 velX velY = P._vel dogProjectile
        gravity        = _shootProjectileGravity $ _config (dogProjData :: DogProjectileData)
        vel            = Vel2 velX (velY + gravity * timeStep)
        atkPos         = _pos (atk :: Attack)
        pos            = atkPos `vecAdd` (toPos2 $ vel `vecMul` timeStep)

        dir              = _dir (atk :: Attack)
        atk'             = updateAttack pos dir atk
        ttl
            | _done atk' = 0.0
            | otherwise  = _ttl dogProjectile
        dogProjData'     = dogProjData {_attack = atk'} :: DogProjectileData

processCollisions :: ProjectileProcessCollisions DogProjectileData
processCollisions collisions dogProjectile = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player -> playerCollision player dogProjectile
        _                          -> []

playerCollision :: CollisionEntity e => e -> Projectile DogProjectileData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player dogProjectile =
    [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
    , mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsgTo (ProjectileMsgSetTtl 0.0) dogProjId
    , mkMsg $ AudioMsgPlaySound projHitSoundPath pos
    ]
        where
            playerId      = collisionEntityMsgId player
            atk           = _attack (P._data dogProjectile :: DogProjectileData)
            atkHit        = mkAttackHit $ _attack (P._data dogProjectile :: DogProjectileData)
            pos           = _pos (atk :: Attack)
            dir           = _dir (atk :: Attack)
            mkHitEffect = loadSimpleParticle pos dir enemyAttackProjectileZIndex projHitEffectPath
            dogProjId     = P._msgId dogProjectile

drawDogProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw DogProjectileData m
drawDogProjectile dogProjectile =
    let
        atk    = _attack (P._data dogProjectile :: DogProjectileData)
        spr    = attackSprite atk
        atkPos = _pos (atk :: Attack)
        vel    = P._vel dogProjectile
        dir    = _dir (atk :: Attack)
    in do
        pos <- graphicsLerpPos atkPos vel
        drawSprite pos dir enemyAttackProjectileZIndex spr
