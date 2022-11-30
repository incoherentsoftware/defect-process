module Enemy.All.Zombie.Projectile
    ( mkZombieProjectile
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs.All.Enemy
import Configs.All.Enemy.Zombie
import Enemy.All.Zombie.AttackDescriptions
import Enemy.All.Zombie.Behavior
import Enemy.All.Zombie.Data
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packFilePath       = "data/enemies/zombie-enemy.pack"                               :: FilePath
projFadeEffectPath = PackResourceFilePath packFilePath "attack-projectile-fade.spr" :: PackResourceFilePath

data ZombieProjData = ZombieProjData
    { _initialAttack :: Attack
    , _lingerAttack  :: Attack
    }

mkZombieProjData :: MonadIO m => Pos2 -> Direction -> AttackType -> ZombieEnemyData -> m ZombieProjData
mkZombieProjData pos dir atkType enemyData =
    ZombieProjData <$>
    mkAttack pos dir initialAtkDesc <*>
    mkAttack pos dir (_projFlames atkDescs)
    where
        atkDescs       = _attackDescs enemyData
        initialAtkDesc = case atkType of
            SpitAttackType -> _projSpit atkDescs
            FallAttackType -> _projPuddleIgnite atkDescs

updateZombieProjData :: ZombieProjData -> ZombieProjData
updateZombieProjData projData = ZombieProjData
    { _initialAttack = initialAtk'
    , _lingerAttack  = lingerAtk'
    }
    where
        initialAtk    = _initialAttack projData
        initialAtkPos = _pos (initialAtk :: Attack)
        initialAtkDir = _dir (initialAtk :: Attack)
        initialAtk'   = updateAttack initialAtkPos initialAtkDir initialAtk

        lingerAtk              = _lingerAttack projData
        lingerAtkPos           = _pos (lingerAtk :: Attack)
        lingerAtkDir           = _dir (lingerAtk :: Attack)
        lingerAtk'
            | _done initialAtk = updateAttack lingerAtkPos lingerAtkDir lingerAtk
            | otherwise        = lingerAtk

zombieProjAttack :: ZombieProjData -> Attack
zombieProjAttack projData
    | _done initialAtk = _lingerAttack projData
    | otherwise        = initialAtk
    where initialAtk = _initialAttack projData

zombieProjFinished :: ZombieProjData -> Bool
zombieProjFinished = _done . _lingerAttack

zombieProjHitbox :: ProjectileHitbox ZombieProjData
zombieProjHitbox zombieProj = fromMaybe (dummyHitbox pos) (attackHitbox atk)
    where
        atk = zombieProjAttack $ _data zombieProj
        pos = _pos (atk :: Attack)

mkZombieProjectile :: MonadIO m => Pos2 -> Direction -> AttackType -> ZombieEnemyData -> m (Some Projectile)
mkZombieProjectile enemyPos dir atkType enemyData =
    let
        pos = case atkType of
            FallAttackType -> enemyPos
            SpitAttackType ->
                let
                    Pos2 offsetX offsetY  = _atkSpitProjOffset . _zombie $ _config enemyData
                    offset                = Pos2 (offsetX * directionNeg dir) offsetY
                in enemyPos `vecAdd` offset
    in do
        msgId       <- newId
        projData    <- mkZombieProjData pos dir atkType enemyData
        let dummyHbx = dummyHitbox enemyPos

        return . Some $ (mkProjectile projData msgId dummyHbx maxSecs)
            { _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
            , _hitbox               = zombieProjHitbox
            , _think                = thinkZombieProj
            , _update               = updateZombieProj
            , _draw                 = drawZombieProj
            , _processCollisions    = processCollisions
            }

thinkZombieProj :: MsgsWrite ThinkProjectileMsgsPhase m => ProjectileThink ZombieProjData m
thinkZombieProj = return . thinkAttack . zombieProjAttack . _data

updateZombieProj :: MsgsWrite UpdateProjectileMsgsPhase m => ProjectileUpdate ZombieProjData m
updateZombieProj zombieProj =
    let
        projData = updateZombieProjData $ _data zombieProj

        ttl
            | zombieProjFinished projData = 0.0
            | otherwise                   = _ttl zombieProj
    in do
        when (ttl <= 0.0) $
            let
                atk          = zombieProjAttack projData
                pos          = _pos (atk :: Attack)
                dir          = _dir (atk :: Attack)
                mkFadeEffect = loadSimpleParticle pos dir enemyAttackProjectileZIndex projFadeEffectPath
            in writeMsgs [mkMsg $ ParticleMsgAddM mkFadeEffect]

        return $ zombieProj
            { _data = projData
            , _ttl  = ttl
            }

processCollisions :: ProjectileProcessCollisions ZombieProjData
processCollisions collisions zombieProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player -> playerCollision player zombieProj
        _                          -> []

playerCollision :: CollisionEntity e => e -> Projectile ZombieProjData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player zombieProj =
    [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
    , mkMsgTo (ProjectileMsgSetTtl 0.0) zombieProjId
    ]
    where
        playerId     = collisionEntityMsgId player
        projData     = _data zombieProj
        atkHit       = mkAttackHit $ zombieProjAttack projData
        zombieProjId = P._msgId zombieProj

drawZombieProj :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw ZombieProjData m
drawZombieProj zombieProj =
    let
        atk = zombieProjAttack $ _data zombieProj
        spr = attackSprite atk
        pos = _pos (atk :: Attack)
        vel = P._vel zombieProj
        dir = _dir (atk :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr
