module Enemy.All.Claws.Projectile
    ( projHitEffectPath
    , projFadeEffectPath
    , mkClawsProjectile
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Configs.All.Enemy
import Configs.All.Enemy.Claws
import Constants
import Enemy.All.Claws.AttackDescriptions
import Enemy.All.Claws.Data
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packFilePath         = "data/enemies/claws-enemy.pack"                                :: FilePath
projHitEffectPath    = PackResourceFilePath packFilePath "attack-projectile-hit.spr"  :: PackResourceFilePath
projFadeEffectPath   = PackResourceFilePath packFilePath "attack-projectile-fade.spr" :: PackResourceFilePath
projHitSoundFilePath = "event:/SFX Events/Enemy/Claws/attack-projectile-hit"          :: FilePath

data ClawsProjData = ClawsProjData
    { _attack :: Attack
    }

clawsProjHitbox :: Attack -> Hitbox
clawsProjHitbox atk = fromMaybe (dummyHitbox pos) (attackHitbox atk)
    where pos = _pos (atk :: Attack)

clawsProjVel :: Attack -> Vel2
clawsProjVel atk = attackVelToVel2 (attackVel atk) zeroVel2

mkClawsProjectile :: MonadIO m => Pos2 -> Direction -> ClawsEnemyData -> m (Some Projectile)
mkClawsProjectile enemyPos dir enemyData =
    let
        Pos2 offsetX offsetY = _projectileSpawnOffset . _claws $ _config enemyData
        offset               = Pos2 (offsetX * directionNeg dir) offsetY
        projAtkDesc          = _projectile $ _attackDescs enemyData
        pos                  = enemyPos `vecAdd` offset
    in do
        msgId  <- newId
        atk    <- mkAttack pos dir projAtkDesc
        let hbx = clawsProjHitbox atk

        let clawsProjData = ClawsProjData {_attack = atk}

        return . Some $ (mkProjectile clawsProjData msgId hbx maxSecs)
            { _vel                  = clawsProjVel atk
            , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
            , _update               = updateClawsProj
            , _draw                 = drawClawsProj
            , _processCollisions    = processCollisions
            }

updateClawsProj :: MsgsWrite UpdateProjectileMsgsPhase m => ProjectileUpdate ClawsProjData m
updateClawsProj clawsProj =
    let
        clawsProjData = _data clawsProj
        atk           = _attack (clawsProjData :: ClawsProjData)
        pos           = _pos (atk :: Attack)
        vel           = clawsProjVel atk
        pos'          = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        dir           = _dir (atk :: Attack)

        atk'             = updateAttack pos' dir atk
        ttl
            | _done atk' = 0.0
            | otherwise  = _ttl clawsProj
        clawsProjData'   = clawsProjData {_attack = atk'} :: ClawsProjData
    in do
        when (ttl <= 0.0) $
            let mkFadeEffect = loadSimpleParticle pos' dir enemyAttackProjectileZIndex projFadeEffectPath
            in writeMsgs [mkMsg $ ParticleMsgAddM mkFadeEffect]

        return $ clawsProj
            { _data   = clawsProjData'
            , _vel    = vel
            , _hitbox = const $ clawsProjHitbox atk'
            , _ttl    = ttl
            }

processCollisions :: ProjectileProcessCollisions ClawsProjData
processCollisions collisions clawsProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player -> playerCollision player clawsProj
        _                          -> []

playerCollision :: CollisionEntity e => e -> Projectile ClawsProjData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player clawsProj =
    [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
    , mkMsgTo (ProjectileMsgSetTtl 0.0) clawsProjId
    , mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsg $ AudioMsgPlaySound projHitSoundFilePath pos
    ]
    where
        playerId      = collisionEntityMsgId player
        clawsProjData = _data clawsProj :: ClawsProjData
        atkHit        = mkAttackHit $ _attack clawsProjData
        clawsProjId   = P._msgId clawsProj

        atk           = _attack clawsProjData
        pos           = _pos (atk :: Attack)
        dir           = _dir (atk :: Attack)
        mkHitEffect = loadSimpleParticle pos dir enemyAttackProjectileZIndex projHitEffectPath

drawClawsProj :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw ClawsProjData m
drawClawsProj clawsProj =
    let
        atk = _attack (_data clawsProj :: ClawsProjData)
        spr = attackSprite atk
        pos = _pos (atk :: Attack)
        vel = P._vel clawsProj
        dir = _dir (atk :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr
