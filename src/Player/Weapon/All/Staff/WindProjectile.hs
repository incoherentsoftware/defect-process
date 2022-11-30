module Player.Weapon.All.Staff.WindProjectile
    ( mkWindProjectile
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Collision
import Constants
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

registeredCollisions =
    [ ProjRegisteredEnemyCollision
    , ProjRegisteredPlayerCollision
    , ProjRegisteredRoomItemCollision
    ] :: [ProjectileRegisteredCollision]

staffPack             = \f -> PackResourceFilePath "data/player/weapons/staff.pack" f
windProjDisappearPath = staffPack "wind-projectile-disappear.spr" :: PackResourceFilePath

data WindProjectileData = WindProjectileData
    { _attack :: Attack
    }

mkWindProjectile :: MonadIO m => Pos2 -> Direction -> AttackDescription -> m (Some Projectile)
mkWindProjectile pos dir atkDesc = do
    msgId <- newId
    atk   <- mkAttack pos dir atkDesc

    let
        hbx         = windProjectileHitbox' atk
        atkProjData = WindProjectileData {_attack = atk}

    return . Some $ (mkProjectile atkProjData msgId hbx maxSecs)
        { _hitbox               = windProjectileHitbox
        , _registeredCollisions = S.fromList registeredCollisions
        , _think                = thinkWindProjectile
        , _update               = updateWindProjectile
        , _draw                 = drawWindProjectile
        , _processCollisions    = processWindProjectileCollisions
        }

windProjectileAttack :: Projectile WindProjectileData -> Attack
windProjectileAttack atkProj = _attack (P._data atkProj :: WindProjectileData)

windProjectileHitbox :: ProjectileHitbox WindProjectileData
windProjectileHitbox = windProjectileHitbox' . windProjectileAttack

windProjectileHitbox' :: Attack -> Hitbox
windProjectileHitbox' atk = fromMaybe dummyHbx (attackHitbox atk)
    where dummyHbx = DummyHitbox $ _pos (atk :: Attack)

windProjectileVel :: Projectile WindProjectileData -> Vel2
windProjectileVel atkProj = attackVelToVel2 (attackVel atk) zeroVel2
    where atk = _attack (P._data atkProj :: WindProjectileData)

thinkWindProjectile :: Monad m => ProjectileThink WindProjectileData m
thinkWindProjectile atkProj = return $ thinkAttack atk
    where atk = _attack (P._data atkProj :: WindProjectileData)

updateWindProjectile :: MsgsWrite UpdateProjectileMsgsPhase m => ProjectileUpdate WindProjectileData m
updateWindProjectile atkProj =
    let
        atkProjData  = P._data atkProj
        atk          = _attack (atkProjData :: WindProjectileData)
        vel          = windProjectileVel atkProj
        pos          = _pos (atk :: Attack) `vecAdd` toPos2 (vel `vecMul` timeStep)
        dir          = _dir (atk :: Attack)
        atk'         = updateAttack pos dir atk
        atkDone      = _done atk'
        ttl          = if atkDone then 0.0 else _ttl atkProj
        atkProjData' = atkProjData {_attack = atk'} :: WindProjectileData
    in do
        when atkDone $
            let mkDisappearParticle = loadSimpleParticle pos dir worldProjectileZIndex windProjDisappearPath
            in writeMsgs [mkMsg $ ParticleMsgAddM mkDisappearParticle]

        return $ atkProj
            { _data = atkProjData'
            , _ttl  = ttl
            }

processWindProjectileCollisions :: ProjectileProcessCollisions WindProjectileData
processWindProjectileCollisions collisions atkProj = foldr processCollision [] collisions
    where
        processCollision :: ProjectileCollision -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        processCollision collision !msgs = case collision of
            ProjEnemyCollision enemy              -> attackEnemyHitMessages enemy atk ++ msgs
            ProjRoomItemCollision (Some roomItem) -> attackCollisionEntityHitMessages roomItem atk ++ msgs
            _                                     -> msgs
            where atk = _attack (P._data atkProj :: WindProjectileData)

drawWindProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw WindProjectileData m
drawWindProjectile atkProj =
    let
        atk = _attack (P._data atkProj :: WindProjectileData)
        vel = attackVelToVel2 (attackVel atk) zeroVel2
        pos = _pos (atk :: Attack)
        dir = _dir (atk :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir worldProjectileZIndex (attackSprite atk)
