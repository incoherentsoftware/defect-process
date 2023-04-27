module Attack.Projectile
    ( mkAttackProjectile
    , mkAttackProjectileEx
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Collision
import Constants
import Id
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

data AttackProjectileData = AttackProjectileData
    { _attack :: Attack
    }

mkAttackProjectile
    :: forall m. MonadIO m
    => Pos2
    -> Direction
    -> AttackDescription
    -> (Pos2 -> Direction -> AttackDescription -> m Attack)
    -> S.Set ProjectileRegisteredCollision
    -> m (Some Projectile)
mkAttackProjectile pos dir atkDesc mkAttackF registeredCollisions =
    mkAttackProjectileEx pos dir atkDesc mkAttackF registeredCollisions =<< newId

mkAttackProjectileEx
    :: forall m. MonadIO m
    => Pos2
    -> Direction
    -> AttackDescription
    -> (Pos2 -> Direction -> AttackDescription -> m Attack)
    -> S.Set ProjectileRegisteredCollision
    -> MsgId
    -> m (Some Projectile)
mkAttackProjectileEx pos dir atkDesc mkAttackF registeredCollisions msgId = do
    atk <- mkAttackF pos dir atkDesc
    let
        hbx         = attackProjectileHitbox' atk
        atkProjData = AttackProjectileData {_attack = atk}

    return . Some $ (mkProjectile atkProjData msgId hbx maxSecs)
        { _hitbox               = attackProjectileHitbox
        , _registeredCollisions = registeredCollisions
        , _think                = thinkAttackProjectile
        , _update               = updateAttackProjectile
        , _draw                 = drawAttackProjectile
        , _processCollisions    = processAttackProjectileCollisions
        }

attackProjectileAttack :: Projectile AttackProjectileData -> Attack
attackProjectileAttack atkProj = _attack (P._data atkProj :: AttackProjectileData)

attackProjectileHitbox :: ProjectileHitbox AttackProjectileData
attackProjectileHitbox = attackProjectileHitbox' . attackProjectileAttack

attackProjectileHitbox' :: Attack -> Hitbox
attackProjectileHitbox' atk = fromMaybe dummyHbx (attackHitbox atk)
    where dummyHbx = DummyHitbox $ _pos (atk :: Attack)

attackProjectileVel :: Projectile AttackProjectileData -> Vel2
attackProjectileVel atkProj = attackVelToVel2 (attackVel atk) zeroVel2
    where atk = _attack (P._data atkProj :: AttackProjectileData)

thinkAttackProjectile :: Monad m => ProjectileThink AttackProjectileData m
thinkAttackProjectile atkProj = return $ thinkAttack atk
    where atk = _attack (P._data atkProj :: AttackProjectileData)

updateAttackProjectile :: Monad m => ProjectileUpdate AttackProjectileData m
updateAttackProjectile atkProj = return $ atkProj
    { _data = atkProjData'
    , _ttl  = ttl
    }
    where
        atkProjData  = P._data atkProj
        atk          = _attack (atkProjData :: AttackProjectileData)
        vel          = attackProjectileVel atkProj
        pos          = _pos (atk :: Attack) `vecAdd` toPos2 (vel `vecMul` timeStep)
        dir          = _dir (atk :: Attack)
        atk'         = updateAttack pos dir atk
        atkDone      = _done atk'
        ttl          = if atkDone then 0.0 else _ttl atkProj
        atkProjData' = atkProjData {_attack = atk'} :: AttackProjectileData

processAttackProjectileCollisions :: ProjectileProcessCollisions AttackProjectileData
processAttackProjectileCollisions collisions atkProj = foldr processCollision [] collisions
    where
        processCollision :: ProjectileCollision -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        processCollision collision !msgs = case collision of
            ProjEnemyCollision enemy              -> attackEnemyHitMessages enemy atk ++ msgs
            ProjPlayerCollision player            -> attackCollisionEntityHitMessages player atk ++ msgs
            ProjRoomItemCollision (Some roomItem) -> attackCollisionEntityHitMessages roomItem atk ++ msgs
            ProjSurfaceCollision hbx _            -> attackSurfaceHitMessages hbx atk ++ msgs
            ProjPlayerAttackCollision _ _         -> msgs
            where atk = _attack (P._data atkProj :: AttackProjectileData)

drawAttackProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw AttackProjectileData m
drawAttackProjectile atkProj =
    let
        atk = _attack (P._data atkProj :: AttackProjectileData)
        vel = attackVelToVel2 (attackVel atk) zeroVel2
        pos = _pos (atk :: Attack)
        dir = _dir (atk :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir worldProjectileZIndex (attackSprite atk)
