module Attack.Projectile
    ( mkPlayerAttackProjectile
    , mkEnemyAttackProjectile
    , mkPlayerEnemyAttackProjectile
    , mkAttackProjectile
    , mkAttackProjectileWithId
    , attackProjectileAttack
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

mkPlayerAttackProjectile :: MonadIO m => Pos2 -> Direction -> AttackDescription -> m (Some Projectile)
mkPlayerAttackProjectile pos dir atkDesc = mkAttackProjectile pos dir atkDesc registeredCollisions
    where registeredCollisions = [ProjRegisteredEnemyCollision, ProjRegisteredRoomItemCollision]

mkEnemyAttackProjectile :: MonadIO m => Pos2 -> Direction -> AttackDescription -> m (Some Projectile)
mkEnemyAttackProjectile pos dir atkDesc = mkAttackProjectile pos dir atkDesc [ProjRegisteredPlayerCollision]

mkPlayerEnemyAttackProjectile :: MonadIO m => Pos2 -> Direction -> AttackDescription -> m (Some Projectile)
mkPlayerEnemyAttackProjectile pos dir atkDesc = mkAttackProjectile pos dir atkDesc registeredCollisions
    where
        registeredCollisions =
            [ ProjRegisteredPlayerCollision
            , ProjRegisteredRoomItemCollision
            , ProjRegisteredEnemyCollision
            ]

mkAttackProjectile
    :: MonadIO m
    => Pos2
    -> Direction
    -> AttackDescription
    -> [ProjectileRegisteredCollision]
    -> m (Some Projectile)
mkAttackProjectile pos dir atkDesc registeredCollisions = do
    msgId <- newId
    mkAttackProjectileWithId msgId pos dir atkDesc registeredCollisions

mkAttackProjectileWithId
    :: MonadIO m
    => MsgId
    -> Pos2
    -> Direction
    -> AttackDescription
    -> [ProjectileRegisteredCollision]
    -> m (Some Projectile)
mkAttackProjectileWithId msgId pos dir atkDesc registeredCollisions = do
    atk <- mkAttack pos dir atkDesc
    let
        hbx         = attackProjectileHitbox' atk
        atkProjData = AttackProjectileData {_attack = atk}

    return . Some $ (mkProjectile atkProjData msgId hbx maxSecs)
        { _hitbox               = attackProjectileHitbox
        , _registeredCollisions = S.fromList registeredCollisions
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
