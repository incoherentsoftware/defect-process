module Enemy.All.Turret.Behavior
    ( TurretEnemyBehaviorInstr(..)
    , TurretEnemyBehavior(..)
    , isHurtBehavior
    , isDeathBehavior
    ) where

import Util

data TurretEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr
    | StartAttackInstr
    | SetPostAttackCooldownInstr
    | UpdateHurtInstr Secs
    | UpdateSpawnInstr
    | StartDeathInstr
    | SetDeadInstr

data TurretEnemyBehavior
    = SpawnBehavior
    | IdleBehavior
    | HurtBehavior Secs
    | AttackBehavior
    | DeathBehavior
    deriving (Eq, Show)

isHurtBehavior :: TurretEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ -> True
    _              -> False

isDeathBehavior :: TurretEnemyBehavior -> Bool
isDeathBehavior = \case
    DeathBehavior -> True
    _             -> False
