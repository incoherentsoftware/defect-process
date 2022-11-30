module Enemy.All.BubbleTurret.Behavior
    ( BubbleTurretEnemyBehaviorInstr(..)
    , BubbleTurretEnemyBehavior(..)
    , isHurtBehavior
    ) where

import Util

data BubbleTurretEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr
    | StartAttackInstr
    | CreateBubbleProjInstr
    | UpdateHurtInstr Secs
    | UpdateSpawnInstr
    | StartDeathInstr
    | SetDeadInstr

data BubbleTurretEnemyBehavior
    = SpawnBehavior
    | IdleBehavior
    | HurtBehavior Secs
    | AttackBehavior
    | DeathBehavior
    deriving (Eq, Show)

isHurtBehavior :: BubbleTurretEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ -> True
    _              -> False
