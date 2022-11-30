module Enemy.All.Giant.Behavior
    ( GiantEnemyBehaviorInstr(..)
    , GiantEnemyBehavior(..)
    , isIdleBehavior
    , isAdvanceBehavior
    , isRetreatBehavior
    ) where

import Attack.Description
import Util

data GiantEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartAdvanceInstr
    | UpdateAdvanceInstr Secs
    | StartRetreatInstr
    | UpdateRetreatInstr Secs
    | StartAttackInstr AttackDescription
    | SetPostAttackCooldownInstr
    | UpdateSpawnInstr
    | StartDeathInstr
    | SetDeadInstr

data GiantEnemyBehavior
    = IdleBehavior Secs
    | SpawnBehavior
    | DeathBehavior
    | AdvanceBehavior Secs
    | RetreatBehavior Secs
    | AttackBehavior
    deriving (Eq, Show)

isIdleBehavior :: GiantEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isAdvanceBehavior :: GiantEnemyBehavior -> Bool
isAdvanceBehavior = \case
    AdvanceBehavior _ -> True
    _                 -> False

isRetreatBehavior :: GiantEnemyBehavior -> Bool
isRetreatBehavior = \case
    RetreatBehavior _ -> True
    _                 -> False
