module Enemy.All.Axe.Behavior
    ( AxeEnemyBehaviorInstr(..)
    , HurtType(..)
    , AxeEnemyBehavior(..)
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isIdleBehavior
    , isAdvanceBehavior
    , isRetreatBehavior
    , isLaunchedBehavior
    ) where

import Attack.Description.Types
import Util

data AxeEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartAdvanceInstr
    | UpdateAdvanceInstr Secs
    | StartRetreatInstr
    | UpdateRetreatInstr Secs
    | StartAttackInstr AttackDescription
    | UpdateWillUseAttackLungeInstr
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | LaunchedHangtimeInstr Secs
    | StartFallenInstr Secs
    | UpdateFallenInstr Secs
    | StartGetUpInstr
    | StartWallSplatInstr
    | UpdateWallSplatInstr Secs
    | UpdateSpawnInstr
    | StartDeathInstr
    | SetDeadInstr

data HurtType
    = StandHurt
    | AirHurt
    | LaunchUpHurt
    | FallenHurt
    | KnockDownHurt
    | WallHurt
    deriving (Eq, Show)

data AxeEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | AdvanceBehavior Secs
    | RetreatBehavior Secs
    | AttackBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isHurtBehavior :: AxeEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: AxeEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: AxeEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: AxeEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: AxeEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isIdleBehavior :: AxeEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isAdvanceBehavior :: AxeEnemyBehavior -> Bool
isAdvanceBehavior = \case
    AdvanceBehavior _ -> True
    _                 -> False

isRetreatBehavior :: AxeEnemyBehavior -> Bool
isRetreatBehavior = \case
    RetreatBehavior _ -> True
    _                 -> False

isLaunchedBehavior :: AxeEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
