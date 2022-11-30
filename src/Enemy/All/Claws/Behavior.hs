module Enemy.All.Claws.Behavior
    ( ClawsEnemyBehaviorInstr(..)
    , HurtType(..)
    , ClawsEnemyBehavior(..)
    , isIdleBehavior
    , isAdvanceBehavior
    , isRetreatBehavior
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isLaunchedBehavior
    ) where

import Attack
import Util

data ClawsEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartAdvanceInstr
    | UpdateAdvanceInstr Secs
    | StartRetreatInstr
    | UpdateRetreatInstr Secs
    | StartDashInstr
    | StartAttackInstr AttackDescription
    | CreateAttackProjInstr
    | UpdateWillUseAttackProjInstr
    | UpdateWillUseDashInstr
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

data ClawsEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | AdvanceBehavior Secs
    | DashBehavior
    | RetreatBehavior Secs
    | AttackBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isIdleBehavior :: ClawsEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isAdvanceBehavior :: ClawsEnemyBehavior -> Bool
isAdvanceBehavior = \case
    AdvanceBehavior _ -> True
    _                 -> False

isRetreatBehavior :: ClawsEnemyBehavior -> Bool
isRetreatBehavior = \case
    RetreatBehavior _ -> True
    _                 -> False

isHurtBehavior :: ClawsEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: ClawsEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: ClawsEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: ClawsEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: ClawsEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: ClawsEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
