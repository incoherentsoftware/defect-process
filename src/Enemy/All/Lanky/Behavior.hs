module Enemy.All.Lanky.Behavior
    ( LankyEnemyBehaviorInstr(..)
    , HurtType(..)
    , LankyEnemyBehavior(..)
    , isIdleBehavior
    , isWalkBehavior
    , isRetreatBehavior
    , isHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isKneelingBehavior
    , isLaunchedBehavior
    ) where

import Enemy.All.Lanky.AttackType
import Util

data LankyEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartWalkInstr
    | UpdateWalkInstr Secs
    | StartRetreatInstr
    | UpdateRetreatInstr Secs
    | FacePlayerInstr
    | StartAttackInstr LankyEnemyAttackType
    | CreateAttackPillarInstr
    | SetSummonAtkCooldownInstr
    | SetBeamAtkCooldownInstr
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | LaunchedHangtimeInstr Secs
    | UpdateKneelingInstr Secs
    | StartGetUpInstr
    | StartWallSplatInstr Secs
    | UpdateWallSplatInstr Secs
    | UpdateSpawnInstr
    | StartDeathInstr
    | SetDeadInstr

data HurtType
    = NormalHurt
    | LaunchUpHurt
    | AirHurt
    | KneelingHurt
    | WallHurt
    deriving (Eq, Show)

data LankyEnemyBehavior
    = SpawnBehavior
    | DeathBehavior
    | IdleBehavior Secs
    | WalkBehavior Secs
    | RetreatBehavior Secs
    | AttackBehavior
    | AuraBreakBehavior
    | LaunchedBehavior Secs
    | HurtBehavior Secs HurtType
    | KneelingBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    deriving (Eq, Show)

isIdleBehavior :: LankyEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isWalkBehavior :: LankyEnemyBehavior -> Bool
isWalkBehavior = \case
    WalkBehavior _ -> True
    _              -> False

isRetreatBehavior :: LankyEnemyBehavior -> Bool
isRetreatBehavior = \case
    RetreatBehavior _ -> True
    _                 -> False

isHurtBehavior :: LankyEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isWallSplatBehavior :: LankyEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: LankyEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isKneelingBehavior :: LankyEnemyBehavior -> Bool
isKneelingBehavior = \case
    KneelingBehavior _ -> True
    _                  -> False

isLaunchedBehavior :: LankyEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
