module Enemy.All.Bat.Behavior
    ( BatEnemyBehaviorInstr(..)
    , HurtType(..)
    , HangtimeStatus(..)
    , BatEnemyBehavior(..)
    , isIdleBehavior
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isLaunchedBehavior
    ) where

import Util

data BatEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartPatrolInstr
    | UpdatePatrolInstr
    | SetDirectionInstr Direction
    | StartAttackInstr
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | LaunchedInHangtimeInstr Secs
    | LaunchedNotInHangtimeInstr Secs
    | StartFallenInstr Secs
    | UpdateFallenInstr Secs
    | StartGetUpInstr
    | StartFlyUpwardsInstr
    | UpdateFlyUpwardsInstr
    | StartWallSplatInstr
    | UpdateWallSplatInstr Secs
    | UpdateSpawnInstr
    | StartDeathInstr
    | SetDeadInstr

data HurtType
    = AirHurt
    | LaunchUpHurt
    | LaunchedHurt
    | FallenHurt
    | KnockDownHurt
    | WallHurt
    deriving (Eq, Show)

data HangtimeStatus
    = InHangtime
    | NotInHangtime
    deriving (Eq, Show)

data BatEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | PatrolBehavior
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs HangtimeStatus
    | FlyUpwardsBehavior
    | AttackBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isIdleBehavior :: BatEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isHurtBehavior :: BatEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: BatEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: BatEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: BatEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: BatEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: BatEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ _ -> True
    _                    -> False
