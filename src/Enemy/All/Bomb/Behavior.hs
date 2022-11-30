module Enemy.All.Bomb.Behavior
    ( BombEnemyBehaviorInstr(..)
    , HurtType(..)
    , BombEnemyBehavior(..)
    , isHurtBehavior
    , isLaunchedBehavior
    , isIdleBehavior
    , isWallSplatBehavior
    ) where

import Util

data BombEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartSearchInstr
    | UpdateSearchInstr Secs Int
    | StartSprintInstr
    | UpdateSprintInstr Secs
    | StartExplodeInstr
    | CreateExplosionInstr
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | UpdateLaunchedInstr Secs
    | StartWallSplatInstr
    | UpdateWallSplatInstr Secs
    | UpdateSpawnInstr
    | SetDeadInstr

data HurtType
    = StandHurt
    | AirHurt
    | LaunchUpHurt
    | FallenHurt
    | KnockDownHurt
    | WallHurt
    deriving (Eq, Show)

data BombEnemyBehavior
    = SpawnBehavior
    | SearchBehavior Secs Int
    | IdleBehavior Secs
    | SprintBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | WallSplatBehavior Secs
    deriving (Eq, Show)

isHurtBehavior :: BombEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isLaunchedBehavior :: BombEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False

isIdleBehavior :: BombEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isWallSplatBehavior :: BombEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False
