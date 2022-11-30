module Enemy.All.Blob.Behavior
    ( BlobEnemyBehaviorInstr(..)
    , HurtType(..)
    , BlobEnemyBehavior(..)
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isLaunchedBehavior
    , isIdleBehavior
    ) where

import Util

data BlobEnemyBehaviorInstr
    = StartIdleInstr
    | StartIdleInstrEx Secs
    | UpdateIdleInstr Secs
    | StartAttackInstr (Maybe Int)
    | UpdateAttackInstr
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | UpdateLaunchedInstr Secs
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

data BlobEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | AttackBehavior Int
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isHurtBehavior :: BlobEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: BlobEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: BlobEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: BlobEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: BlobEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: BlobEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False

isIdleBehavior :: BlobEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False
