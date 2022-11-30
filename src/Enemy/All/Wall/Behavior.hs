module Enemy.All.Wall.Behavior
    ( WallEnemyBehaviorInstr(..)
    , HurtType(..)
    , WallEnemyBehavior(..)
    , isIdleBehavior
    , isWalkBehavior
    , isBackWalkBehavior
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isLaunchedBehavior
    ) where

import Util

data WallEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartWalkInstr
    | UpdateWalkInstr Secs
    | StartBackWalkInstr
    | UpdateBackWalkInstr Secs
    | FacePlayerInstr
    | StartAttackInstr
    | CreateAttackProjInstr
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
    deriving Eq

data WallEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | WalkBehavior Secs
    | BackWalkBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | AttackBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving Eq

isIdleBehavior :: WallEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isWalkBehavior :: WallEnemyBehavior -> Bool
isWalkBehavior = \case
    WalkBehavior _ -> True
    _              -> False

isBackWalkBehavior :: WallEnemyBehavior -> Bool
isBackWalkBehavior = \case
    BackWalkBehavior _ -> True
    _                  -> False

isHurtBehavior :: WallEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: WallEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: WallEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: WallEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: WallEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: WallEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
