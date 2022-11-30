module Enemy.All.Zombie.Behavior
    ( ZombieEnemyBehaviorInstr(..)
    , PostIdleAction(..)
    , HurtType(..)
    , AttackType(..)
    , ZombieEnemyBehavior(..)
    , isHurtBehavior
    , isIdleBehavior
    , isWalkBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isLaunchedBehavior
    ) where

import Util

data ZombieEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs PostIdleAction
    | StartWalkInstr
    | UpdateWalkInstr Secs
    | FlipDirectionInstr
    | ResetAtkCooldownInstr
    | StartAttackInstr
    | CreateAttackProjInstr AttackType
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

data PostIdleAction
    = TurnAroundPostIdle
    | WalkPostIdle
    deriving Eq

data HurtType
    = StandHurt
    | AirHurt
    | LaunchUpHurt
    | FallenHurt
    | KnockDownHurt
    | WallHurt
    deriving Eq

data AttackType
    = SpitAttackType
    | FallAttackType
    deriving Eq

data ZombieEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs PostIdleAction
    | WalkBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | AttackBehavior AttackType
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving Eq

isIdleBehavior :: ZombieEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ _ -> True
    _                -> False

isWalkBehavior :: ZombieEnemyBehavior -> Bool
isWalkBehavior = \case
    WalkBehavior _ -> True
    _              -> False

isHurtBehavior :: ZombieEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: ZombieEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: ZombieEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: ZombieEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: ZombieEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: ZombieEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
