module Enemy.All.Flail.Behavior
    ( FlailEnemyBehaviorInstr(..)
    , HurtType(..)
    , FlailEnemyBehavior(..)
    , isIdleBehavior
    , isWalkBehavior
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

data FlailEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartWalkInstr
    | UpdateWalkInstr Secs
    | StartRetreatInstr
    | UpdateRetreatInstr Secs
    | StartIdleToWalkInstr
    | StartIdleToRetreatInstr
    | StartWalkToIdleInstr
    | FacePlayerInstr
    | StartAttackInstr AttackDescription
    | SetAttackCooldownInstr
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

data FlailEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | WalkBehavior Secs
    | RetreatBehavior Secs
    | IdleToWalkBehavior
    | IdleToRetreatBehavior
    | WalkToIdleBehavior
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | AttackBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving Eq

isIdleBehavior :: FlailEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isWalkBehavior :: FlailEnemyBehavior -> Bool
isWalkBehavior = \case
    WalkBehavior _ -> True
    _              -> False

isRetreatBehavior :: FlailEnemyBehavior -> Bool
isRetreatBehavior = \case
    RetreatBehavior _ -> True
    _                 -> False

isHurtBehavior :: FlailEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: FlailEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: FlailEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: FlailEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: FlailEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: FlailEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
