module Enemy.All.Flying.Behavior
    ( FlyingEnemyBehaviorInstr(..)
    , AttackType(..)
    , HurtType(..)
    , HangtimeStatus(..)
    , FlyingEnemyBehavior(..)
    , isIdleBehavior
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isLaunchedBehavior
    ) where

import Attack.Description
import Util

data FlyingEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | FacePlayerInstr
    | StartAttackInstr AttackDescription
    | CreateAttackProjInstr
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

data AttackType
    = ShootAttackType
    | ShockAttackType
    deriving Eq

data HurtType
    = FlyingHurt
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

data FlyingEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs HangtimeStatus
    | FlyUpwardsBehavior
    | AttackBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isIdleBehavior :: FlyingEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isHurtBehavior :: FlyingEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: FlyingEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: FlyingEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: FlyingEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isLaunchedBehavior :: FlyingEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ _ -> True
    _                    -> False
