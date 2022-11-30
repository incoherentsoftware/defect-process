module Enemy.All.Hammer.Behavior
    ( HammerEnemyBehaviorInstr(..)
    , HurtType(..)
    , HangtimeStatus(..)
    , HammerEnemyBehavior(..)
    , isIdleBehavior
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isLaunchedBehavior
    ) where

import Attack.Description
import Util

data HammerEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartPatrolInstr
    | UpdatePatrolInstr
    | StartTeleportInstr
    | UpdateTeleportInstr
    | StartReFormInstr
    | StartAttackLandInstr
    | CreateAttackLandParticlesInstr
    | StartAttackInstr AttackDescription
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | LaunchedInHangtimeInstr Secs
    | LaunchedNotInHangtimeInstr Secs
    | StartFallenInstr Secs
    | UpdateFallenInstr Secs
    | StartSitUpInstr
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

data HammerEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | PatrolBehavior
    | TeleportBehavior
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs HangtimeStatus
    | ReFormBehavior
    | AttackBehavior
    | AttackLandBehavior
    | FallenBehavior Secs
    | SitUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isIdleBehavior :: HammerEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isHurtBehavior :: HammerEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: HammerEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: HammerEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: HammerEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isLaunchedBehavior :: HammerEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ _ -> True
    _                    -> False
