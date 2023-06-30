module Enemy.All.Hop.Behavior
    ( HopEnemyBehaviorInstr(..)
    , HurtType(..)
    , HopEnemyBehavior(..)
    , isIdleBehavior
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isLaunchedBehavior
    ) where

import Util

data HopEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartHopLongInstr
    | StartHopLongLandInstr
    | StartHopShortInstr
    | StartHopShortLandInstr
    | SetHopLongVelInstr
    | SetHopShortVelInstr
    | FlipHopDirectionInstr
    | StartAttackHopLongInstr
    | StartAttackHopLongLandInstr
    | StartAttackHopShortInstr
    | StartAttackHopShortLandInstr
    | CreateAttackProjInstr
    | ClearAttackInstr
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

data HopEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | HopLongBehavior
    | HopLongLandBehavior
    | HopShortBehavior
    | HopShortLandBehavior
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | AttackHopLongBehavior
    | AttackHopLongLandBehavior
    | AttackHopShortBehavior
    | AttackHopShortLandBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isIdleBehavior :: HopEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isHurtBehavior :: HopEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: HopEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: HopEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: HopEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: HopEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: HopEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
