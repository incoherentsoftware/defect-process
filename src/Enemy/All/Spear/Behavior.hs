module Enemy.All.Spear.Behavior
    ( SpearEnemyBehaviorInstr(..)
    , HurtType(..)
    , SpearEnemyBehavior(..)
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

import Enemy.All.Spear.AttackType
import Util

data SpearEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartWalkInstr
    | UpdateWalkInstr Secs
    | StartRetreatInstr
    | UpdateRetreatInstr Secs
    | FlipDirectionInstr
    | StartAttackInstr SpearEnemyAttackType
    | CreateAttackProjInstr
    | SetThrowAtkCooldownInstr
    | SetShoveAtkCooldownInstr
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

data SpearEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | WalkBehavior Secs
    | RetreatBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | AttackBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isIdleBehavior :: SpearEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isWalkBehavior :: SpearEnemyBehavior -> Bool
isWalkBehavior = \case
    WalkBehavior _ -> True
    _              -> False

isRetreatBehavior :: SpearEnemyBehavior -> Bool
isRetreatBehavior = \case
    RetreatBehavior _ -> True
    _                 -> False

isHurtBehavior :: SpearEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: SpearEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: SpearEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: SpearEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: SpearEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: SpearEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
