module Enemy.All.Dog.Behavior
    ( DogEnemyBehaviorInstr(..)
    , HurtType(..)
    , PaceStatus(..)
    , DogEnemyBehavior(..)
    , isIdleBehavior
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isLaunchedBehavior
    ) where

import Attack
import Util

data DogEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | StartRunTowardsInstr
    | UpdateRunTowardsInstr Secs
    | StartRunFromInstr
    | UpdateRunFromInstr Secs
    | StartPaceForwardsInstr
    | StartPaceTurnAroundInstr
    | UpdatePaceForwardsInstr Secs
    | UpdatePaceTurnAroundInstr
    | FlipDirectionInstr
    | StartAttackInstr AttackDescription
    | SetPostAttackCooldownInstr
    | CreateAttackProjInstr
    | UpdateWillUseAttackProjInstr
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

data PaceStatus
    = PaceForwards Secs
    | PaceTurnAround
    deriving (Eq, Show)

data DogEnemyBehavior
    = SpawnBehavior
    | IdleBehavior Secs
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | PaceBehavior PaceStatus
    | RunTowardsBehavior Secs
    | RunFromBehavior Secs
    | AttackBehavior
    | FallenBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | DeathBehavior
    deriving (Eq, Show)

isIdleBehavior :: DogEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isHurtBehavior :: DogEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: DogEnemyBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: DogEnemyBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: DogEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: DogEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isLaunchedBehavior :: DogEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
