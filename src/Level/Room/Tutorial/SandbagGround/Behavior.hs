module Level.Room.Tutorial.SandbagGround.Behavior
    ( SandbagGroundBehaviorInstr(..)
    , HurtType(..)
    , SandbagGroundBehavior(..)
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isIdleBehavior
    , isLaunchedBehavior
    ) where

import Util

data SandbagGroundBehaviorInstr
    = StartIdleInstr
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | LaunchedHangtimeInstr Secs
    | StartFallenInstr Secs
    | UpdateFallenInstr Secs
    | StartDematerializeInstr
    | StartRematerializeInstr
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

data SandbagGroundBehavior
    = SpawnBehavior
    | DeathBehavior
    | IdleBehavior
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs
    | FallenBehavior Secs
    | DematerializeBehavior
    | RematerializeBehavior
    | WallSplatBehavior Secs
    deriving (Eq, Show)

isHurtBehavior :: SandbagGroundBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: SandbagGroundBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: SandbagGroundBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: SandbagGroundBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: SandbagGroundBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isIdleBehavior :: SandbagGroundBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior -> True
    _            -> False

isLaunchedBehavior :: SandbagGroundBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False
