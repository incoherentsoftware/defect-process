module Level.Room.Tutorial.SandbagAir.Behavior
    ( SandbagAirBehaviorInstr(..)
    , HurtType(..)
    , HangtimeStatus(..)
    , SandbagAirBehavior(..)
    , isIdleBehavior
    , isHurtBehavior
    , isFallenBehavior
    , isFallenHurtBehavior
    , isWallSplatBehavior
    , isLaunchedBehavior
    ) where

import Util

data SandbagAirBehaviorInstr
    = StartIdleInstr
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | LaunchedInHangtimeInstr Secs
    | LaunchedNotInHangtimeInstr Secs
    | StartFallenInstr Secs
    | UpdateFallenInstr Secs
    | StartWallSplatInstr
    | UpdateWallSplatInstr Secs
    | StartDematerializeInstr
    | StartRematerializeInstr
    | UpdateSpawnInstr
    | StartDeathInstr
    | SetDeadInstr

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

data SandbagAirBehavior
    = SpawnBehavior
    | DeathBehavior
    | IdleBehavior
    | HurtBehavior Secs HurtType
    | LaunchedBehavior Secs HangtimeStatus
    | FallenBehavior Secs
    | WallSplatBehavior Secs
    | DematerializeBehavior
    | RematerializeBehavior
    deriving (Eq, Show)

isIdleBehavior :: SandbagAirBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior -> True
    _            -> False

isHurtBehavior :: SandbagAirBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isFallenBehavior :: SandbagAirBehavior -> Bool
isFallenBehavior = \case
    FallenBehavior _ -> True
    _                -> False

isFallenHurtBehavior :: SandbagAirBehavior -> Bool
isFallenHurtBehavior = \case
    HurtBehavior _ FallenHurt -> True
    _                         -> False

isWallSplatBehavior :: SandbagAirBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isLaunchedBehavior :: SandbagAirBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ _ -> True
    _                    -> False
