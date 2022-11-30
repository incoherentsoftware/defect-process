module Enemy.All.Boss.Behavior
    ( BossEnemyBehaviorInstr(..)
    , HurtType(..)
    , BossEnemyBehavior(..)
    , isIdleBehavior
    , isHurtBehavior
    , isWallSplatBehavior
    , isWallHurtBehavior
    , isKneelingBehavior
    , isLaunchedBehavior
    , isAnyDeathBehavior
    , isAnyGuardBehavior
    , isIncapacitatedBehavior
    ) where

import Attack
import Util

data BossEnemyBehaviorInstr
    = StartIdleInstr
    | UpdateIdleInstr Secs
    | FacePlayerInstr
    | TeleportToPlayerInstr
    | StartHpThresholdSummonFlyingInstr
    | StartHpThresholdSummonSpearsInstr
    | StartHpThresholdSummonWallsInstr
    | StartHpThresholdPhaseOutInstr
    | StartHpThresholdAttackInstr
    | TeleportToPreHpThresholdPosInstr
    | StartAttackShortInstr
    | StartAttackMediumAirInstr
    | StartAttackLongInstr
    | SetAttackInstr AttackDescription
    | UpdateHurtInstr Secs HurtType
    | StartLaunchedInstr Secs
    | LaunchedHangtimeInstr Secs
    | UpdateKneelingInstr Secs
    | StartGetUpInstr
    | StartWallSplatInstr Secs
    | UpdateWallSplatInstr Secs
    | StartGuardInstr
    | StartAirGuardInstr
    | UpdateAirGuardInstr
    | UpdateSpawnInstr
    | StartDeathInstr
    | UpdateDeathInstr
    | StartAirDeathInstr
    | UpdateAirDeathInstr
    | SetDeadInstr
    | CreateBlobProjectileInstr
    | CreateTurret1ProjectileInstr
    | CreateTurret2ProjectileInstr
    | CreateHopProjectilesInstr
    | CreateLankyProjectilesInstr
    | PlaySoundContinuousInstr FilePath

data HurtType
    = NormalHurt
    | LaunchUpHurt
    | AirHurt
    | KneelingHurt
    | WallHurt
    deriving (Eq, Show)

data BossEnemyBehavior
    = SpawnBehavior
    | DeathBehavior
    | AirDeathBehavior
    | IdleBehavior Secs
    | AttackBehavior
    | LaunchedBehavior Secs
    | HurtBehavior Secs HurtType
    | KneelingBehavior Secs
    | GetUpBehavior
    | WallSplatBehavior Secs
    | GuardBehavior
    | AirGuardBehavior
    deriving (Eq, Show)

isIdleBehavior :: BossEnemyBehavior -> Bool
isIdleBehavior = \case
    IdleBehavior _ -> True
    _              -> False

isHurtBehavior :: BossEnemyBehavior -> Bool
isHurtBehavior = \case
    HurtBehavior _ _ -> True
    _                -> False

isWallSplatBehavior :: BossEnemyBehavior -> Bool
isWallSplatBehavior = \case
    WallSplatBehavior _ -> True
    _                   -> False

isWallHurtBehavior :: BossEnemyBehavior -> Bool
isWallHurtBehavior = \case
    HurtBehavior _ WallHurt -> True
    _                       -> False

isKneelingBehavior :: BossEnemyBehavior -> Bool
isKneelingBehavior = \case
    KneelingBehavior _ -> True
    _                  -> False

isLaunchedBehavior :: BossEnemyBehavior -> Bool
isLaunchedBehavior = \case
    LaunchedBehavior _ -> True
    _                  -> False

isAnyDeathBehavior :: BossEnemyBehavior -> Bool
isAnyDeathBehavior = \case
    DeathBehavior    -> True
    AirDeathBehavior -> True
    _                -> False

isAnyGuardBehavior :: BossEnemyBehavior -> Bool
isAnyGuardBehavior = \case
    GuardBehavior    -> True
    AirGuardBehavior -> True
    _                -> False

isIncapacitatedBehavior :: BossEnemyBehavior -> Bool
isIncapacitatedBehavior = \case
    LaunchedBehavior _  -> True
    HurtBehavior _ _    -> True
    KneelingBehavior _  -> True
    GetUpBehavior       -> True
    WallSplatBehavior _ -> True
    _                   -> False
