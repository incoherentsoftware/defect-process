module Enemy.All.Lanky.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Lanky
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Lanky.AI.Run
import Enemy.All.Lanky.AttackType
import Enemy.All.Lanky.Behavior
import Enemy.All.Lanky.Data
import InfoMsg.Util
import Msg
import Util
import Window.Graphics

attackSummonFrameTagName = FrameTagName "summon" :: FrameTagName

thinkAI :: ConfigsRead m => EnemyThinkAI LankyEnemyData m
thinkAI enemy = do
    aiEnabled <- not <$> readSettingsConfig _debug _disableAI

    return . flip execState [] $ do
        modify (++ mkGravityMsg enemy)

        let
            runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
            behaviorInstrs    = thinkBehaviorInstrs enemy
        modify (++ concatMap runBehaviorInstr' behaviorInstrs)

        modify (++ mkEnemyUpdateDataMsgs enemy)

mkGravityMsg :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkGravityMsg enemy
    | behavior `elem` [SpawnBehavior, DeathBehavior] || inWallSplat || inHangtime = []
    | otherwise                                                                   =
        let
            gravity    = _gravity $ _config (_data enemy)
            gravityVel = Vel2 0.0 (gravity * timeStep)
            enemyId    = E._msgId enemy
        in [mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder]
    where
        enemyData     = _data enemy
        behavior      = _behavior enemyData
        inWallSplat   = isWallSplatBehavior behavior || isWallHurtBehavior behavior
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        inHangtime    = inHangtimeVel && case behavior of
            LaunchedBehavior hangtimeTtl
                | hangtimeTtl > 0.0 -> True
            HurtBehavior _ AirHurt  -> True
            _                       -> False

mkEnemyUpdateDataMsgs :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData                    = E._data e
        lastKnownPlayerGroundPos = maybe (_lastKnownPlayerGroundPos eData) _groundBeneathPos (_knownPlayerInfo e)
    in e
        { _data = eData
            { _lastKnownPlayerGroundPos = lastKnownPlayerGroundPos
            , _summonAtkCooldownTtl     = max 0.0 (_summonAtkCooldownTtl eData - timeStep)
            , _beamAtkCooldownTtl       = max 0.0 (_beamAtkCooldownTtl eData - timeStep)
            , _prevBehavior             = prevBehavior
            }
        }
    where prevBehavior = _behavior $ E._data enemy

inAttackRange :: LankyEnemyAttackType -> Enemy LankyEnemyData -> Bool
inAttackRange atkType enemy = case _knownPlayerInfo enemy of
    Nothing         -> False
    Just playerInfo ->
        let
            playerX = vecX $ playerInfoPos playerInfo
            x       = vecX $ E._pos enemy
            cfg     = _lanky $ _config (E._data enemy)

            atkRangeX   = case atkType of
                SummonAttackType -> _summonAtkRangeX cfg
                BeamAttackType   -> _beamAtkRangeX cfg
            inAtkRangeX = abs (playerX - x) <= atkRangeX
        in case atkType of
            SummonAttackType -> inAtkRangeX
            BeamAttackType   -> inAtkRangeX && _touchingGround (playerInfo :: PlayerInfo)

canAttackPlayer :: LankyEnemyAttackType -> Enemy LankyEnemyData -> Bool
canAttackPlayer atkType enemy = isAttackableBehavior && onGround && offCooldown && inAttackRange atkType enemy
    where
        enemyData            = E._data enemy
        isAttackableBehavior = case _behavior enemyData of
            IdleBehavior _    -> True
            WalkBehavior _    -> True
            RetreatBehavior _ -> True
            _                 -> False

        onGround    = enemyTouchingGround enemy
        offCooldown = case atkType of
            SummonAttackType -> _summonAtkCooldownTtl enemyData <= 0.0
            BeamAttackType   -> _beamAtkCooldownTtl enemyData <= 0.0

thinkAttackBehaviorInstrs :: Enemy LankyEnemyData -> [LankyEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy
    | atkFinished = if
        | inAttackRange SummonAttackType enemy -> [StartRetreatInstr, SetBeamAtkCooldownInstr]
        | otherwise                            -> [StartWalkInstr, SetBeamAtkCooldownInstr]

    | isSummonFrame = [CreateAttackPillarInstr, SetSummonAtkCooldownInstr]
    | otherwise     = []
    where
        atkFinished   = maybe True _done (_attack enemy)
        isSummonFrame = case enemyAttackSprite enemy of
            Just atkSpr -> _frameChanged atkSpr && attackSummonFrameTagName `isSpriteFrameTag` atkSpr
            Nothing     -> False

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy LankyEnemyData -> [LankyEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    NormalHurt
        | hurtFinished -> [StartIdleInstr]
    KneelingHurt
        | hurtFinished -> [StartGetUpInstr]
    WallHurt
        | sprFinished  -> [StartWallSplatInstr hurtTtl']

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed -> [StartWallSplatInstr minWallSplatSecs]
        | inAir && (sprFinished || (inHangtimeVel && isLaunchUpHurt))  -> [StartLaunchedInstr minHangtimeSecs]
        | inAir && hurtFinished                                        -> [StartLaunchedInstr 0.0]
        | otherwise                                                    -> [UpdateHurtInstr hurtTtl hurtType]

    where
        cfg                     = _config $ _data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        minHangtimeSecs         = _minHangtimeSecs cfg
        minWallSplatSecs        = _minWallSplatSecs cfg

        sprFinished    = enemySpriteFinished enemy
        hurtFinished   = sprFinished && hurtTtl <= 0.0
        hurtTtl'       = hurtTtl - timeStep
        inAir          = not $ enemyTouchingGround enemy
        velX           = vecX $ E._vel enemy
        touchingWall   = enemyTouchingWall enemy
        inHangtimeVel  = enemyInHangtimeVel enemy cfg
        isLaunchUpHurt = hurtType == LaunchUpHurt

thinkBehaviorInstrs :: Enemy LankyEnemyData -> [LankyEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

        | canAttackPlayer BeamAttackType enemy   -> [StartAttackInstr BeamAttackType, SetSummonAtkCooldownInstr]
        | canAttackPlayer SummonAttackType enemy -> [StartAttackInstr SummonAttackType]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior)                                                -> [StartIdleInstr]
        | idleTtl > 0.0                                                                    -> [UpdateIdleInstr idleTtl]
        | Just playerX <- vecX <$> enemyKnownPlayerPos enemy, abs (playerX - x) > atkRange ->
            [FacePlayerInstr, StartWalkInstr]

    WalkBehavior walkTtl
        | not (isWalkBehavior prevBehavior) -> [StartWalkInstr]
        | walkTtl > 0.0                     -> [UpdateWalkInstr walkTtl]
        | otherwise                         -> [StartIdleInstr]

    RetreatBehavior retreatTtl
        | not (isRetreatBehavior prevBehavior) -> [StartRetreatInstr]
        | retreatTtl > 0.0                     -> [UpdateRetreatInstr retreatTtl]
        | otherwise                            -> [StartIdleInstr]

    AuraBreakBehavior
        | sprFinished -> [StartIdleInstr]

    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> [StartLaunchedInstr hangtimeTtl]
        | hangtimeTtl > 0.0 && inHangtimeVel    -> [LaunchedHangtimeInstr hangtimeTtl]

    KneelingBehavior kneelingTtl
        | kneelingTtl > 0.0 -> [UpdateKneelingInstr kneelingTtl]
        | otherwise         -> [StartGetUpInstr]

    GetUpBehavior
        | sprFinished -> [StartIdleInstr]

    WallSplatBehavior wallSplatTtl
        | wallSplatTtl > 0.0 -> [UpdateWallSplatInstr wallSplatTtl]
        | otherwise          -> [StartLaunchedInstr 0.0]

    SpawnBehavior
        | sprFinished -> [StartIdleInstr]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]

    _ -> []

    where
        health        = E._health enemy
        x             = vecX $ E._pos enemy
        enemyData     = _data enemy
        prevBehavior  = _prevBehavior enemyData
        sprFinished   = enemySpriteFinished enemy
        cfg           = _config enemyData
        inHangtimeVel = enemyInHangtimeVel enemy cfg
        atkRange      = _summonAtkRangeX $ _lanky cfg
