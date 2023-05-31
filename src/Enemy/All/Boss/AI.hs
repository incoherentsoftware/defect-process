module Enemy.All.Boss.AI
    ( thinkAI
    ) where

import Control.Monad.State (execStateT, modify)
import qualified Data.Map as M

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Boss
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Boss.AI.Run
import Enemy.All.Boss.AttackDescriptions
import Enemy.All.Boss.Behavior
import Enemy.All.Boss.Data
import Enemy.All.Boss.SeekWallDummyProjectile
import Enemy.All.Boss.Util
import InfoMsg.Util
import Msg
import Util
import Window.Graphics

releaseFrameTagName   = FrameTagName "release"   :: FrameTagName
teleportFrameTagName  = FrameTagName "teleport"  :: FrameTagName
fallFrameTagName      = FrameTagName "fall"      :: FrameTagName
fallSoundFrameTagName = FrameTagName "fallSound" :: FrameTagName

attackHammerFallSoundPath = "event:/SFX Events/Enemy/Boss/attack-hammer-fall" :: FilePath

thinkAI :: ConfigsRead m => EnemyThinkAI BossEnemyData m
thinkAI enemy = do
    aiEnabled <- not <$> readSettingsConfig _debug _disableAI

    flip execStateT [] $ do
        modify (++ mkGravityMsg enemy)

        let
            runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
            behaviorInstrs    = thinkBehaviorInstrs enemy
        modify (++ concatMap runBehaviorInstr' behaviorInstrs)

        modify (mkEnemyUpdateDataMsgs enemy ++)
        modify (mkSeekWallsMsgs enemy ++)

mkGravityMsg :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkGravityMsg enemy
    | behavior == SpawnBehavior || inWallSplat || isPhased || inHangtime = []
    | otherwise                                                          =
        let
            gravity    = _gravity $ _config (_data enemy)
            gravityVel = Vel2 0.0 (gravity * timeStep)
            enemyId    = E._msgId enemy
        in [mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder]
    where
        enemyData   = _data enemy
        behavior    = _behavior enemyData
        inWallSplat = isWallSplatBehavior behavior || isWallHurtBehavior behavior
        isPhased    = isPhasedHitbox enemy

        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        inHangtime    = inHangtimeVel && case behavior of
            LaunchedBehavior hangtimeTtl
                | hangtimeTtl > 0.0 -> True
            HurtBehavior _ AirHurt  -> True
            _                       -> False

mkEnemyUpdateDataMsgs :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData        = E._data e
        prevBehavior = _behavior $ E._data enemy

        incapacitatedSecs
            | isIncapacitatedBehavior prevBehavior = _incapacitatedSecs eData + timeStep
            | otherwise                            = 0.0
    in e
        { _data = eData
            { _incapacitatedSecs = incapacitatedSecs
            , _prevBehavior      = prevBehavior
            }
        }

mkSeekWallsMsgs :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkSeekWallsMsgs enemy
    | isUnknownWalls = [mkMsg $ NewThinkProjectileMsgAddM (mkSeekWallDummyProjectile pos enemyId)]
    | otherwise      = []
    where
        enemyData      = E._data enemy
        isUnknownWalls = _knownInnerLeftWallX enemyData `approxEq` _knownInnerRightWallX enemyData
        pos            = E._pos enemy
        enemyId        = E._msgId enemy

isAttackableBehavior :: Enemy BossEnemyData -> Bool
isAttackableBehavior enemy = case _behavior (_data enemy) of
    IdleBehavior _ -> True
    _              -> False

canAttackPlayerAtDistanceX :: Distance -> Enemy BossEnemyData -> Bool
canAttackPlayerAtDistanceX atkDistX enemy = case enemyKnownPlayerPos enemy of
    Just (Pos2 playerX _)
        | isAttackableBehavior enemy ->
            let
                x                 = vecX $ E._pos enemy
                dir               = E._dir enemy
                onGround          = enemyTouchingGround enemy
                facingPlayer
                    | playerX > x = dir == RightDir
                    | otherwise   = dir == LeftDir
                inAtkRange        = abs (playerX - x) <= atkDistX
            in onGround && facingPlayer && inAtkRange

    _ -> False

canAttackShortPlayer :: Enemy BossEnemyData -> Bool
canAttackShortPlayer enemy = canAttackPlayerAtDistanceX range enemy
    where range = _attackShortRange . _boss . _config $ _data enemy

canAttackMediumAirPlayer :: Enemy BossEnemyData -> Bool
canAttackMediumAirPlayer enemy = case _knownPlayerInfo enemy of
    Just playerInfo
        | isAttackableBehavior enemy ->
            let
                range         = _attackMediumRange . _boss . _config $ _data enemy
                isPlayerInAir = not $ _touchingGround (playerInfo :: PlayerInfo)
            in canAttackPlayerAtDistanceX range enemy && isPlayerInAir
    _                                -> False

canAttackLongPlayer :: Enemy BossEnemyData -> Bool
canAttackLongPlayer enemy = canAttackPlayerAtDistanceX range enemy
    where range = _attackLongRange . _boss . _config $ _data enemy

canHpThresholdAttackPlayer :: Enemy BossEnemyData -> Bool
canHpThresholdAttackPlayer enemy = isAnyThreshold && canAttack
    where
        hpThresholdAtkData = _hpThresholdAttackData $ _data enemy
        isAnyThreshold     = or [isAtUnusedHpThreshold t enemy | t <- M.toList (_thresholds hpThresholdAtkData)]
        canAttack          = canAttackShortPlayer enemy || canAttackMediumAirPlayer enemy || canAttackLongPlayer enemy

thinkAttackBehaviorInstrs :: Enemy BossEnemyData -> [BossEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Just atk
        | _done atk -> if
            | atk `attackIs` summonInAtkDesc -> [SetAttackInstr summonOutAtkDesc]
            | otherwise                      -> [StartIdleInstr]

        | _description atk == dogAtkDesc -> if
            | dir == LeftDir && _touchingLeftWall flags   -> [SetAttackInstr dogImpactAtkDesc]
            | dir == RightDir && _touchingRightWall flags -> [SetAttackInstr dogImpactAtkDesc]
            | otherwise                                   -> []

        | _description atk == blobAtkDesc -> if
            | releaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [CreateBlobProjectileInstr]
            | otherwise                                                            -> []

        | _description atk == hammerAtkDesc -> if
            | teleportFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [TeleportToPlayerInstr]
            | fallFrameTagName `isAttackFrameTag` atk && enemyTouchingGround enemy  ->
                [SetAttackInstr hammerImpactAtkDesc]
            | fallSoundFrameTagName `isAttackFrameTag` atk                          ->
                [PlaySoundContinuousInstr attackHammerFallSoundPath]
            | otherwise                                                             -> []

        | _description atk == turret1AtkDesc -> if
            | releaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [CreateTurret1ProjectileInstr]
            | otherwise                                                            -> []

        | _description atk == turret2AtkDesc -> if
            | releaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [CreateTurret2ProjectileInstr]
            | otherwise                                                            -> []

        | _description atk == hopAtkDesc -> if
            | releaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [CreateHopProjectilesInstr]
            | otherwise                                                            -> []

        | _description atk == lankyAtkDesc -> if
            | releaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [CreateLankyProjectilesInstr]
            | otherwise                                                            -> []

    _ -> []

    where
        flags     = _flags enemy
        dir       = E._dir enemy
        enemyData = E._data enemy

        SummonAttackDesc summonInAtkDesc summonOutAtkDesc = _summonAttackDesc $ _hpThresholdAttackData enemyData

        atkDescs            = _attackDescs enemyData
        dogAtkDesc          = _dog (atkDescs :: EnemyAttackDescriptions)
        dogImpactAtkDesc    = _dogImpact atkDescs
        blobAtkDesc         = _blob (atkDescs :: EnemyAttackDescriptions)
        hammerAtkDesc       = _hammer (atkDescs :: EnemyAttackDescriptions)
        hammerImpactAtkDesc = _hammerImpact (atkDescs :: EnemyAttackDescriptions)
        turret1AtkDesc      = _turret1 (atkDescs :: EnemyAttackDescriptions)
        turret2AtkDesc      = _turret2 (atkDescs :: EnemyAttackDescriptions)
        hopAtkDesc          = _hop (atkDescs :: EnemyAttackDescriptions)
        lankyAtkDesc        = _lanky (atkDescs :: EnemyAttackDescriptions)

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy BossEnemyData -> [BossEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    NormalHurt
        | hurtFinished -> [StartIdleInstr]
    KneelingHurt
        | hurtFinished -> [StartGetUpInstr]
    WallHurt
        | sprFinished  -> [StartWallSplatInstr hurtTtl']

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed          ->
            [StartWallSplatInstr minWallSplatSecs]
        | inAir && (sprFinished || (inHangtimeVel && hurtType == LaunchUpHurt)) -> [StartLaunchedInstr minHangtimeSecs]
        | inAir && hurtFinished                                                 -> [StartLaunchedInstr 0.0]
        | otherwise                                                             -> [UpdateHurtInstr hurtTtl hurtType]

    where
        sprFinished    = enemySpriteFinished enemy
        hurtFinished   = sprFinished && hurtTtl <= 0.0
        hurtTtl'       = hurtTtl - timeStep
        inAir          = not $ enemyTouchingGround enemy
        velX           = vecX $ E._vel enemy
        touchingWall   = enemyTouchingWall enemy

        cfg                     = _config $ _data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg
        minWallSplatSecs        = _minWallSplatSecs cfg
        minHangtimeSecs         = _minHangtimeSecs cfg

thinkBehaviorInstrs :: Enemy BossEnemyData -> [BossEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && not (isAnyDeathBehavior behavior) -> if
            | enemyTouchingGround enemy -> [StartDeathInstr]
            | otherwise                 -> [StartAirDeathInstr]

        | isHpThresholdSummonFlying enemy       -> [StartHpThresholdSummonFlyingInstr]
        | isHpThresholdSummonSpears enemy       -> [StartHpThresholdSummonSpearsInstr]
        | isHpThresholdSummonWalls enemy        -> [StartHpThresholdSummonWallsInstr]
        | isDoHpThresholdAttackFrame enemy      -> [StartHpThresholdAttackInstr]
        | isHpThresholdPhaseInAttackFrame enemy -> [TeleportToPreHpThresholdPosInstr]
        | canHpThresholdAttackPlayer enemy      -> [StartHpThresholdPhaseOutInstr]

        | canAttackShortPlayer enemy     -> [StartAttackShortInstr]
        | canAttackMediumAirPlayer enemy -> [StartAttackMediumAirInstr]
        | canAttackLongPlayer enemy      -> [StartAttackLongInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl, FacePlayerInstr]

    GuardBehavior
        | prevBehavior /= GuardBehavior -> [StartGuardInstr]
        | sprFinished                   -> [StartIdleInstr]

    AirGuardBehavior
        | prevBehavior /= AirGuardBehavior -> [StartAirGuardInstr]
        | sprFinished                      -> [StartIdleInstr]
        | isFallFrame                      -> []
        | otherwise                        -> [UpdateAirGuardInstr]

    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> [StartLaunchedInstr hangtimeTtl]
        | hangtimeTtl > 0.0 && inHangtimeVel    -> [LaunchedHangtimeInstr hangtimeTtl]

    KneelingBehavior kneelingTtl
        | kneelingTtl > 0.0 -> [UpdateKneelingInstr kneelingTtl]
        | otherwise         -> [StartGetUpInstr]

    GetUpBehavior
        | prevBehavior /= GetUpBehavior -> [StartGetUpInstr]
        | sprFinished                   -> [StartIdleInstr]

    WallSplatBehavior wallSplatTtl
        | not (isWallSplatBehavior prevBehavior) -> [StartWallSplatInstr minWallSplatSecs]
        | wallSplatTtl > 0.0                     -> [UpdateWallSplatInstr wallSplatTtl]
        | otherwise                              -> [StartLaunchedInstr 0.0]

    SpawnBehavior
        | sprFinished -> [StartIdleInstr]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]
        | otherwise   -> [UpdateDeathInstr]

    AirDeathBehavior
        | sprFinished -> [SetDeadInstr]
        | otherwise   -> [UpdateAirDeathInstr]

    _ -> []

    where
        health           = E._health enemy
        enemyData        = _data enemy
        prevBehavior     = _prevBehavior enemyData
        sprFinished      = enemySpriteFinished enemy
        cfg              = _config enemyData
        inHangtimeVel    = enemyInHangtimeVel enemy cfg
        isFallFrame      = maybe False (fallFrameTagName `isSpriteFrameTag`) (E._sprite enemy)
        minWallSplatSecs = _minWallSplatSecs cfg
