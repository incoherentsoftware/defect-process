module Enemy.All.Flail.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Flail
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Flail.AI.Run
import Enemy.All.Flail.AttackDescriptions
import Enemy.All.Flail.Behavior
import Enemy.All.Flail.Data
import Msg
import Util
import Window.Graphics

flailPullUpSfxFrameTagName   = FrameTagName "flail-pull-up-sfx"                   :: FrameTagName
flailPullDownSfxFrameTagName = FrameTagName "flail-pull-down-sfx"                 :: FrameTagName
flailPullUpSoundPath         = "event:/SFX Events/Enemy/Flail/flail-pull-up-down" :: FilePath
flailPullDownSoundPath       = "event:/SFX Events/Enemy/Flail/flail-pull-up-down" :: FilePath

enemyGravityVel :: FlailEnemyData -> Vel2
enemyGravityVel enemyData = Vel2 0.0 (gravity * timeStep)
    where gravity = _gravity $ _config enemyData

thinkAI :: ConfigsRead m => EnemyThinkAI FlailEnemyData m
thinkAI enemy =
    let
        enemyData     = _data enemy
        gravityVel    = enemyGravityVel enemyData
        enemyId       = _msgId enemy
        behavior      = _behavior enemyData
        inWallSplat   = isWallSplatBehavior behavior || isWallHurtBehavior behavior
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        inHangtime    = inHangtimeVel && case behavior of
            LaunchedBehavior hangtimeTtl
                | hangtimeTtl > 0.0 -> True
            HurtBehavior _ AirHurt  -> True
            _                       -> False
    in do
        aiEnabled <- not <$> readSettingsConfig _debug _disableAI

        return . flip execState [] $ do
            unless (behavior `elem` [SpawnBehavior, DeathBehavior] || inWallSplat || inHangtime) $
                modify (mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder:)

            let
                runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
                behaviorInstrs    = thinkBehaviorInstrs enemy
            modify (++ concatMap runBehaviorInstr' behaviorInstrs)

            modify (mkEnemyUpdateDataMsgs enemy ++)
            modify (mkAdditionalAudioMsgs enemy ++)

mkAdditionalAudioMsgs :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkAdditionalAudioMsgs enemy = case _attack enemy of
    Just atk
        | flailPullUpSfxFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk   ->
            [mkMsg $ AudioMsgPlaySound flailPullUpSoundPath pos]
        | flailPullDownSfxFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk ->
            [mkMsg $ AudioMsgPlaySound flailPullDownSoundPath pos]
    _                                                                                   -> []
    where pos = E._pos enemy

mkEnemyUpdateDataMsgs :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = _data e
    in e
        { _data = eData
            { _attackCooldownTtl = max 0.0 (_attackCooldownTtl eData - timeStep)
            , _prevBehavior      = prevBehavior
            }
        }
    where prevBehavior = _behavior $ _data enemy

isAttackableBehavior :: Enemy FlailEnemyData -> Bool
isAttackableBehavior enemy
    | _attackCooldownTtl enemyData > 0.0 = False
    | otherwise                          = case _behavior enemyData of
        IdleBehavior _    -> True
        WalkBehavior _    -> True
        RetreatBehavior _ -> True
        _                 -> False
    where enemyData = _data enemy

canAttackPlayerAtDistanceX :: Distance -> Enemy FlailEnemyData -> Bool
canAttackPlayerAtDistanceX atkDistX enemy = case enemyKnownPlayerPos enemy of
    Just (Pos2 playerX _)
        | isAttackableBehavior enemy ->
            let
                x          = vecX $ E._pos enemy
                onGround   = enemyTouchingGround enemy
                inAtkRange = abs (playerX - x) <= atkDistX
            in onGround && inAtkRange

    _ -> False

canAttackForwardsPlayer :: Enemy FlailEnemyData -> Bool
canAttackForwardsPlayer enemy = canAttackPlayerAtDistanceX rangeX enemy && isEnemyFacingPlayer enemy
    where rangeX = _attackForwardsRangeX . _flail . _config $ _data enemy

canAttackDiagUpwardsPlayer :: Enemy FlailEnemyData -> Bool
canAttackDiagUpwardsPlayer enemy = canAttackPlayerAtDistanceX rangeX enemy && inRangeY && isEnemyFacingPlayer enemy
    where
        cfg      = _flail $ _config (_data enemy)
        rangeX   = _attackDiagUpwardsRangeX cfg
        inRangeY = case enemyKnownPlayerPos enemy of
            Just (Pos2 _ playerY) ->
                let Pos2 _ y = E._pos enemy
                in y - playerY >= _attackDiagUpwardsMinDistY cfg
            Nothing               -> False

canAttackUpwardsPlayer :: Enemy FlailEnemyData -> Bool
canAttackUpwardsPlayer enemy = canAttackPlayerAtDistanceX rangeX enemy && inRangeY
    where
        cfg      = _flail $ _config (_data enemy)
        rangeX   = _attackUpwardsRangeX cfg
        inRangeY = case enemyKnownPlayerPos enemy of
            Just (Pos2 _ playerY) ->
                let Pos2 _ y = E._pos enemy
                in y - playerY >= _attackUpwardsMinDistY cfg
            Nothing               -> False

thinkAttackBehaviorInstrs :: Enemy FlailEnemyData -> [FlailEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy
    | atkFinished = [StartIdleToRetreatInstr, SetAttackCooldownInstr]
    | otherwise   = []
    where atkFinished = maybe True _done (_attack enemy)

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy FlailEnemyData -> [FlailEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    FallenHurt
        | sprFinished -> [StartFallenInstr hurtTtl']
    KnockDownHurt
        | sprFinished -> [StartFallenInstr hurtTtl']
    WallHurt
        | sprFinished -> [StartLaunchedInstr 0.0]

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed -> [StartWallSplatInstr]
        | inAir && (sprFinished || (inHangtimeVel && isLaunchUpHurt))  -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                -> [StartIdleInstr]
        | otherwise                                                    -> [UpdateHurtInstr hurtTtl hurtType]

    where
        sprFinished    = enemySpriteFinished enemy
        hurtTtl'       = hurtTtl - timeStep
        inAir          = not $ enemyTouchingGround enemy
        velX           = vecX $ _vel enemy
        touchingWall   = enemyTouchingWall enemy
        isLaunchUpHurt = hurtType == LaunchUpHurt

        cfg                     = _config $ _data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg
        minHangtimeSecs         = _minHangtimeSecs cfg

thinkBehaviorInstrs :: Enemy FlailEnemyData -> [FlailEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]
        | canAttackUpwardsPlayer enemy                     -> [StartAttackInstr upwardsAtkDesc]
        | canAttackDiagUpwardsPlayer enemy                 -> [StartAttackInstr diagUpwardsAtkDesc]
        | canAttackForwardsPlayer enemy                    -> [StartAttackInstr forwardsAtkDesc]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | onGround                          -> [FacePlayerInstr, StartIdleToWalkInstr]

    WalkBehavior walkTtl
        | not (isWalkBehavior prevBehavior) -> [StartWalkInstr]
        | walkTtl > 0.0                     -> [UpdateWalkInstr walkTtl]
        | otherwise                         -> [StartWalkToIdleInstr]

    RetreatBehavior retreatTtl
        | not (isRetreatBehavior prevBehavior) -> [StartRetreatInstr]
        | retreatTtl > 0.0                     -> [UpdateRetreatInstr retreatTtl]
        | otherwise                            -> [StartWalkToIdleInstr]

    IdleToWalkBehavior
        | prevBehavior /= IdleToWalkBehavior -> [StartIdleToWalkInstr]
        | sprFinished                        -> [StartWalkInstr]

    IdleToRetreatBehavior
        | prevBehavior /= IdleToRetreatBehavior -> [StartIdleToRetreatInstr]
        | sprFinished                           -> [FacePlayerInstr, StartRetreatInstr]

    WalkToIdleBehavior
        | prevBehavior /= WalkToIdleBehavior -> [StartWalkToIdleInstr]
        | sprFinished                        -> [StartIdleInstr]

    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> [StartLaunchedInstr hangtimeTtl]
        | hangtimeTtl > 0.0 && inHangtimeVel    -> [LaunchedHangtimeInstr hangtimeTtl]

    FallenBehavior fallenTtl
        | not (isFallenBehavior prevBehavior) -> [StartFallenInstr fallenTtl]
        | fallenTtl > 0.0                     -> [UpdateFallenInstr fallenTtl]
        | otherwise                           -> [StartGetUpInstr]

    GetUpBehavior
        | prevBehavior /= GetUpBehavior -> [StartGetUpInstr]
        | sprFinished                   -> [StartIdleInstr]

    WallSplatBehavior wallSplatTtl
        | not (isWallSplatBehavior prevBehavior) -> [StartWallSplatInstr]
        | wallSplatTtl > 0.0                     -> [UpdateWallSplatInstr wallSplatTtl]
        | otherwise                              -> [StartLaunchedInstr 0.0]

    SpawnBehavior
        | sprFinished -> [StartIdleInstr]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]

    _ -> []

    where
        health        = E._health enemy
        enemyData     = _data enemy
        prevBehavior  = _prevBehavior enemyData
        sprFinished   = enemySpriteFinished enemy
        cfg           = _config enemyData
        inHangtimeVel = enemyInHangtimeVel enemy cfg
        onGround      = enemyTouchingGround enemy

        atkDescs           = _attackDescs enemyData
        upwardsAtkDesc     = _upwards atkDescs
        diagUpwardsAtkDesc = _diagUpwards atkDescs
        forwardsAtkDesc    = _forwards atkDescs
