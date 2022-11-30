module Enemy.All.Zombie.AI
    ( thinkAI
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execStateT, get, lift, modify, put, unless)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Zombie
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Zombie.AI.Run
import Enemy.All.Zombie.Behavior
import Enemy.All.Zombie.Data
import Msg
import Util
import Window.Graphics

projReleaseFrameTagName = FrameTagName "projRelease" :: FrameTagName

thinkAI :: (ConfigsRead m, MonadIO m) => EnemyThinkAI ZombieEnemyData m
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

        flip execStateT [] $ do
            unless (behavior `elem` [SpawnBehavior, DeathBehavior] || inWallSplat || inHangtime) $
                modify (mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder:)

            let
                runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
                behaviorInstrs    = thinkBehaviorInstrs enemy
            get >>= \msgs -> do
                behaviorMsgs <- concat <$> lift (traverse runBehaviorInstr' behaviorInstrs)
                put $ msgs ++ behaviorMsgs

            modify (++ mkEnemyUpdateDataMsgs enemy)

enemyGravityVel :: ZombieEnemyData -> Vel2
enemyGravityVel enemyData = Vel2 0.0 (gravity * timeStep)
    where gravity = _gravity $ _config enemyData

mkEnemyUpdateDataMsgs :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = _data e
    in e
        { _data = eData
            { _attackCooldown            = max 0.0 (_attackCooldown eData - timeStep)
            , _postSpawnNoAttackCooldown = max 0.0 (_postSpawnNoAttackCooldown eData - timeStep)
            , _prevBehavior              = prevBehavior
            }
        }
    where prevBehavior = _behavior $ _data enemy

canAttackPlayer :: Enemy ZombieEnemyData -> Bool
canAttackPlayer enemy = isAtkableBehavior && not isPostSpawnAtkCooldown && (atkOffCooldown || inForceAtkRange)
    where
        enemyData              = _data enemy
        isAtkableBehavior      = case _behavior enemyData of
            IdleBehavior _ _ -> True
            WalkBehavior _   -> True
            _                -> False
        isPostSpawnAtkCooldown = _postSpawnNoAttackCooldown enemyData > 0.0

        atkOffCooldown  = _attackCooldown enemyData <= 0.0
        inForceAtkRange = case enemyKnownPlayerPos enemy of
            Nothing                     -> False
            Just (Pos2 playerX playerY) ->
                let
                    Pos2 enemyX enemyY = E._pos enemy
                    forceAtkRangeX     = _forceAtkRangeX $ _zombie (_config enemyData)
                    inRangeX           = abs (playerX - enemyX) <= forceAtkRangeX
                    forceAtkRangeY     = _forceAtkRangeY $ _zombie (_config enemyData)
                    inRangeY           = abs (playerY - enemyY) <= forceAtkRangeY
                    facingPlayer       = case E._dir enemy of
                        LeftDir  -> playerX < enemyX
                        RightDir -> playerX > enemyX
                in inRangeX && inRangeY && facingPlayer

thinkAttackBehaviorInstrs :: AttackType -> Enemy ZombieEnemyData -> [ZombieEnemyBehaviorInstr]
thinkAttackBehaviorInstrs atkType enemy
    | atkFinished   = [ResetAtkCooldownInstr, StartIdleInstr]
    | dir' /= dir   = [ResetAtkCooldownInstr, FlipDirectionInstr, StartIdleInstr]
    | isProjRelease = [CreateAttackProjInstr atkType]
    | otherwise     = []
    where
        atkFinished = maybe True _done (_attack enemy)
        dir         = E._dir enemy
        dir'        = enemyFlippedDirIfWallOrGround enemy

        isProjRelease = case _attack enemy of
            Nothing  -> False
            Just atk -> projReleaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy ZombieEnemyData -> [ZombieEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    FallenHurt
        | sprFinished -> [StartFallenInstr hurtTtl]
    KnockDownHurt
        | sprFinished -> [StartFallenInstr hurtTtl]
    WallHurt
        | sprFinished -> [StartLaunchedInstr 0.0]

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed          -> [StartWallSplatInstr]
        | inAir && (sprFinished || (inHangtimeVel && hurtType == LaunchUpHurt)) -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                         -> [StartIdleInstr]
        | otherwise                                                             -> [UpdateHurtInstr hurtTtl hurtType]

    where
        enemyData               = _data enemy
        cfg                     = _config enemyData
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        minHangtimeSecs         = _minHangtimeSecs cfg

        inAir         = not $ enemyTouchingGround enemy
        touchingWall  = enemyTouchingWall enemy
        velX          = vecX $ _vel enemy
        sprFinished   = enemySpriteFinished enemy
        inHangtimeVel = enemyInHangtimeVel enemy cfg

thinkBehaviorInstrs :: Enemy ZombieEnemyData -> [ZombieEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]
        | canAttackPlayer enemy                            -> [StartAttackInstr]

    IdleBehavior idleTtl postIdleAction
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl postIdleAction]
        | onGround                          -> case postIdleAction of
            TurnAroundPostIdle -> [FlipDirectionInstr, StartIdleInstr]
            WalkPostIdle       -> [StartWalkInstr]

    WalkBehavior walkTtl
        | not (isWalkBehavior prevBehavior) -> [StartWalkInstr]
        | walkTtl > 0.0                     -> [UpdateWalkInstr walkTtl]
        | otherwise                         -> [StartIdleInstr]

    AttackBehavior atkType        -> thinkAttackBehaviorInstrs atkType enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> []
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
