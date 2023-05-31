module Enemy.All.Bat.AI
    ( thinkAI
    ) where

import Control.Monad.State (execStateT, modify)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Bat.AI.Run
import Enemy.All.Bat.Behavior
import Enemy.All.Bat.Data
import Msg
import Util

thinkAI :: ConfigsRead m => EnemyThinkAI BatEnemyData m
thinkAI enemy = do
    aiEnabled <- not <$> readSettingsConfig _debug _disableAI

    flip execStateT [] $ do
        modify (++ mkGravityMsg enemy)

        let
            runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
            behaviorInstrs    = thinkBehaviorInstrs enemy
        modify (++ concatMap runBehaviorInstr' behaviorInstrs)

mkGravityMsg :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkGravityMsg enemy = case _behavior (_data enemy) of
    HurtBehavior _ hurtType
        | hurtType `elem` [LaunchUpHurt, FallenHurt, KnockDownHurt] -> gravityMsg
    LaunchedBehavior _ NotInHangtime                                -> gravityMsg
    FallenBehavior _                                                -> gravityMsg
    _                                                               -> []
    where
        gravity    = _gravity $ _config (_data enemy)
        gravityVel = Vel2 0.0 (gravity * timeStep)
        enemyId    = _msgId enemy
        gravityMsg = [mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder]

isBelowStartPosY :: Enemy BatEnemyData -> Bool
isBelowStartPosY enemy = vecY (E._pos enemy) > _startPosY (_data enemy)

canAttackPlayer :: Enemy BatEnemyData -> Bool
canAttackPlayer enemy = case _behavior (_data enemy) of
    PatrolBehavior atkCooldownTtl -> atkCooldownTtl <= 0.0
    _                             -> False

thinkAttackBehaviorInstrs :: Enemy BatEnemyData -> [BatEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Just atk
        | _done atk   -> [StartPatrolInstr]
    _
        | dir' /= dir -> [SetDirectionInstr dir']
        | otherwise   -> []
    where
        dir  = E._dir enemy
        dir' = enemyFlippedDirIfWallOrGround enemy

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy BatEnemyData -> [BatEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    AirHurt
        | sprFinished && hurtTtl <= 0.0 -> if
            | isBelowStartPosY enemy -> [StartFlyUpwardsInstr]
            | otherwise              -> [StartIdleInstr]
    FallenHurt
        | sprFinished -> [StartFallenInstr hurtTtl']

    WallHurt
        | sprFinished && hurtTtl <= 0.0 -> [StartLaunchedInstr 0.0]
        | otherwise                     -> [UpdateHurtInstr hurtTtl hurtType]

    -- let enemy hit ground
    KnockDownHurt -> []

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed -> [StartWallSplatInstr]
        | inAir && inHangtimeVel && hurtType == LaunchUpHurt           -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                -> case hurtType of
            AirHurt      -> [StartIdleInstr]
            LaunchedHurt -> [StartLaunchedInstr 0.0]
            _            -> [StartLaunchedInstr minHangtimeSecs]
        | otherwise                                                    -> [UpdateHurtInstr hurtTtl hurtType]

    where
        inAir                   = not $ enemyTouchingGround enemy
        touchingWall            = enemyTouchingWall enemy
        velX                    = vecX $ _vel enemy
        sprFinished             = enemySpriteFinished enemy
        hurtTtl'                = hurtTtl - timeStep
        enemyData               = _data enemy
        cfg                     = _config enemyData
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        minHangtimeSecs         = _minHangtimeSecs cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg

thinkBehaviorInstrs :: Enemy BatEnemyData -> [BatEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | canAttackPlayer enemy                            -> [StartAttackInstr]
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | otherwise                         -> [StartPatrolInstr]

    PatrolBehavior atkCooldownTtl
        | not (isPatrolBehavior prevBehavior) -> [StartPatrolInstr]
        | otherwise                           -> [UpdatePatrolInstr atkCooldownTtl]

    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl _
        | not (isLaunchedBehavior prevBehavior) -> [StartLaunchedInstr hangtimeTtl]
        | hangtimeTtl > 0.0 && inHangtimeVel    -> [LaunchedInHangtimeInstr hangtimeTtl]
        | otherwise                             -> [LaunchedNotInHangtimeInstr hangtimeTtl]

    FallenBehavior fallenTtl
        | not (isFallenBehavior prevBehavior) -> [StartFallenInstr fallenTtl]
        | fallenTtl > 0.0                     -> [UpdateFallenInstr fallenTtl]
        | otherwise                           -> [StartGetUpInstr]

    GetUpBehavior
        | prevBehavior /= GetUpBehavior -> [StartGetUpInstr]
        | sprFinished                   -> [StartFlyUpwardsInstr]

    FlyUpwardsBehavior
        | isBelowStartPosY enemy -> [UpdateFlyUpwardsInstr]
        | otherwise              -> [StartIdleInstr]

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
