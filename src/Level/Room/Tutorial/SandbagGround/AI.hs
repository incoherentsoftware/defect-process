module Level.Room.Tutorial.SandbagGround.AI
    ( thinkAI
    ) where

import Control.Monad       (unless)
import Control.Monad.State (execState, modify)

import Attack.Util
import Configs.All.Enemy
import Constants
import Enemy as E
import Level.Room.Tutorial.SandbagGround.AI.Run
import Level.Room.Tutorial.SandbagGround.Behavior
import Level.Room.Tutorial.SandbagGround.Data
import Msg
import Util

thinkAI :: Monad m => EnemyThinkAI SandbagGroundData m
thinkAI enemy =
    let
        enemyData  = _data enemy
        gravity    = _gravity $ _config enemyData
        gravityVel = Vel2 0.0 (gravity * timeStep)

        enemyId       = _msgId enemy
        behavior      = _behavior enemyData
        inWallSplat   = isWallSplatBehavior behavior || isWallHurtBehavior behavior
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        inHangtime    = inHangtimeVel && case behavior of
            LaunchedBehavior hangtimeTtl
                | hangtimeTtl > 0.0 -> True
            HurtBehavior _ AirHurt  -> True
            _                       -> False

        inMaterialize = case behavior of
            DematerializeBehavior -> True
            RematerializeBehavior -> True
            _                     -> False
    in return . flip execState [] $ do
        unless (behavior `elem` [SpawnBehavior, DeathBehavior] || inWallSplat || inHangtime || inMaterialize) $
            modify (mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder:)

        let
            runBehaviorInstr' = \cmd -> runBehaviorInstr cmd enemy
            behaviorInstrs    = thinkBehaviorInstrs enemy
        modify (++ concatMap runBehaviorInstr' behaviorInstrs)

        modify (++ mkEnemyUpdateDataMsg enemy)

mkEnemyUpdateDataMsg :: Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsg enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_prevBehavior = prevBehavior}
    }
    where prevBehavior = _behavior $ _data enemy

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy SandbagGroundData -> [SandbagGroundBehaviorInstr]
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

thinkBehaviorInstrs :: Enemy SandbagGroundData -> [SandbagGroundBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]

    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> []
        | hangtimeTtl > 0.0 && inHangtimeVel    -> [LaunchedHangtimeInstr hangtimeTtl]

    FallenBehavior fallenTtl
        | not (isFallenBehavior prevBehavior) -> [StartFallenInstr fallenTtl]
        | fallenTtl > 0.0                     -> [UpdateFallenInstr fallenTtl]
        | otherwise                           -> [StartDematerializeInstr]

    DematerializeBehavior
        | prevBehavior /= DematerializeBehavior -> [StartDematerializeInstr]
        | sprFinished                           -> [StartRematerializeInstr]

    RematerializeBehavior
        | prevBehavior /= RematerializeBehavior -> [StartRematerializeInstr]
        | sprFinished                           -> [StartIdleInstr]

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
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        sprFinished   = enemySpriteFinished enemy
