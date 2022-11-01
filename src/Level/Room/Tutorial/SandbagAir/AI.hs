module Level.Room.Tutorial.SandbagAir.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify)

import Attack.Util
import Configs.All.Enemy
import Constants
import Enemy as E
import Level.Room.Tutorial.SandbagAir.AI.Run
import Level.Room.Tutorial.SandbagAir.Behavior
import Level.Room.Tutorial.SandbagAir.Data
import Msg
import Util

thinkAI :: Monad m => EnemyThinkAI SandbagAirData m
thinkAI enemy = return . flip execState [] $ do
    modify (++ mkGravityMsg enemy)

    let
        runBehaviorInstr' = \cmd -> runBehaviorInstr cmd enemy
        behaviorInstrs    = thinkBehaviorInstrs enemy
    modify (++ concatMap runBehaviorInstr' behaviorInstrs)

    modify (++ mkEnemyUpdateDataMsg enemy)

mkGravityMsg :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
mkGravityMsg enemy = case _behavior (E._data enemy) of
    RematerializeBehavior                                           -> []
    HurtBehavior _ hurtType
        | hurtType `elem` [LaunchUpHurt, FallenHurt, KnockDownHurt] -> gravityMsg
    LaunchedBehavior _ NotInHangtime                                -> gravityMsg
    FallenBehavior _                                                -> gravityMsg
    _                                                               -> []
    where
        gravity    = _gravity $ _config (E._data enemy)
        gravityVel = Vel2 0.0 (gravity * timeStep)
        enemyId    = E._msgId enemy
        gravityMsg = [mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder]

mkEnemyUpdateDataMsg :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsg enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_prevBehavior = prevBehavior}
    }
    where prevBehavior = _behavior $ E._data enemy

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy SandbagAirData -> [SandbagAirBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    FlyingHurt
        | sprFinished && hurtTtl <= 0.0 -> [StartIdleInstr]
    FallenHurt
        | sprFinished                   -> [StartFallenInstr hurtTtl']

    WallHurt
        | sprFinished && hurtTtl <= 0.0 -> [StartLaunchedInstr 0.0]
        | otherwise                     -> [UpdateHurtInstr hurtTtl hurtType]

    -- let enemy hit ground
    KnockDownHurt -> []

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed -> [StartWallSplatInstr]
        | inAir && inHangtimeVel && hurtType == LaunchUpHurt           -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                -> case hurtType of
            FlyingHurt   -> [StartIdleInstr]
            LaunchedHurt -> [StartLaunchedInstr 0.0]
            _            -> [StartLaunchedInstr minHangtimeSecs]
        | otherwise                                                    -> [UpdateHurtInstr hurtTtl hurtType]

    where
        inAir                   = not $ enemyTouchingGround enemy
        touchingWall            = enemyTouchingWall enemy
        velX                    = vecX $ E._vel enemy
        sprFinished             = enemySpriteFinished enemy
        hurtTtl'                = hurtTtl - timeStep
        cfg                     = _config $ E._data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        minHangtimeSecs         = _minHangtimeSecs cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg

thinkBehaviorInstrs :: Enemy SandbagAirData -> [SandbagAirBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior                  -> []
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl _
        | hangtimeTtl > 0.0 && inHangtimeVel -> [LaunchedInHangtimeInstr hangtimeTtl]
        | otherwise                          -> [LaunchedNotInHangtimeInstr hangtimeTtl]

    FallenBehavior fallenTtl
        | fallenTtl > 0.0 -> [UpdateFallenInstr fallenTtl]
        | otherwise       -> [StartDematerializeInstr]

    WallSplatBehavior wallSplatTtl
        | wallSplatTtl > 0.0 -> [UpdateWallSplatInstr wallSplatTtl]
        | otherwise          -> [StartLaunchedInstr 0.0]

    DematerializeBehavior
        | prevBehavior /= DematerializeBehavior -> [StartDematerializeInstr]
        | sprFinished                           -> [StartRematerializeInstr]

    RematerializeBehavior
        | prevBehavior /= RematerializeBehavior -> [StartRematerializeInstr]
        | sprFinished                           -> [StartIdleInstr]

    SpawnBehavior
        | sprFinished -> [StartIdleInstr]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]

    _ -> []

    where
        health        = E._health enemy
        enemyData     = E._data enemy
        prevBehavior  = _prevBehavior enemyData
        sprFinished   = enemySpriteFinished enemy
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
