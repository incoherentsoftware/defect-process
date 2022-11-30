module Enemy.All.Wall.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Wall
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Wall.AI.Run
import Enemy.All.Wall.Behavior
import Enemy.All.Wall.Data
import InfoMsg.Util
import Msg
import Util
import Window.Graphics

projReleaseFrameTagName = FrameTagName "projRelease" :: FrameTagName

thinkAI :: ConfigsRead m => EnemyThinkAI WallEnemyData m
thinkAI enemy =
    let
        enemyData     = _data enemy
        gravityVel    = enemyGravityVel enemyData
        enemyId       = E._msgId enemy
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

enemyGravityVel :: WallEnemyData -> Vel2
enemyGravityVel enemyData = Vel2 0.0 (gravity * timeStep)
    where gravity = _gravity $ _config enemyData

mkEnemyUpdateDataMsgs :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData        = _data e
        atkCooldown  = max 0.0 (_attackCooldown eData - timeStep)
        prevBehavior = _behavior $ _data enemy
    in e
        { _data = eData
            { _attackCooldown = atkCooldown
            , _prevBehavior   = prevBehavior
            }
        }

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy WallEnemyData -> [WallEnemyBehaviorInstr]
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
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed $ _config enemyData
        minHangtimeSecs         = _minHangtimeSecs cfg

        inAir         = not $ enemyTouchingGround enemy
        touchingWall  = enemyTouchingWall enemy
        velX          = vecX $ E._vel enemy
        sprFinished   = enemySpriteFinished enemy
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)

canAttackPlayer :: Enemy WallEnemyData -> Bool
canAttackPlayer enemy = enemyTouchingGround enemy && atkCooldown <= 0.0 && isAttackableBehavior
    where
        enemyData            = _data enemy
        atkCooldown          = _attackCooldown enemyData
        isAttackableBehavior = case _behavior enemyData of
            IdleBehavior _ -> True
            _              -> False

thinkAttackBehaviorInstrs :: Enemy WallEnemyData -> [WallEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Just atk
        | _done atk                                                                -> [StartIdleInstr]
        | projReleaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [CreateAttackProjInstr]
        | otherwise                                                                -> []
    Nothing                                                                        -> [StartIdleInstr]

thinkBehaviorInstrs :: Enemy WallEnemyData -> [WallEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]
        | canAttackPlayer enemy                            -> [StartAttackInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | onGround                          -> case _knownPlayerInfo enemy of
            Nothing         -> []
            Just playerInfo ->
                let
                    enemyX                      = vecX $ E._pos enemy
                    playerX                     = vecX $ playerInfoPos playerInfo
                    dist                        = abs $ enemyX - playerX
                    walkPlayerDistanceThreshold = _walkPlayerDistanceThreshold . _wall . _config $ _data enemy
                in if
                    | dist <= walkPlayerDistanceThreshold -> [FacePlayerInstr, StartBackWalkInstr]
                    | otherwise                           -> [FacePlayerInstr, StartWalkInstr]

    WalkBehavior walkTtl
        | not (isWalkBehavior prevBehavior) -> [StartWalkInstr]
        | walkTtl > 0.0                     -> [UpdateWalkInstr walkTtl]
        | otherwise                         -> [StartIdleInstr]

    BackWalkBehavior backWalkTtl
        | not (isBackWalkBehavior prevBehavior) -> [StartBackWalkInstr]
        | backWalkTtl > 0.0                     -> [UpdateBackWalkInstr backWalkTtl]
        | otherwise                             -> [StartIdleInstr]

    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
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
