module Enemy.All.Hop.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Hop.AI.Run
import Enemy.All.Hop.Behavior
import Enemy.All.Hop.Data
import InfoMsg.Util
import Msg
import Util
import Window.Graphics

jumpFrameTagName        = FrameTagName "jump"        :: FrameTagName
projReleaseFrameTagName = FrameTagName "projRelease" :: FrameTagName

thinkAI :: ConfigsRead m => EnemyThinkAI HopEnemyData m
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

            modify (++ mkEnemyUpdateDataMsgs enemy)

enemyGravityVel :: HopEnemyData -> Vel2
enemyGravityVel enemyData = Vel2 0.0 (gravity * timeStep)
    where gravity = _gravity $ _config enemyData

mkEnemyUpdateDataMsgs :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let prevBehavior = _behavior $ _data enemy
    in e
        { _data = (_data e) {_prevBehavior = prevBehavior}
        }

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy HopEnemyData -> [HopEnemyBehaviorInstr]
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
        velX           = vecX $ E._vel enemy
        touchingWall   = enemyTouchingWall enemy
        isLaunchUpHurt = hurtType == LaunchUpHurt

        cfg                     = _config $ _data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg
        minHangtimeSecs         = _minHangtimeSecs cfg

isFacingPlayer :: Enemy HopEnemyData -> Bool
isFacingPlayer enemy = case _knownPlayerInfo enemy of
    Nothing         -> False
    Just playerInfo ->
        let
            playerX = vecX $ playerInfoPos playerInfo
            enemyX  = vecX $ E._pos enemy
            dir     = E._dir enemy
        in (playerX < enemyX && dir == LeftDir) || (playerX > enemyX && dir == RightDir)

thinkBehaviorInstrs :: Enemy HopEnemyData -> [HopEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | onGround                          -> if
            | isFacingPlayer enemy -> [StartAttackHopLongInstr]
            | otherwise            -> [StartHopLongInstr]

    HopLongBehavior -> case E._sprite enemy of
        Nothing                                                            -> [StartIdleInstr]
        Just spr
            | spriteFinished spr && enemyTouchingGround enemy              -> [StartHopLongLandInstr]
            | jumpFrameTagName `isSpriteFrameTag` spr && _frameChanged spr -> [SetHopLongVelInstr]
            | enemyTouchingWall enemy                                      -> [FlipHopDirectionInstr]
            | otherwise                                                    -> []

    HopLongLandBehavior
        | sprFinished -> if
            | isFacingPlayer enemy -> [StartAttackHopShortInstr]
            | otherwise            -> [StartHopShortInstr]

    HopShortBehavior -> case E._sprite enemy of
        Nothing                                                            -> [StartIdleInstr]
        Just spr
            | spriteFinished spr && enemyTouchingGround enemy              -> [StartHopShortLandInstr]
            | jumpFrameTagName `isSpriteFrameTag` spr && _frameChanged spr -> [SetHopShortVelInstr]
            | enemyTouchingWall enemy                                      -> [FlipHopDirectionInstr]
            | otherwise                                                    -> []

    HopShortLandBehavior
        | enemySpriteFinished enemy -> if
            | isFacingPlayer enemy -> [StartAttackHopLongInstr]
            | otherwise            -> [StartHopLongInstr]

    AttackHopShortBehavior -> case enemyAttackSprite enemy of
        Nothing                                               -> [StartIdleInstr]
        Just spr
            | spriteFinished spr && enemyTouchingGround enemy -> [StartAttackHopShortLandInstr]
            | enemyTouchingWall enemy                         -> [FlipHopDirectionInstr]
            | otherwise                                       -> []

    AttackHopShortLandBehavior -> case _attack enemy of
        Nothing                                                                        -> [StartIdleInstr]
        Just atk
            | projReleaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [CreateAttackProjInstr]
            | _done atk || (isTauntedActive && attackIsLastFrameIndex atk)             -> if
                | isFacingPlayer enemy -> [StartAttackHopLongInstr]
                | otherwise            -> [ClearAttackInstr, StartHopLongInstr]
            | otherwise                                                                -> []

    AttackHopLongBehavior -> case _attack enemy of
        Nothing                                                                        -> [StartIdleInstr]
        Just atk
            | projReleaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> [CreateAttackProjInstr]
            | _done atk && enemyTouchingGround enemy                                   ->
                [ClearAttackInstr, StartAttackHopLongLandInstr]
            | enemyTouchingWall enemy                                                  -> [FlipHopDirectionInstr]
            | otherwise                                                                -> []

    AttackHopLongLandBehavior -> case _attack enemy of
        Nothing                                                            -> [StartIdleInstr]
        Just atk
            | _done atk || (isTauntedActive && attackIsLastFrameIndex atk) -> if
                | isFacingPlayer enemy -> [StartAttackHopShortInstr]
                | otherwise            -> [ClearAttackInstr, StartHopShortInstr]
            | otherwise                                                    -> []

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
        health          = E._health enemy
        enemyData       = _data enemy
        prevBehavior    = _prevBehavior enemyData
        sprFinished     = enemySpriteFinished enemy
        cfg             = _config enemyData
        inHangtimeVel   = enemyInHangtimeVel enemy cfg
        onGround        = enemyTouchingGround enemy
        isTauntedActive = enemyTauntedStatus enemy == EnemyTauntedActive
