module Enemy.All.Blob.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Blob
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Blob.AI.Run
import Enemy.All.Blob.Behavior
import Enemy.All.Blob.Data
import Msg
import Util

thinkAI :: ConfigsRead m => EnemyThinkAI BlobEnemyData m
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
    in do
        aiEnabled <- not <$> readSettingsConfig _debug _disableAI

        return . flip execState [] $ do
            unless (behavior `elem` [SpawnBehavior, DeathBehavior] || inWallSplat || inHangtime) $
                modify (mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder:)

            let
                runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
                behaviorInstrs    = thinkBehaviorInstrs enemy
            modify (++ concatMap runBehaviorInstr' behaviorInstrs)

            modify (++ mkEnemyUpdatePrevBehaviorMsg enemy)

mkEnemyUpdatePrevBehaviorMsg :: Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdatePrevBehaviorMsg enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_prevBehavior = behavior}
    }
    where behavior = _behavior $ _data enemy

thinkAttackBehaviorInstrs :: Enemy BlobEnemyData -> [BlobEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy
    | atkFinished = case _behavior (_data enemy) of
        AttackBehavior numLoops
            | numLoops > 0 -> [StartAttackInstr $ Just (numLoops - 1)]
        _                  -> [StartIdleInstr]

    | otherwise = [UpdateAttackInstr]

    where atkFinished = maybe True _done (_attack enemy)

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy BlobEnemyData -> [BlobEnemyBehaviorInstr]
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

        enemyCfg                = _config $ _data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed enemyCfg
        inHangtimeVel           = enemyInHangtimeVel enemy enemyCfg
        minHangtimeSecs         = _minHangtimeSecs enemyCfg

thinkBehaviorInstrs :: Enemy BlobEnemyData -> [BlobEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstrEx idleTtl]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | enemyTouchingGround enemy         -> [StartAttackInstr Nothing]

    AttackBehavior _              -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> [StartLaunchedInstr hangtimeTtl]
        | hangtimeTtl > 0.0 && inHangtimeVel    -> [UpdateLaunchedInstr hangtimeTtl]

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
        | sprFinished -> [StartIdleInstrEx $ _postSpawnIdleSecs (_blob enemyCfg)]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]

    _ -> []

    where
        health        = E._health enemy
        enemyData     = _data enemy
        prevBehavior  = _prevBehavior enemyData
        sprFinished   = enemySpriteFinished enemy
        enemyCfg      = _config enemyData
        inHangtimeVel = enemyInHangtimeVel enemy enemyCfg
