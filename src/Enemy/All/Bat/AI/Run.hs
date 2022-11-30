module Enemy.All.Bat.AI.Run
    ( runBehaviorInstr
    ) where

import Collision
import Configs.All.Enemy
import Configs.All.Enemy.Bat
import Constants
import Enemy as E
import Enemy.All.Bat.AttackDescriptions
import Enemy.All.Bat.Behavior
import Enemy.All.Bat.Data
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> BatEnemyBehaviorInstr -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                         -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl                -> updateIdleBehavior idleTtl enemy
            StartPatrolInstr                       -> startPatrolBehavior enemy
            UpdatePatrolInstr                      -> updatePatrolBehavior enemy
            SetDirectionInstr dir                  -> setDirectionMessages dir enemy
            StartAttackInstr                       -> startAttackBehavior enemy
            UpdateHurtInstr hurtTtl hurtType       -> updateHurtBehavior hurtTtl hurtType enemy
            StartLaunchedInstr hangtimeTtl         -> startLaunchedBehavior hangtimeTtl enemy
            LaunchedInHangtimeInstr hangtimeTtl    -> launchedInHangtimeBehavior hangtimeTtl enemy
            LaunchedNotInHangtimeInstr hangtimeTtl -> launchedNotInHangtimeBehavior hangtimeTtl enemy
            StartFallenInstr fallenTtl             -> startFallenBehavior fallenTtl enemy
            UpdateFallenInstr fallenTtl            -> updateFallenBehavior fallenTtl enemy
            StartGetUpInstr                        -> startGetUpBehavior enemy
            StartFlyUpwardsInstr                   -> startFlyUpwardsBehavior enemy
            UpdateFlyUpwardsInstr                  -> updateFlyUpwardsBehavior enemy
            StartWallSplatInstr                    -> startWallSplatBehavior enemy
            UpdateWallSplatInstr wallSplatTtl      -> updateWallSplatBehavior wallSplatTtl enemy
            UpdateSpawnInstr                       -> updateSpawnBehavior enemy
            StartDeathInstr                        -> startDeathBehavior enemy
            SetDeadInstr                           -> enemySetDeadMessages enemy

        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior (_data enemy) of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                StartPatrolInstr  -> setIdleMsgs
                UpdatePatrolInstr -> setIdleMsgs
                StartAttackInstr  -> setIdleMsgs
                _                 -> aiEnabledMsgs

mkBatUpdateMsg :: Enemy BatEnemyData -> (Enemy BatEnemyData -> Enemy BatEnemyData) -> [Msg ThinkEnemyMsgsPhase]
mkBatUpdateMsg enemy update = mkEnemyUpdateMsg enemy (update . updatePrevBehavior)
    where
        updatePrevBehavior = \e -> e
            { _data =
                let eData = _data e
                in eData {_prevBehavior = _behavior eData}
            }

mkBatUpdateBehaviorMsg :: Enemy BatEnemyData -> BatEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkBatUpdateBehaviorMsg enemy behavior = mkBatUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = behavior}
    }

updateBehaviorIfMatching :: Enemy BatEnemyData -> BatEnemyBehavior -> BatEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (IdleBehavior _, IdleBehavior _)             -> behavior
    (HurtBehavior _ _, HurtBehavior _ _)         -> behavior
    (LaunchedBehavior _ _, LaunchedBehavior _ _) -> behavior
    (FallenBehavior _, FallenBehavior _)         -> behavior
    (WallSplatBehavior _, WallSplatBehavior _)   -> behavior
    _                                            -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

setDirectionMessages :: Direction -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
setDirectionMessages dir enemy = [mkMsgToEx (EnemyMsgSetDirection dir) enemyId MsgAfterNormalOrder]
    where enemyId = E._msgId enemy

updateSpawnBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []

updateHurtBehavior :: Secs -> HurtType -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkBatUpdateMsg enemy $ \e ->
    let
        hurtTtl' = hurtTtl - timeStep
        behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)
    in e
        { _data = (_data e) {_behavior = behavior}
        }

startIdleBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkBatUpdateMsg enemy $ \e -> e
    { _data   = (_data e) {_behavior = IdleBehavior idleSecs}
    , _vel    = zeroVel2
    , _attack = Nothing
    }
    where idleSecs = _idleSecs . _bat . _config $ _data enemy

updateIdleBehavior :: Secs -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkBatUpdateMsg enemy $ \e ->
    let
        idleTtl' = idleTtl - timeStep
        behavior = updateBehaviorIfMatching e (IdleBehavior idleTtl')
    in e
        { _data = (_data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

startLaunchedBehavior :: Secs -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeSecs enemy = mkBatUpdateBehaviorMsg enemy (LaunchedBehavior hangtimeSecs NotInHangtime)

launchedInHangtimeBehavior :: Secs -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedInHangtimeBehavior hangtimeTtl enemy = mkBatUpdateMsg enemy $ \e ->
    let
        hangtimeTtl' = hangtimeTtl - timeStep
        behavior     = updateBehaviorIfMatching e (LaunchedBehavior hangtimeTtl' InHangtime)
    in e
        { _data = (_data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

launchedNotInHangtimeBehavior :: Secs -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedNotInHangtimeBehavior hangtimeTtl enemy = mkBatUpdateMsg enemy $ \e ->
    let behavior = updateBehaviorIfMatching e (LaunchedBehavior hangtimeTtl NotInHangtime)
    in e {_data = (_data e) {_behavior = behavior}}

startWallSplatBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _bat enemyCfg

        updateEnemyMsg = mkBatUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        wallSplatTtl' = wallSplatTtl - timeStep
        behavior      = updateBehaviorIfMatching e (WallSplatBehavior wallSplatTtl')
    in e
        { _data = (_data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

startFlyUpwardsBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFlyUpwardsBehavior enemy = updateEnemyMsg ++ updateFlyUpwardsBehavior enemy
    where
        updateEnemyMsg = mkBatUpdateMsg enemy $ \e -> e
            {_data = (_data e) {_behavior = FlyUpwardsBehavior}}

updateFlyUpwardsBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFlyUpwardsBehavior enemy = mkBatUpdateMsg enemy $ \e -> e {_vel = vel}
    where
        riseRecoverVelY = _riseRecoverVelY . _bat $ _config (_data enemy)
        vel             = Vel2 0.0 riseRecoverVelY

startAttackBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior enemy = setAtkMsg:enemyUpdateMsg
    where
        atkDesc        = _attack1 $ _attackDescs (_data enemy)
        setAtkMsg      = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) (_msgId enemy)
        enemyUpdateMsg = mkBatUpdateBehaviorMsg enemy AttackBehavior

startFallenBehavior :: Secs -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkBatUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkBatUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkBatUpdateBehaviorMsg enemy GetUpBehavior

startDeathBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startDeathBehavior enemy = deathSoundMsg:enemyUpdateMsg
    where
        x            = vecX $ E._pos enemy
        centerY      = vecY $ hitboxCenter (enemyHitbox enemy)
        pos          = Pos2 x centerY
        deathSoundMsg = mkMsg $ AudioMsgPlaySound enemyDeathSoundPath pos

        enemyUpdateMsg = mkBatUpdateMsg enemy $ \e -> e
            { _data   = (_data e) {_behavior = DeathBehavior}
            , _vel    = zeroVel2
            , _attack = Nothing
            }

startPatrolBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
startPatrolBehavior enemy = mkBatUpdateBehaviorMsg enemy PatrolBehavior

updatePatrolBehavior :: Enemy BatEnemyData -> [Msg ThinkEnemyMsgsPhase]
updatePatrolBehavior enemy = mkBatUpdateMsg enemy $ \e ->
    let
        dir         = enemyFlippedDirIfWallOrGround e
        patrolSpeed = _patrolSpeed . _bat . _config $ _data e
        vel         = Vel2 (patrolSpeed * directionNeg dir) 0.0
    in e
        { _dir = dir
        , _vel = vel
        }
