module Enemy.All.Blob.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)

import AppEnv
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Blob
import Constants
import Enemy as E
import Enemy.All.Blob.AttackDescriptions
import Enemy.All.Blob.Behavior
import Enemy.All.Blob.Data
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> BlobEnemyBehaviorInstr -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                    -> startIdleBehavior enemy
            StartIdleInstrEx idleSecs         -> startIdleBehaviorEx idleSecs enemy
            UpdateIdleInstr idleTtl           -> updateIdleBehavior idleTtl enemy
            StartAttackInstr numLoops         -> startAttackBehavior numLoops enemy
            UpdateAttackInstr                 -> updateAttackBehavior enemy
            UpdateHurtInstr hurtTtl hurtType  -> updateHurtBehavior hurtTtl hurtType enemy
            StartLaunchedInstr hangtimeTtl    -> startLaunchedBehavior hangtimeTtl enemy
            UpdateLaunchedInstr hangtimeTtl   -> updateLaunchedBehavior hangtimeTtl enemy
            StartFallenInstr fallenTtl        -> startFallenBehavior fallenTtl enemy
            UpdateFallenInstr fallenTtl       -> updateFallenBehavior fallenTtl enemy
            StartGetUpInstr                   -> startGetUpBehavior enemy
            StartWallSplatInstr               -> startWallSplatBehavior enemy
            UpdateWallSplatInstr wallSplatTtl -> updateWallSplatBehavior wallSplatTtl enemy
            UpdateSpawnInstr                  -> updateSpawnBehavior enemy
            StartDeathInstr                   -> startDeathBehavior enemy
            SetDeadInstr                      -> enemySetDeadMessages enemy

        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior (_data enemy) of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                StartAttackInstr _ -> setIdleMsgs
                UpdateAttackInstr  -> setIdleMsgs
                _                  -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy BlobEnemyData -> BlobEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = behavior}
    }

mkEnemyUpdateBehaviorMsgM
    :: Enemy BlobEnemyData
    -> AppEnv UpdateEnemyMsgsPhase BlobEnemyBehavior
    -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsgM enemy behavior = [mkMsgTo (EnemyMsgUpdateM update) enemyId]
    where
        update  = \e -> do
            behavior' <- behavior
            return $ e
                { _data = (_data e) {_behavior = behavior'}
                }
        enemyId = _msgId enemy

updateBehaviorIfMatching :: Enemy BlobEnemyData -> BlobEnemyBehavior -> BlobEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

updateSpawnBehavior :: Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []

startDeathBehavior :: Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
startDeathBehavior enemy = deathSoundMsg:updateMsg
    where
        x             = vecX $ E._pos enemy
        centerY       = vecY $ hitboxCenter (enemyHitbox enemy)
        pos           = Pos2 x centerY
        deathSoundMsg = mkMsg $ AudioMsgPlaySound enemyDeathSoundPath pos

        updateMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data   = (_data e) {_behavior = DeathBehavior}
            , _vel    = zeroVel2
            , _attack = Nothing
            }

startLaunchedBehavior :: Secs -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

updateLaunchedBehavior :: Secs -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _blob enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior wallSplatTtl'}
    , _vel  = zeroVel2
    }
    where wallSplatTtl' = wallSplatTtl - timeStep

updateHurtBehavior :: Secs -> HurtType -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where
        hurtTtl' = hurtTtl - timeStep
        behavior = HurtBehavior hurtTtl' hurtType

mkAttackMsg :: Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkAttackMsg enemy = [mkMsgTo (EnemyMsgSetAttackDesc attackMoveDesc) enemyId]
    where
        attackMoveDesc = _attackMove $ _attackDescs (_data enemy)
        enemyId        = _msgId enemy

startAttackBehavior :: Maybe Int -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior numLoops enemy = behaviorMsg ++ mkAttackMsg enemy
    where
        behaviorMsg = case numLoops of
            Just loops -> mkEnemyUpdateBehaviorMsg enemy (AttackBehavior loops)
            Nothing    ->
                let
                    cfg                = _blob . _config $ _data enemy
                    minAttackMoveLoops = _minAttackMoveLoops cfg
                    maxAttackMoveLoops = _maxAttackMoveLoops cfg
                    atkBehavior        = liftIO $ AttackBehavior <$> randomRIO (minAttackMoveLoops, maxAttackMoveLoops)
                in mkEnemyUpdateBehaviorMsgM enemy atkBehavior

updateAttackBehavior :: Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateAttackBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e {_dir = enemyFlippedDirIfWallOrGround e}

startIdleBehavior :: Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = startIdleBehaviorEx idleSecs enemy
    where idleSecs = _idleSecs . _blob . _config $ _data enemy

startIdleBehaviorEx :: Secs -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehaviorEx idleSecs enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data   = (_data e) {_behavior = IdleBehavior idleSecs}
    , _vel    = Vel2 0.0 (vecY $ _vel e)
    , _attack = Nothing
    }

updateIdleBehavior :: Secs -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy (IdleBehavior idleTtl')
    where idleTtl' = idleTtl - timeStep

startFallenBehavior :: Secs -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy BlobEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

