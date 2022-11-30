module Enemy.All.Flail.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)

import AppEnv
import Attack
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Flail
import Constants
import Enemy as E
import Enemy.All.Flail.Behavior
import Enemy.All.Flail.Data
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> FlailEnemyBehaviorInstr -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                    -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl           -> updateIdleBehavior idleTtl enemy
            StartWalkInstr                    -> startWalkBehavior enemy
            UpdateWalkInstr advanceTtl        -> updateWalkBehavior advanceTtl enemy
            StartRetreatInstr                 -> startRetreatBehavior enemy
            UpdateRetreatInstr advanceTtl     -> updateRetreatBehavior advanceTtl enemy
            StartIdleToWalkInstr              -> startIdleToWalkBehavior enemy
            StartIdleToRetreatInstr           -> startIdleToRetreatBehavior enemy
            StartWalkToIdleInstr              -> startWalkToIdleBehavior enemy
            FacePlayerInstr                   -> facePlayerMessages enemy
            StartAttackInstr atkDesc          -> startAttackBehavior atkDesc enemy
            SetAttackCooldownInstr            -> setAttackCooldownMessages enemy
            UpdateHurtInstr hurtTtl hurtType  -> updateHurtBehavior hurtTtl hurtType enemy
            StartLaunchedInstr hangtimeTtl    -> startLaunchedBehavior hangtimeTtl enemy
            LaunchedHangtimeInstr hangtimeTtl -> launchedHangtimeBehavior hangtimeTtl enemy
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
                StartWalkInstr          -> setIdleMsgs
                UpdateWalkInstr _       -> setIdleMsgs
                StartRetreatInstr       -> setIdleMsgs
                UpdateRetreatInstr _    -> setIdleMsgs
                StartIdleToWalkInstr    -> setIdleMsgs
                StartIdleToRetreatInstr -> setIdleMsgs
                StartWalkToIdleInstr    -> setIdleMsgs
                FacePlayerInstr         -> setIdleMsgs
                StartAttackInstr _      -> setIdleMsgs
                _                       -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy FlailEnemyData -> FlailEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = behavior}
    }

mkEnemyUpdateBehaviorNoAttackMsg :: Enemy FlailEnemyData -> FlailEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorNoAttackMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { _data   = (_data e) {_behavior = behavior}
    , _attack = Nothing
    }

mkEnemyUpdateBehaviorMsgM
    :: Enemy FlailEnemyData
    -> AppEnv UpdateEnemyMsgsPhase FlailEnemyBehavior
    -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsgM enemy behavior = [mkMsgTo (EnemyMsgUpdateM update) enemyId]
    where
        update  = \e -> do
            behavior' <- behavior
            return $ e {_data = (_data e) {_behavior = behavior'}}
        enemyId = _msgId enemy

updateBehaviorIfMatching :: Enemy FlailEnemyData -> FlailEnemyBehavior -> FlailEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

facePlayerMessages :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
facePlayerMessages enemy = case vecX <$> enemyKnownPlayerPos enemy of
    Just playerX ->
        let
            x   = vecX $ E._pos enemy
            dir = if playerX < x then LeftDir else RightDir
        in [mkMsgTo (EnemyMsgSetDirection dir) (E._msgId enemy)]
    Nothing      -> []

setAttackCooldownMessages :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
setAttackCooldownMessages enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData = E._data e
        cfg   = _flail $ _config eData
    in e
        { _data = eData {_attackCooldownTtl = _attackCooldownSecs cfg}
        }

startDeathBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _flail enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

startAttackBehavior :: AttackDescription -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkDesc enemy = attackMsg:behaviorMsgs
    where
        behaviorMsgs = mkEnemyUpdateBehaviorMsg enemy AttackBehavior
        enemyId      = _msgId enemy
        attackMsg    = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) enemyId

startWalkBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWalkBehavior enemy = mkEnemyUpdateBehaviorMsgM enemy behavior
    where
        cfg         = _flail . _config $ _data enemy
        minWalkSecs = _minWalkSecs cfg
        maxWalkSecs = _maxWalkSecs cfg
        behavior    = liftIO $ WalkBehavior <$> randomRIO (minWalkSecs, maxWalkSecs)

updateWalkBehavior :: Secs -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWalkBehavior walkTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData     = _data e
        walkTtl'  = walkTtl - timeStep
        walkSpeed = _walkSpeed . _flail $ _config eData
        dir       = enemyFlippedDirIfWallOrGround e
        velX      = walkSpeed * directionNeg dir
        velY      = vecY $ _vel e
    in e
        { _data   = eData {_behavior = WalkBehavior walkTtl'}
        , _vel    = Vel2 velX velY
        , _dir    = dir
        , _attack = Nothing
        }

startRetreatBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startRetreatBehavior enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where
        cfg      = _flail . _config $ _data enemy
        behavior = RetreatBehavior $ _retreatSecs cfg

updateRetreatBehavior :: Secs -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateRetreatBehavior retreatTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData       = _data e
        retreatTtl' = retreatTtl - timeStep
        walkSpeed   = _walkSpeed . _flail $ _config eData
        dir         = E._dir e
        velX        = walkSpeed * directionPos dir
        velY        = vecY $ _vel e
    in e
        { _data   = eData {_behavior = RetreatBehavior retreatTtl'}
        , _vel    = Vel2 velX velY
        , _attack = Nothing
        }

startIdleBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = _data e
        idleSecs = _idleSecs $ _flail (_config eData)
    in e
        { _data   = eData {_behavior = IdleBehavior idleSecs}
        , _vel    = Vel2 0.0 (vecY $ _vel e)
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

startIdleToWalkBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleToWalkBehavior enemy = mkEnemyUpdateBehaviorNoAttackMsg enemy IdleToWalkBehavior

startIdleToRetreatBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleToRetreatBehavior enemy = mkEnemyUpdateBehaviorNoAttackMsg enemy IdleToRetreatBehavior

startWalkToIdleBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWalkToIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WalkToIdleBehavior}
    , _vel  = Vel2 0.0 (vecY $ _vel e)
    }

startFallenBehavior :: Secs -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

updateSpawnBehavior :: Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
        | spriteFinished spr                        -> startIdleBehavior enemy
    _                                               -> []

updateHurtBehavior :: Secs -> HurtType -> Enemy FlailEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior (hurtTtl - timeStep) hurtType
