module Enemy.All.Claws.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)

import Attack
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Claws
import Constants
import Enemy as E
import Enemy.All.Claws.AttackDescriptions
import Enemy.All.Claws.Behavior
import Enemy.All.Claws.Data
import Enemy.All.Claws.Projectile
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> ClawsEnemyBehaviorInstr -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                    -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl           -> updateIdleBehavior idleTtl enemy
            StartAdvanceInstr                 -> startAdvanceBehavior enemy
            UpdateAdvanceInstr advanceTtl     -> updateAdvanceBehavior advanceTtl enemy
            StartRetreatInstr                 -> startRetreatBehavior enemy
            UpdateRetreatInstr retreatTtl     -> updateRetreatBehavior retreatTtl enemy
            StartDashInstr                    -> startDashBehavior enemy
            StartAttackInstr atkDesc          -> startAttackBehavior atkDesc enemy
            CreateAttackProjInstr             -> createAttackProjectileMessages enemy
            UpdateWillUseAttackProjInstr      -> updateWillUseAttackProjMessages enemy
            UpdateWillUseDashInstr            -> updateWillUseDashMessages enemy
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
                UpdateAdvanceInstr _  -> setIdleMsgs
                UpdateRetreatInstr _  -> setIdleMsgs
                StartAdvanceInstr     -> setIdleMsgs
                StartRetreatInstr     -> setIdleMsgs
                StartDashInstr        -> setIdleMsgs
                StartAttackInstr _    -> setIdleMsgs
                CreateAttackProjInstr -> setIdleMsgs
                _                     -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy ClawsEnemyData -> ClawsEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (_data e) {_behavior = behavior}}

updateBehaviorIfMatching :: Enemy ClawsEnemyData -> ClawsEnemyBehavior -> ClawsEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

createAttackProjectileMessages :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackProjectileMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM mkProj]
    where mkProj = mkClawsProjectile (E._pos enemy) (E._dir enemy) (_data enemy) (enemyTauntedStatus enemy)

updateWillUseAttackProjMessages :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWillUseAttackProjMessages enemy = mkEnemyUpdateMsgM enemy $ \e -> do
    let eData       = _data e
    willUseAtkProj <- rollWillUseAttackProjectile $ _config eData
    return $ e
        { _data   = eData {_willUseAttackProjectile = willUseAtkProj}
        , _attack = Nothing
        }

updateWillUseDashMessages :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWillUseDashMessages enemy = mkEnemyUpdateMsgM enemy $ \e -> do
    let eData    = _data e
    willUseDash <- rollWillUseDash $ _config eData
    return $ e
        { _data   = eData {_willUseDash = willUseDash}
        , _attack = Nothing
        }

startDeathBehavior :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _claws enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

updateHurtBehavior :: Secs -> HurtType -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior (hurtTtl - timeStep) hurtType

startAttackBehavior :: AttackDescription -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkDesc enemy = setAtkMsg:updateBehaviorMsgs
    where
        updateBehaviorMsgs = mkEnemyUpdateBehaviorMsg enemy AttackBehavior
        setAtkMsg          = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) (_msgId enemy)

startDashBehavior :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startDashBehavior enemy = setAtkMsg:updateBehaviorMsgs
    where
        updateBehaviorMsgs = mkEnemyUpdateBehaviorMsg enemy DashBehavior
        dashAtkDesc        = _dash $ _attackDescs (_data enemy)
        setAtkMsg          = mkMsgTo (EnemyMsgSetAttackDesc dashAtkDesc) (_msgId enemy)

startAdvanceBehavior :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAdvanceBehavior enemy = mkEnemyUpdateMsgM enemy updateAdvance
    where
        cfg            = _claws . _config $ _data enemy
        minAdvanceSecs = _minAdvanceSecs cfg
        maxAdvanceSecs = _maxAdvanceSecs cfg
        advanceSpeed   = _advanceSpeed cfg

        x     = vecX $ E._pos enemy
        dir   = case enemyKnownPlayerPos enemy of
            Nothing               -> E._dir enemy
            Just (Pos2 playerX _) -> if playerX > x then RightDir else LeftDir
        velX' = advanceSpeed * directionNeg dir
        velY  = vecY $ _vel enemy

        updateAdvance = \e -> do
            advanceSecs <- liftIO $ randomRIO (minAdvanceSecs, maxAdvanceSecs)
            return $ e
                { _data = (_data e) {_behavior = AdvanceBehavior advanceSecs}
                , _dir  = dir
                , _vel  = Vel2 velX' velY
                }

updateAdvanceBehavior :: Secs -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateAdvanceBehavior advanceTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        behavior     = AdvanceBehavior $ advanceTtl - timeStep
        eData        = _data e
        advanceSpeed = _advanceSpeed . _claws $ _config eData
        dir          = enemyFlippedDirIfWallOrGround e
        velX         = advanceSpeed * directionNeg dir
        velY         = vecY $ _vel e
    in e
        { _data = eData {_behavior = behavior}
        , _dir  = dir
        , _vel  = Vel2 velX velY
        }

startRetreatBehavior :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startRetreatBehavior enemy = mkEnemyUpdateMsgM enemy updateRetreat
    where
        enemyData      = _data enemy
        cfg            = _claws $ _config enemyData
        retreatSpeed   = _retreatSpeed cfg
        minRetreatSecs = _minRetreatSecs cfg
        maxRetreatSecs = _maxRetreatSecs cfg

        x             = vecX $ E._pos enemy
        playerPos     = enemyKnownPlayerPos enemy
        facePlayerDir = \pos -> if vecX pos > x then RightDir else LeftDir
        dir           = E._dir enemy
        dir'          = maybe dir facePlayerDir playerPos
        velX'         = retreatSpeed * directionPos dir'
        velY          = vecY $ _vel enemy

        updateRetreat = \e -> do
            retreatSecs <- liftIO $ randomRIO (minRetreatSecs, maxRetreatSecs)
            return $ e
                { _data   = (_data e) {_behavior = RetreatBehavior retreatSecs}
                , _dir    = dir'
                , _vel    = Vel2 velX' velY
                , _attack = Nothing
                }

updateRetreatBehavior :: Secs -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateRetreatBehavior retreatTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = RetreatBehavior $ retreatTtl - timeStep

startIdleBehavior :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy updateBehavior
    where
        idleSecs       = _idleSecs . _claws . _config $ _data enemy
        behavior       = IdleBehavior idleSecs
        updateBehavior = \e -> e
            { _data = (_data e) {_behavior = behavior}
            , _vel  = Vel2 0.0 (vecY $ _vel e)
            }

updateIdleBehavior :: Secs -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

startFallenBehavior :: Secs -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = FallenBehavior fallenTtl

updateFallenBehavior :: Secs -> Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

updateSpawnBehavior :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
        | spriteFinished spr                        -> startIdleBehavior enemy
    _                                               -> []
