module Enemy.All.Axe.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)

import Attack
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Axe
import Constants
import Enemy as E
import Enemy.All.Axe.Behavior
import Enemy.All.Axe.Data
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> AxeEnemyBehaviorInstr -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs'
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                    -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl           -> updateIdleBehavior idleTtl enemy
            StartAdvanceInstr                 -> startAdvanceBehavior enemy
            UpdateAdvanceInstr advanceTtl     -> updateAdvanceBehavior advanceTtl enemy
            StartRetreatInstr                 -> startRetreatBehavior enemy
            UpdateRetreatInstr retreatTtl     -> updateRetreatBehavior retreatTtl enemy
            StartAttackInstr atkDesc          -> startAttackBehavior atkDesc enemy
            UpdateWillUseAttackLungeInstr     -> updateWillUseAttackLungeBehavior enemy
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

        cfg                   = _axe $ _config (_data enemy)
        tauntedIdleSecs       = _tauntedIdleSecs cfg
        tauntedMaxRetreatSecs = _tauntedMaxRetreatSecs cfg

        aiEnabledMsgs' = case enemyTauntedStatus enemy of
            EnemyTauntedInactive -> aiEnabledMsgs
            EnemyTauntedActive   -> case cmd of
                UpdateIdleInstr idleTtl
                    | idleTtl > tauntedIdleSecs          -> updateIdleBehavior tauntedIdleSecs enemy
                UpdateRetreatInstr retreatTtl
                    | retreatTtl > tauntedMaxRetreatSecs -> updateRetreatBehavior tauntedMaxRetreatSecs enemy
                _                                        -> aiEnabledMsgs

        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior (_data enemy) of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                UpdateAdvanceInstr _ -> setIdleMsgs
                UpdateRetreatInstr _ -> setIdleMsgs
                StartAttackInstr _   -> setIdleMsgs
                StartAdvanceInstr    -> setIdleMsgs
                StartRetreatInstr    -> setIdleMsgs
                _                    -> aiEnabledMsgs'

mkEnemyUpdateBehaviorMsg :: Enemy AxeEnemyData -> AxeEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = behavior}
    }

updateBehaviorIfMatching :: Enemy AxeEnemyData -> AxeEnemyBehavior -> AxeEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

updateSpawnBehavior :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []

startDeathBehavior :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _axe enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

updateHurtBehavior :: Secs -> HurtType -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)}
    }
    where hurtTtl' = hurtTtl - timeStep

startAttackBehavior :: AttackDescription -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkDesc enemy = behaviorMsg ++ attackMsg
    where
        behaviorMsg = mkEnemyUpdateBehaviorMsg enemy AttackBehavior
        enemyId     = _msgId enemy
        attackMsg   = [mkMsgTo (EnemyMsgSetAttackDesc atkDesc) enemyId]

updateWillUseAttackLungeBehavior :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWillUseAttackLungeBehavior enemy = mkEnemyUpdateMsgM enemy $ \e -> do
    willUseAtkLunge <- rollWillUseAttackLunge $ _config (_data e)
    return $ e
        { _data = (_data e) {_willUseAttackLunge = willUseAtkLunge}
        }

startAdvanceBehavior :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAdvanceBehavior enemy = mkEnemyUpdateMsgM enemy updateAdvance
    where
        cfg            = _axe . _config $ _data enemy
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
            return $ enemy
                { _data = (_data e) {_behavior = AdvanceBehavior advanceSecs}
                , _dir  = dir
                , _vel  = Vel2 velX' velY
                }

updateAdvanceBehavior :: Secs -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateAdvanceBehavior advanceTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        behavior     = AdvanceBehavior $ advanceTtl - timeStep
        eData        = _data e
        advanceSpeed = _advanceSpeed . _axe $ _config eData
        dir          = enemyFlippedDirIfWallOrGround e
        velX         = advanceSpeed * directionNeg dir
        velY         = vecY $ _vel e
    in e
        { _data = eData {_behavior = behavior}
        , _dir  = dir
        , _vel  = Vel2 velX velY
        }

startRetreatBehavior :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
startRetreatBehavior enemy = mkEnemyUpdateMsgM enemy updateRetreat
    where
        enemyData      = _data enemy
        cfg            = _axe $ _config enemyData
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

updateRetreatBehavior :: Secs -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateRetreatBehavior retreatTtl enemy = mkEnemyUpdateBehaviorMsg enemy (RetreatBehavior retreatTtl')
    where retreatTtl' = retreatTtl - timeStep

startIdleBehavior :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy updateBehavior
    where
        idleSecs       = _idleSecs . _axe . _config $ _data enemy
        behavior       = IdleBehavior idleSecs
        updateBehavior = \e -> e
            { _data   = (_data e) {_behavior = behavior}
            , _vel    = Vel2 0.0 (vecY $ _vel e)
            , _attack = Nothing
            }

updateIdleBehavior :: Secs -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy (IdleBehavior idleTtl')
    where idleTtl' = idleTtl - timeStep

startFallenBehavior :: Secs -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior
