module Enemy.All.Wall.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)

import AppEnv
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Wall
import Constants
import Enemy as E
import Enemy.All.Wall.AttackDescriptions
import Enemy.All.Wall.Behavior
import Enemy.All.Wall.Data
import Enemy.All.Wall.Util
import Enemy.All.Wall.WallProjectile
import InfoMsg.Util
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> WallEnemyBehaviorInstr -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs'
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                    -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl           -> updateIdleBehavior idleTtl enemy
            StartWalkInstr                    -> startWalkBehavior enemy
            UpdateWalkInstr walkTtl           -> updateWalkBehavior walkTtl enemy
            StartBackWalkInstr                -> startBackWalkBehavior enemy
            UpdateBackWalkInstr backWalkTtl   -> updateBackWalkBehavior backWalkTtl enemy
            FacePlayerInstr                   -> facePlayerMessages enemy
            StartAttackInstr                  -> startAttackBehavior enemy
            CreateAttackProjInstr             -> createAttackProjectileMessages enemy
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

        cfg                = _wall $ _config (_data enemy)
        tauntedIdleSecs    = _tauntedIdleSecs cfg
        tauntedMaxWalkSecs = _tauntedMaxWalkSecs cfg

        aiEnabledMsgs' = case enemyTauntedStatus enemy of
            EnemyTauntedInactive -> aiEnabledMsgs
            EnemyTauntedActive   -> case cmd of
                UpdateIdleInstr idleTtl
                    | idleTtl > tauntedIdleSecs    -> updateIdleBehavior tauntedIdleSecs enemy
                UpdateWalkInstr walkTtl
                    | walkTtl > tauntedMaxWalkSecs -> updateWalkBehavior tauntedMaxWalkSecs enemy
                _                                  -> aiEnabledMsgs

        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior (E._data enemy) of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                StartWalkInstr        -> setIdleMsgs
                UpdateWalkInstr _     -> setIdleMsgs
                StartBackWalkInstr    -> setIdleMsgs
                UpdateBackWalkInstr _ -> setIdleMsgs
                FacePlayerInstr       -> setIdleMsgs
                StartAttackInstr      -> setIdleMsgs
                _                     -> aiEnabledMsgs'

mkEnemyUpdateBehaviorMsg :: Enemy WallEnemyData -> WallEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (_data e) {_behavior = behavior}}

mkEnemyUpdateDataMsgM
    :: Enemy WallEnemyData
    -> AppEnv UpdateEnemyMsgsPhase WallEnemyBehavior
    -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgM enemy behavior = [mkMsgTo (EnemyMsgUpdateM update) enemyId]
    where
        update  = \e -> do
            behavior' <- behavior
            return $ e
                { _data = (_data e) {_behavior = behavior'}
                }
        enemyId = E._msgId enemy

updateBehaviorIfMatching :: Enemy WallEnemyData -> WallEnemyBehavior -> WallEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

facePlayerMessages :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
facePlayerMessages enemy = case vecX <$> enemyKnownPlayerPos enemy of
    Just playerX ->
        let
            x   = vecX $ E._pos enemy
            dir = if playerX < x then LeftDir else RightDir
        in [mkMsgTo (EnemyMsgSetDirection dir) (E._msgId enemy)]
    Nothing      -> []

createAttackProjectileMessages :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackProjectileMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM mkWallProj]
    where mkWallProj = mkWallProjectile (E._pos enemy) (E._dir enemy) (_data enemy) (enemyTauntedStatus enemy)

startDeathBehavior :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _wall enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

updateHurtBehavior :: Secs -> HurtType -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)}
    }
    where hurtTtl' = hurtTtl - timeStep

startAttackBehavior :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior enemy = attackMsg:behaviorDataMsgs
    where
        behaviorDataMsgs = mkEnemyUpdateMsg enemy $ \e ->
            let
                eData = _data e
                posX  = vecX $ E._pos e
                dir   = case _knownPlayerInfo e of
                    Nothing                                  -> E._dir e
                    Just aiInfo
                        | vecX (playerInfoPos aiInfo) < posX -> LeftDir
                        | otherwise                          -> RightDir
                cfg   = _wall $ _config eData
            in e
                { _dir  = dir
                , _data = eData
                    { _attackCooldown = _releaseWallProjCooldown cfg * attackCooldownMultiplier e
                    , _behavior       = AttackBehavior
                    }
                }

        enemyId   = E._msgId enemy
        atkDesc   = _releaseWall $ _attackDescs (_data enemy)
        attackMsg = mkMsgToEx (EnemyMsgSetAttackDesc atkDesc) enemyId MsgEndOrder

startWalkBehavior :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWalkBehavior enemy = mkEnemyUpdateDataMsgM enemy behavior
    where
        cfg         = _wall . _config $ _data enemy
        minWalkSecs = _minWalkSecs cfg
        maxWalkSecs = _maxWalkSecs cfg
        behavior    = liftIO $ WalkBehavior <$> randomRIO (minWalkSecs, maxWalkSecs)

updateWalkBehavior :: Secs -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWalkBehavior walkTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData     = _data e
        walkTtl'  = walkTtl - timeStep
        walkSpeed = _walkSpeed . _wall $ _config eData
        velX      = walkSpeed * directionNeg (E._dir e)
        velY      = vecY $ E._vel e
    in e
        { _data = eData {_behavior = WalkBehavior walkTtl'}
        , _vel  = Vel2 velX velY
        }

startBackWalkBehavior :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
startBackWalkBehavior enemy = mkEnemyUpdateDataMsgM enemy behavior
    where
        cfg         = _wall . _config $ _data enemy
        minWalkSecs = _minWalkSecs cfg
        maxWalkSecs = _maxWalkSecs cfg
        behavior    = liftIO $ BackWalkBehavior <$> randomRIO (minWalkSecs, maxWalkSecs)

updateBackWalkBehavior :: Secs -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateBackWalkBehavior walkTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData     = _data e
        walkTtl'  = walkTtl - timeStep
        walkSpeed = _walkSpeed . _wall $ _config eData
        velX      = walkSpeed * directionPos (E._dir e)
        velY      = vecY $ E._vel e
    in e
        { _data = eData {_behavior = BackWalkBehavior walkTtl'}
        , _vel  = Vel2 velX velY
        }

startIdleBehavior :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = _data e
        idleSecs = _idleSecs $ _wall (_config eData)
    in e
        { _data   = eData {_behavior = IdleBehavior idleSecs}
        , _vel    = Vel2 0.0 (vecY $ E._vel e)
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

startFallenBehavior :: Secs -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

updateSpawnBehavior :: Enemy WallEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []
