module Enemy.All.Flying.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)

import Attack
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Flying
import Constants
import Enemy as E
import Enemy.All.Flying.AttackDescriptions
import Enemy.All.Flying.Behavior
import Enemy.All.Flying.Data
import Enemy.All.Flying.Projectile
import Msg
import Projectile
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> FlyingEnemyBehaviorInstr -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                         -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl                -> updateIdleBehavior idleTtl enemy
            FacePlayerInstr                        -> facePlayerMessages enemy
            StartAttackInstr atkDesc               -> startAttackBehavior atkDesc enemy
            CreateAttackProjInstr                  -> createAttackProjMessages enemy
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
                setIdleMsgs = case _behavior (E._data enemy) of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                UpdateIdleInstr idleTtl -> updateIdleBehavior idleTtl enemy
                FacePlayerInstr         -> setIdleMsgs
                StartAttackInstr _      -> setIdleMsgs
                CreateAttackProjInstr   -> setIdleMsgs
                _                       -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy FlyingEnemyData -> FlyingEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (E._data e) {_behavior = behavior}}

updateBehaviorIfMatching :: Enemy FlyingEnemyData -> FlyingEnemyBehavior -> FlyingEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (IdleBehavior _, IdleBehavior _)             -> behavior
    (HurtBehavior _ _, HurtBehavior _ _)         -> behavior
    (LaunchedBehavior _ _, LaunchedBehavior _ _) -> behavior
    (FallenBehavior _, FallenBehavior _)         -> behavior
    (WallSplatBehavior _, WallSplatBehavior _)   -> behavior
    _                                            -> existingBehavior
    where existingBehavior = _behavior $ E._data enemy

facePlayerMessages :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
facePlayerMessages enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        x   = vecX $ E._pos enemy
        dir = fromMaybe (E._dir e) $ do
            Pos2 playerX _ <- enemyKnownPlayerCenterPos enemy
            Just $ if playerX > x then RightDir else LeftDir
    in e {_dir = dir}

createAttackProjMessages :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackProjMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM mkFireball]
    where
        enemyData                      = E._data enemy
        flyingCfg                      = _flying $ _config enemyData
        Pos2 shootOffsetX shootOffsetY = _shootOffsetPos flyingCfg
        pos                            = E._pos enemy
        dir                            = E._dir enemy
        atkPos                         = pos `vecAdd` Pos2 (shootOffsetX * directionNeg dir) shootOffsetY
        atkDescs                       = _attackDescs enemyData

        playerPos = case enemyKnownPlayerCenterPos enemy of
            Just knownPos -> knownPos
            Nothing       -> Pos2 (1.0 * directionNeg dir) 1.0  -- aim diagonally down if unknown player position

        mkFireball :: (ConfigsRead m, MonadIO m) => m (Some Projectile)
        mkFireball = do
            fireballAtk <- mkEnemyAttack atkPos dir (_fireball atkDescs) (enemyTauntedStatus enemy)
            mkFlyingProjectile fireballAtk playerPos flyingCfg

startIdleBehavior :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data   = (E._data e) {_behavior = IdleBehavior idleSecs}
    , _vel    = zeroVel2
    , _attack = Nothing
    }
    where idleSecs = _idleSecs . _flying . _config $ E._data enemy

updateIdleBehavior :: Secs -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        idleTtl' = max 0.0 (idleTtl - timeStep)
        behavior = updateBehaviorIfMatching e (IdleBehavior idleTtl')
    in e
        { _data = (E._data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

updateHurtBehavior :: Secs -> HurtType -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        hurtTtl' = hurtTtl - timeStep
        behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)
    in e {_data = (E._data e) {_behavior = behavior}}

startFlyUpwardsBehavior :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFlyUpwardsBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = FlyUpwardsBehavior}
    }

updateFlyUpwardsBehavior :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFlyUpwardsBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e {_vel = vel}
    where
        enemyData       = E._data enemy
        riseRecoverVelY = _riseRecoverVelY . _flying $ _config enemyData
        vel             = Vel2 0.0 riseRecoverVelY

startAttackBehavior :: AttackDescription -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkDesc enemy = setAtkMsg:enemyUpdateMsg
    where
        setAtkMsg = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) (E._msgId enemy)

        enemyData                        = E._data enemy
        atkDescs                         = _attackDescs enemyData
        cfg                              = _flying $ _config enemyData
        (atkType, cooldown)
            | atkDesc == _shock atkDescs = (ShockAttackType, _shockCooldown cfg)
            | otherwise                  = (ShootAttackType, _shootCooldown cfg)

        enemyUpdateMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (E._data e)
                { _attackCooldown = cooldown
                , _behavior       = AttackBehavior
                , _prevAttackType = atkType
                }
            }

updateSpawnBehavior :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
        | spriteFinished spr                        -> startIdleBehavior enemy
    _                                               -> []

startDeathBehavior :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
startDeathBehavior enemy = deathSoundMsg:updateMsg
    where
        x             = vecX $ E._pos enemy
        centerY       = vecY $ hitboxCenter (enemyHitbox enemy)
        pos           = Pos2 x centerY
        deathSoundMsg = mkMsg $ AudioMsgPlaySound enemyDeathSoundPath pos

        updateMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data   = (E._data e) {_behavior = DeathBehavior}
            , _vel    = zeroVel2
            , _attack = Nothing
            }

startLaunchedBehavior :: Secs -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeSecs enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeSecs NotInHangtime

launchedInHangtimeBehavior :: Secs -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedInHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        hangtimeTtl' = hangtimeTtl - timeStep
        behavior     = updateBehaviorIfMatching e (LaunchedBehavior hangtimeTtl' InHangtime)
    in e
        { _data = (E._data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

launchedNotInHangtimeBehavior :: Secs -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedNotInHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let behavior = updateBehaviorIfMatching e (LaunchedBehavior hangtimeTtl NotInHangtime)
    in e {_data = (E._data e) {_behavior = behavior}}

startFallenBehavior :: Secs -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        fallenTtl' = fallenTtl - timeStep
        behavior   = updateBehaviorIfMatching e (FallenBehavior fallenTtl')
    in e
        { _data = (E._data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

startGetUpBehavior :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

startWallSplatBehavior :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ E._data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _flying enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (E._data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        wallSplatTtl' = wallSplatTtl - timeStep
        behavior      = updateBehaviorIfMatching e (WallSplatBehavior wallSplatTtl')
    in e
        { _data = (E._data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }
