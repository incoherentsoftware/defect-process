module Enemy.All.Zombie.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor           ((<&>))
import System.Random          (randomRIO)
import qualified Data.List.NonEmpty as NE

import AppEnv
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Zombie
import Constants
import Enemy as E
import Enemy.All.Zombie.AttackDescriptions
import Enemy.All.Zombie.Behavior
import Enemy.All.Zombie.Data
import Enemy.All.Zombie.Projectile
import Msg
import Util
import Window.Graphics

runBehaviorInstr
    :: MonadIO m
    => Bool
    -> ZombieEnemyBehaviorInstr
    -> Enemy ZombieEnemyData
    -> m [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                         -> return $ startIdleBehavior enemy
            UpdateIdleInstr idleTtl postIdleAction -> return $ updateIdleBehavior idleTtl postIdleAction enemy
            StartWalkInstr                         -> return $ startWalkBehavior enemy
            UpdateWalkInstr walkTtl                -> return $ updateWalkBehavior walkTtl enemy
            FlipDirectionInstr                     -> return $ flipDirectionMessages enemy
            ResetAtkCooldownInstr                  -> return $ resetAtkCooldownMessages enemy
            StartAttackInstr                       -> startAttackBehavior enemy
            CreateAttackProjInstr atkType          -> return $ createAttackProjMessages atkType enemy
            UpdateHurtInstr hurtTtl hurtType       -> return $ updateHurtBehavior hurtTtl hurtType enemy
            StartLaunchedInstr hangtimeTtl         -> return $ startLaunchedBehavior hangtimeTtl enemy
            LaunchedHangtimeInstr hangtimeTtl      -> return $ launchedHangtimeBehavior hangtimeTtl enemy
            StartFallenInstr fallenTtl             -> return $ startFallenBehavior fallenTtl enemy
            UpdateFallenInstr fallenTtl            -> return $ updateFallenBehavior fallenTtl enemy
            StartGetUpInstr                        -> return $ startGetUpBehavior enemy
            StartWallSplatInstr                    -> return $ startWallSplatBehavior enemy
            UpdateWallSplatInstr wallSplatTtl      -> return $ updateWallSplatBehavior wallSplatTtl enemy
            UpdateSpawnInstr                       -> return $ updateSpawnBehavior enemy
            StartDeathInstr                        -> return $ startDeathBehavior enemy
            SetDeadInstr                           -> return $ enemySetDeadMessages enemy

        aiDisabledMsgs =
            let
                setIdleMsgs = return $ case _behavior (E._data enemy) of
                    IdleBehavior _ _ -> []
                    _                -> startIdleBehavior enemy
            in case cmd of
                FlipDirectionInstr -> setIdleMsgs
                StartWalkInstr     -> setIdleMsgs
                UpdateWalkInstr _  -> setIdleMsgs
                StartAttackInstr   -> setIdleMsgs
                _                  -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy ZombieEnemyData -> ZombieEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (_data e) {_behavior = behavior}}

mkEnemyUpdateBehaviorMsgM
    :: Enemy ZombieEnemyData
    -> AppEnv UpdateEnemyMsgsPhase ZombieEnemyBehavior
    -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsgM enemy behavior = [mkMsgTo (EnemyMsgUpdateM update) enemyId]
    where
        update  = \e -> do
            behavior' <- behavior
            return $ e {_data = (_data e) {_behavior = behavior'}}
        enemyId = _msgId enemy

updateBehaviorIfMatching :: Enemy ZombieEnemyData -> ZombieEnemyBehavior -> ZombieEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

startDeathBehavior :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _zombie enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

updateHurtBehavior :: Secs -> HurtType -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior (hurtTtl - timeStep) hurtType

flipDirectionMessages :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
flipDirectionMessages enemy = [mkMsgToEx (EnemyMsgSetDirection dir) enemyId MsgAfterNormalOrder]
    where
        dir     = flipDirection $ E._dir enemy
        enemyId = E._msgId enemy

startWalkBehavior :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWalkBehavior enemy = mkEnemyUpdateBehaviorMsgM enemy behavior
    where
        cfg         = _zombie . _config $ _data enemy
        minWalkSecs = _minWalkSecs cfg
        maxWalkSecs = _maxWalkSecs cfg
        behavior    = liftIO $ WalkBehavior <$> randomRIO (minWalkSecs, maxWalkSecs)

updateWalkBehavior :: Secs -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWalkBehavior walkTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData     = _data e
        walkTtl'  = walkTtl - timeStep
        walkSpeed = _walkSpeed . _zombie $ _config eData
        dir       = enemyFlippedDirIfWallOrGround e
        velX      = walkSpeed * directionNeg dir
        velY      = vecY $ _vel e
    in e
        { _data = eData {_behavior = WalkBehavior walkTtl'}
        , _vel  = Vel2 velX velY
        , _dir  = dir
        }

startIdleBehavior :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = startIdleBehaviorWithDir dir enemy
    where dir = E._dir enemy

startIdleBehaviorWithDir :: Direction -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehaviorWithDir dir enemy = mkEnemyUpdateMsgM enemy $ \e -> do
    postIdleAction <- randomChoice $ TurnAroundPostIdle NE.:| [WalkPostIdle, WalkPostIdle]
    let idleSecs    = _idleSecs . _zombie $ _config (_data e)
    return $ e
        { _data   = (_data e) {_behavior = IdleBehavior idleSecs postIdleAction}
        , _dir    = dir
        , _vel    = Vel2 0.0 (vecY $ _vel e)
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> PostIdleAction -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl postIdleAction enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior (idleTtl - timeStep) postIdleAction

startFallenBehavior :: Secs -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

updateSpawnBehavior :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []

startAttackBehavior :: MonadIO m => Enemy ZombieEnemyData -> m [Msg ThinkEnemyMsgsPhase]
startAttackBehavior enemy =
    let
        enemyData     = _data enemy
        atkFallChance = _atkFallChance $ _zombie (_config enemyData)
        atkDescs      = _attackDescs enemyData
    in do
        (atkDesc, atkType) <- liftIO (randomRIO (0.0, 1.0)) <&> \case
            roll
                | roll <= atkFallChance -> (_fall atkDescs, FallAttackType)
                | otherwise             -> (_spit atkDescs, SpitAttackType)

        let setAtkMsg = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) (_msgId enemy)
        return $ setAtkMsg:mkEnemyUpdateBehaviorMsg enemy (AttackBehavior atkType)

createAttackProjMessages :: AttackType -> Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackProjMessages atkType enemy = [mkMsg $ NewThinkProjectileMsgAddM mkProj]
    where
        pos    = E._pos enemy
        dir    = E._dir enemy
        mkProj = mkZombieProjectile pos dir atkType (_data enemy)

resetAtkCooldownMessages :: Enemy ZombieEnemyData -> [Msg ThinkEnemyMsgsPhase]
resetAtkCooldownMessages enemy = mkEnemyUpdateMsgM enemy $ \e ->
    let
        zombieCfg      = _zombie $ _config (_data e)
        minAtkCooldown = _minAtkCooldown zombieCfg
        maxAtkCooldown = _maxAtkCooldown zombieCfg
    in do
        atkCooldown <- liftIO $ randomRIO (minAtkCooldown, maxAtkCooldown)
        return $ e
            { _data = (_data e) {_attackCooldown = atkCooldown}
            }
