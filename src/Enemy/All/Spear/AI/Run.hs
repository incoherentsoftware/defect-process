module Enemy.All.Spear.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)

import AppEnv
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Spear
import Constants
import Enemy as E
import Enemy.All.Spear.AttackDescriptions
import Enemy.All.Spear.AttackType
import Enemy.All.Spear.Behavior
import Enemy.All.Spear.Data
import Enemy.All.Spear.Projectile
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> SpearEnemyBehaviorInstr -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                    -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl           -> updateIdleBehavior idleTtl enemy
            StartWalkInstr                    -> startWalkBehavior enemy
            UpdateWalkInstr walkTtl           -> updateWalkBehavior walkTtl enemy
            StartRetreatInstr                 -> startRetreatBehavior enemy
            UpdateRetreatInstr retreatTtl     -> updateRetreatBehavior retreatTtl enemy
            FlipDirectionInstr                -> flipDirectionMessages enemy
            StartAttackInstr atkType          -> startAttackBehavior atkType enemy
            CreateAttackProjInstr             -> createAttackProjMessages enemy
            SetThrowAtkCooldownInstr          -> setThrowAtkCooldownMessages enemy
            SetShoveAtkCooldownInstr          -> setShoveAtkCooldownMessages enemy
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
                StartWalkInstr       -> setIdleMsgs
                UpdateWalkInstr _    -> setIdleMsgs
                StartRetreatInstr    -> setIdleMsgs
                UpdateRetreatInstr _ -> setIdleMsgs
                StartAttackInstr _   -> setIdleMsgs
                FlipDirectionInstr   -> setIdleMsgs
                _                    -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy SpearEnemyData -> SpearEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (_data e) {_behavior = behavior}}

mkEnemyUpdateBehaviorMsgM
    :: Enemy SpearEnemyData
    -> AppEnv UpdateEnemyMsgsPhase SpearEnemyBehavior
    -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsgM enemy behavior = [mkMsgTo (EnemyMsgUpdateM update) enemyId]
    where
        update  = \e -> do
            behavior' <- behavior
            return $ e {_data = (_data e) {_behavior = behavior'}}
        enemyId = _msgId enemy

updateBehaviorIfMatching :: Enemy SpearEnemyData -> SpearEnemyBehavior -> SpearEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

setThrowAtkCooldownMessages :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
setThrowAtkCooldownMessages enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData = E._data e
        cfg   = _spear $ _config eData
    in e
        { _data = eData {_throwAtkCooldownTtl = _throwAtkCooldownSecs cfg}
        }

setShoveAtkCooldownMessages :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
setShoveAtkCooldownMessages enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData = E._data e
        cfg   = _spear $ _config eData
    in e
        { _data = eData {_shoveAtkCooldownTtl = _shoveAtkCooldownSecs cfg}
        }

startDeathBehavior :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _spear enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

updateHurtBehavior :: Secs -> HurtType -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior (hurtTtl - timeStep) hurtType

startAttackBehavior :: SpearEnemyAttackType -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkType enemy = attackMsg:updateDataMsgs
    where
        atkDescs = _attackDescs $ E._data enemy
        atkDesc  = case atkType of
            ThrowAttackType -> _throw atkDescs
            ShoveAttackType -> _shove atkDescs

        updateDataMsgs = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = AttackBehavior}
            }

        enemyId   = _msgId enemy
        attackMsg = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) enemyId

flipDirectionMessages :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
flipDirectionMessages enemy = [mkMsgToEx (EnemyMsgSetDirection dir) enemyId MsgAfterNormalOrder]
    where
        dir     = flipDirection $ E._dir enemy
        enemyId = E._msgId enemy

createAttackProjMessages :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackProjMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM mkSpearProj]
    where mkSpearProj = mkSpearProjectile (E._pos enemy) (E._dir enemy) (_data enemy) (enemyTauntedStatus enemy)

startWalkBehavior :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWalkBehavior enemy = mkEnemyUpdateBehaviorMsgM enemy behavior
    where
        cfg         = _spear . _config $ _data enemy
        minWalkSecs = _minWalkSecs cfg
        maxWalkSecs = _maxWalkSecs cfg
        behavior    = liftIO $ WalkBehavior <$> randomRIO (minWalkSecs, maxWalkSecs)

updateWalkBehavior :: Secs -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWalkBehavior walkTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData     = _data e
        walkTtl'  = walkTtl - timeStep
        walkSpeed = _walkSpeed . _spear $ _config eData
        dir       = enemyFlippedDirIfWallOrGround e
        velX      = walkSpeed * directionNeg dir
        velY      = vecY $ _vel e
    in e
        { _data   = eData {_behavior = WalkBehavior walkTtl'}
        , _vel    = Vel2 velX velY
        , _dir    = dir
        , _attack = Nothing
        }

startRetreatBehavior :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
startRetreatBehavior enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where
        cfg      = _spear . _config $ _data enemy
        behavior = RetreatBehavior $ _retreatSecs cfg

updateRetreatBehavior :: Secs -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateRetreatBehavior retreatTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData       = _data e
        retreatTtl' = retreatTtl - timeStep
        walkSpeed   = _walkSpeed . _spear $ _config eData
        dir         = E._dir e
        velX        = walkSpeed * directionPos dir
        velY        = vecY $ _vel e
    in e
        { _data   = eData {_behavior = RetreatBehavior retreatTtl'}
        , _vel    = Vel2 velX velY
        , _attack = Nothing
        }

startIdleBehavior :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let idleSecs = _idleSecs . _spear $ _config (_data e)
    in e
        { _data   = (_data e) {_behavior = IdleBehavior idleSecs}
        , _vel    = Vel2 0.0 (vecY $ _vel e)
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

startFallenBehavior :: Secs -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

updateSpawnBehavior :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []
