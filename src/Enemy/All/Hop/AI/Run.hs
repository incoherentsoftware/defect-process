module Enemy.All.Hop.AI.Run
    ( runBehaviorInstr
    ) where

import Attack
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Hop
import Constants
import Enemy as E
import Enemy.All.Hop.AttackDescriptions
import Enemy.All.Hop.Behavior
import Enemy.All.Hop.Data
import Enemy.All.Hop.Projectile
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> HopEnemyBehaviorInstr -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                    -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl           -> updateIdleBehavior idleTtl enemy
            StartHopLongInstr                 -> startHopLongBehavior enemy
            StartHopLongLandInstr             -> startHopLongLandBehavior enemy
            StartHopShortInstr                -> startHopShortBehavior enemy
            StartHopShortLandInstr            -> startHopShortLandBehavior enemy
            SetHopLongVelInstr                -> setHopLongVelMessages enemy
            SetHopShortVelInstr               -> setHopShortVelMessages enemy
            StartAttackHopLongInstr           -> startAttackHopLongBehavior enemy
            StartAttackHopLongLandInstr       -> startAttackHopLongLandBehavior enemy
            StartAttackHopShortInstr          -> startAttackHopShortBehavior enemy
            StartAttackHopShortLandInstr      -> startAttackHopShortLandBehavior enemy
            FlipHopDirectionInstr             -> flipHopDirectionMessages enemy
            ClearAttackInstr                  -> clearAttackProjMessages enemy
            CreateAttackProjInstr             -> createAttackProjMessages enemy
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
                StartHopLongInstr            -> setIdleMsgs
                StartHopLongLandInstr        -> setIdleMsgs
                StartHopShortInstr           -> setIdleMsgs
                StartHopShortLandInstr       -> setIdleMsgs
                StartAttackHopLongInstr      -> setIdleMsgs
                StartAttackHopLongLandInstr  -> setIdleMsgs
                StartAttackHopShortInstr     -> setIdleMsgs
                StartAttackHopShortLandInstr -> setIdleMsgs
                _                            -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy HopEnemyData -> HopEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = behavior}
    }

updateBehaviorIfMatching :: Enemy HopEnemyData -> HopEnemyBehavior -> HopEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

startDeathBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _hop enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

updateHurtBehavior :: Secs -> HurtType -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior (hurtTtl - timeStep) hurtType

startIdleBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = _data e
        idleSecs = _idleSecs $ _hop (_config eData)
    in e
        { _data   = eData {_behavior = IdleBehavior idleSecs}
        , _vel    = Vel2 0.0 (vecY $ _vel e)
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

startFallenBehavior :: Secs -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

updateSpawnBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []

flipHopDirectionMessages :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
flipHopDirectionMessages enemy =
    [ mkMsgToEx (EnemyMsgUpdateVelocity updateVel) enemyId MsgAfterNormalOrder
    , mkMsgToEx (EnemyMsgSetDirection dir) enemyId MsgAfterNormalOrder
    ]
        where
            velX    = vecX $ E._vel enemy
            flags   = E._flags enemy
            enemyId = _msgId enemy

            (updateVel, dir)
                | _touchingLeftWall flags && velX < 0.0  =
                    ( \(Vel2 vX vY) -> Vel2 (abs vX) vY
                    , RightDir
                    )
                | _touchingRightWall flags && velX > 0.0 =
                    (\(Vel2 vX vY) -> Vel2 (-(abs vX)) vY
                    , LeftDir
                    )
                | otherwise                              = (id, E._dir enemy)

startHopLongBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startHopLongBehavior enemy = updateBehaviorMsg
    where updateBehaviorMsg = mkEnemyUpdateBehaviorMsg enemy HopLongBehavior

setHopLongVelMessages :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
setHopLongVelMessages enemy = [mkMsgToEx (EnemyMsgSetVelocity hopLongVel') enemyId MsgAfterNormalOrder]
    where
        hopLongVel  = _hopLongVel . _hop . _config $ _data enemy
        hopLongVel' = hopLongVel `vecFlip` E._dir enemy
        enemyId     = _msgId enemy

startHopLongLandBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startHopLongLandBehavior enemy = mkEnemyUpdateBehaviorMsg enemy HopLongLandBehavior

startHopShortBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startHopShortBehavior enemy = mkEnemyUpdateBehaviorMsg enemy HopShortBehavior

setHopShortVelMessages :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
setHopShortVelMessages enemy = [mkMsgToEx (EnemyMsgSetVelocity hopShortVel') enemyId MsgAfterNormalOrder]
    where
        hopShortVel  = _hopShortVel . _hop . _config $ _data enemy
        hopShortVel' = hopShortVel `vecFlip` E._dir enemy
        enemyId      = _msgId enemy

startHopShortLandBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startHopShortLandBehavior enemy = mkEnemyUpdateBehaviorMsg enemy HopShortLandBehavior

setAttackMessages :: (EnemyAttackDescriptions -> AttackDescription) -> Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
setAttackMessages atkDescF enemy = mkMsgToEx (EnemyMsgSetAttackDesc atkDesc) enemyId MsgEndOrder:setBehaviorMsgs
    where
        enemyData    = _data enemy
        atkDescs     = _attackDescs enemyData
        atkDesc      = atkDescF atkDescs
        enemyId      = _msgId enemy
        attackDescIn = \atkDescFs -> atkDesc `elem` map ($ atkDescs) atkDescFs

        behavior
            | attackDescIn [_attackPreHopLong, _attackHopLong]   = AttackHopLongBehavior
            | attackDescIn [_attackPreHopShort, _attackHopShort] = AttackHopShortBehavior
            | atkDesc == _attackHopLongLand atkDescs             = AttackHopLongLandBehavior
            | atkDesc == _attackHopShortLand atkDescs            = AttackHopShortLandBehavior
            | otherwise                                          = _behavior enemyData

        setBehaviorMsgs = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = behavior}
            }

startAttackHopLongBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackHopLongBehavior enemy = setAttackMessages _attackPreHopLong enemy

startAttackHopLongLandBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackHopLongLandBehavior enemy = setAttackMessages _attackHopLongLand enemy

startAttackHopShortBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackHopShortBehavior enemy = setAttackMessages _attackPreHopShort enemy

startAttackHopShortLandBehavior :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackHopShortLandBehavior enemy = setAttackMessages _attackHopShortLand enemy

createAttackProjMessages :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackProjMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM (mkHopProjectile enemy)]

clearAttackProjMessages :: Enemy HopEnemyData -> [Msg ThinkEnemyMsgsPhase]
clearAttackProjMessages enemy = [mkMsgTo EnemyMsgClearAttack (E._msgId enemy)]
