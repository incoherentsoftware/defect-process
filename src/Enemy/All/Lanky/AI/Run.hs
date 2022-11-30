module Enemy.All.Lanky.AI.Run
    ( runBehaviorInstr
    ) where

import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Lanky
import Constants
import Enemy as E
import Enemy.All.Lanky.AttackDescriptions
import Enemy.All.Lanky.AttackType
import Enemy.All.Lanky.Behavior
import Enemy.All.Lanky.Data
import Enemy.All.Lanky.Projectile
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> LankyEnemyBehaviorInstr -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
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
            FacePlayerInstr                   -> facePlayerMessages enemy
            StartAttackInstr atkType          -> startAttackBehavior atkType enemy
            CreateAttackPillarInstr           -> createAttackPillarMessages enemy
            SetSummonAtkCooldownInstr         -> setSummonAtkCooldownMessages enemy
            SetBeamAtkCooldownInstr           -> setBeamAtkCooldownMessages enemy
            UpdateHurtInstr hurtTtl hurtType  -> updateHurtBehavior hurtTtl hurtType enemy
            StartLaunchedInstr hangtimeTtl    -> startLaunchedBehavior hangtimeTtl enemy
            LaunchedHangtimeInstr hangtimeTtl -> launchedHangtimeBehavior hangtimeTtl enemy
            UpdateKneelingInstr kneelingTtl   -> updateKneelingBehavior kneelingTtl enemy
            StartGetUpInstr                   -> startGetUpBehavior enemy
            StartWallSplatInstr wallSplatTtl  -> startWallSplatBehavior wallSplatTtl enemy
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
                _                    -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy LankyEnemyData -> LankyEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = behavior}
    }

updateBehaviorIfMatching :: Enemy LankyEnemyData -> LankyEnemyBehavior -> LankyEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ E._data enemy

setSummonAtkCooldownMessages :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
setSummonAtkCooldownMessages enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData = E._data e
        cfg   = _lanky $ _config eData
    in e
        { _data = eData {_summonAtkCooldownTtl = _summonAtkCooldownSecs cfg}
        }

setBeamAtkCooldownMessages :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
setBeamAtkCooldownMessages enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData = E._data e
        cfg   = _lanky $ _config eData
    in e
        { _data = eData {_beamAtkCooldownTtl = _beamAtkCooldownSecs cfg}
        }

facePlayerMessages :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
facePlayerMessages enemy = case vecX <$> enemyKnownPlayerPos enemy of
    Just playerX ->
        let
            x   = vecX $ E._pos enemy
            dir = if playerX < x then LeftDir else RightDir
        in [mkMsgTo (EnemyMsgSetDirection dir) (E._msgId enemy)]
    Nothing      -> []

createAttackPillarMessages :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackPillarMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM mkAtkPillarProj]
    where
        enemyData                                          = E._data enemy
        Pos2 lastKnownPlayerGroundX lastKnownPlayerGroundY = _lastKnownPlayerGroundPos enemyData

        y                                = vecY $ E._pos enemy
        pillarPosY
            | lastKnownPlayerGroundY > y = lastKnownPlayerGroundY
            | otherwise                  = y
        pillarPos                        = Pos2 lastKnownPlayerGroundX pillarPosY

        dir             = E._dir enemy
        atkPillarDesc   = _pillar $ _attackDescs enemyData
        mkAtkPillarProj = mkLankyProjectile pillarPos dir atkPillarDesc

startDeathBehavior :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

updateHurtBehavior :: Secs -> HurtType -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        hurtTtl' = hurtTtl - timeStep
        behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)
    in e
        { _data = (_data e) {_behavior = behavior}
        }

startLaunchedBehavior :: Secs -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = LaunchedBehavior hangtimeTtl}
    }

launchedHangtimeBehavior :: Secs -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startGetUpBehavior :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = GetUpBehavior}
    }

startWallSplatBehavior :: Secs -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior wallSplatTtl enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        effectDrawScale = _wallImpactEffectDrawScale . _lanky . _config $ _data enemy
        updateEnemyMsg  = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (E._data e) {_behavior = WallSplatBehavior wallSplatTtl}
            }

updateWallSplatBehavior :: Secs -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = behavior}
    , _vel  = zeroVel2
    }
    where behavior = WallSplatBehavior $ wallSplatTtl - timeStep

startAttackBehavior :: LankyEnemyAttackType -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkType enemy = attackMsg:behaviorDataMsgs
    where
        enemyData = _data enemy
        hasAura   = hasLankyEnemyDataAura enemyData
        atkDescs  = _attackDescs enemyData
        atkDesc   = case atkType of
            SummonAttackType
                | hasAura   -> _summonAura atkDescs
                | otherwise -> _summon atkDescs
            BeamAttackType
                | hasAura   -> _beamAura atkDescs
                | otherwise -> _beam atkDescs

        enemyId   = E._msgId enemy
        attackMsg = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) enemyId

        behaviorDataMsgs = mkEnemyUpdateMsg enemy $ \e ->
            let
                x           = vecX $ E._pos enemy
                dir         = case vecX <$> enemyKnownPlayerPos enemy of
                    Just playerX
                        | playerX < x -> LeftDir
                        | playerX > x -> RightDir
                    _                 -> E._dir enemy
            in e
                { _data = (E._data e) { _behavior = AttackBehavior}
                , _dir  = dir
                }

startIdleBehavior :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = E._data e
        idleSecs = _idleSecs . _lanky $ _config eData
    in e
        { _data   = eData {_behavior = IdleBehavior idleSecs}
        , _vel    = zeroVel2
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where
        idleTtl' = idleTtl - timeStep
        behavior = IdleBehavior idleTtl'

startWalkBehavior :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWalkBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = E._data e
        lankyCfg = _lanky $ _config eData
    in e
        { _data   = eData {_behavior = WalkBehavior $ _walkSecs lankyCfg}
        , _attack = Nothing
        }

updateWalkBehavior :: Secs -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWalkBehavior walkTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData     = E._data e
        walkTtl'  = walkTtl - timeStep
        dir       = enemyFlippedDirIfWallOrGround e
        walkSpeed = _walkSpeed . _lanky $ _config eData
        vel       = Vel2 (walkSpeed * directionNeg dir) 0.0
    in e
        { _data = eData {_behavior = WalkBehavior walkTtl'}
        , _dir  = dir
        , _vel  = vel
        }

startRetreatBehavior :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
startRetreatBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = E._data e
        lankyCfg = _lanky $ _config eData
    in e
        { _data   = eData {_behavior = RetreatBehavior $ _retreatSecs lankyCfg}
        , _attack = Nothing
        }

updateRetreatBehavior :: Secs -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateRetreatBehavior retreatTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData     = E._data e
        walkSpeed = _walkSpeed . _lanky $ _config eData
        dir       = E._dir e
        vel       = Vel2 (walkSpeed * directionPos dir) 0.0
    in e
        { _data = eData {_behavior = RetreatBehavior $ retreatTtl - timeStep}
        , _vel  = vel
        }

updateSpawnBehavior :: Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
        | spriteFinished spr                        -> startIdleBehavior enemy
    _                                               -> []

updateKneelingBehavior :: Secs -> Enemy LankyEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateKneelingBehavior kneelingTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = KneelingBehavior $ kneelingTtl - timeStep
