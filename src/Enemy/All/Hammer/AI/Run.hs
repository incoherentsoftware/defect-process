module Enemy.All.Hammer.AI.Run
    ( runBehaviorInstr
    ) where

import Attack
import Collision
import Configs.All.Enemy
import Configs.All.Enemy.Hammer
import Constants
import Enemy as E
import Enemy.All.Hammer.AttackDescriptions
import Enemy.All.Hammer.Behavior
import Enemy.All.Hammer.Data
import FileCache
import Msg
import Particle.All.Simple
import Util
import Window.Graphics
import World.ZIndex

packPath              = \f -> PackResourceFilePath "data/enemies/hammer-enemy.pack" f
hammerSummonOutPath   = packPath "hammer-summon-out.spr"         :: PackResourceFilePath
atkMeteorBodyLandPath = packPath "attack-meteor-body-land.spr"   :: PackResourceFilePath

runBehaviorInstr :: Bool -> HammerEnemyBehaviorInstr -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                         -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl                -> updateIdleBehavior idleTtl enemy
            StartPatrolInstr                       -> startPatrolBehavior enemy
            UpdatePatrolInstr                      -> updatePatrolBehavior enemy
            StartTeleportInstr                     -> startTeleportBehavior enemy
            UpdateTeleportInstr                    -> updateTeleportBehavior enemy
            StartReFormInstr                       -> startReFormBehavior enemy
            StartAttackLandInstr                   -> startAttackLandBehavior enemy
            StartAttackInstr atkDesc               -> startAttackBehavior atkDesc enemy
            CreateAttackLandParticlesInstr         -> createAttackLandsParticlesMessages enemy
            UpdateHurtInstr hurtTtl hurtType       -> updateHurtBehavior hurtTtl hurtType enemy
            StartLaunchedInstr hangtimeTtl         -> startLaunchedBehavior hangtimeTtl enemy
            LaunchedInHangtimeInstr hangtimeTtl    -> launchedInHangtimeBehavior hangtimeTtl enemy
            LaunchedNotInHangtimeInstr hangtimeTtl -> launchedNotInHangtimeBehavior hangtimeTtl enemy
            StartFallenInstr fallenTtl             -> startFallenBehavior fallenTtl enemy
            UpdateFallenInstr fallenTtl            -> updateFallenBehavior fallenTtl enemy
            StartSitUpInstr                        -> startSitUpBehavior enemy
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
                StartPatrolInstr   -> setIdleMsgs
                UpdatePatrolInstr  -> setIdleMsgs
                StartTeleportInstr -> setIdleMsgs
                StartAttackInstr _ -> setIdleMsgs
                _                  -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy HammerEnemyData -> HammerEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (_data e) {_behavior = behavior}}

updateBehaviorIfMatching :: Enemy HammerEnemyData -> HammerEnemyBehavior -> HammerEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (IdleBehavior _, IdleBehavior _)             -> behavior
    (HurtBehavior _ _, HurtBehavior _ _)         -> behavior
    (LaunchedBehavior _ _, LaunchedBehavior _ _) -> behavior
    (FallenBehavior _, FallenBehavior _)         -> behavior
    (WallSplatBehavior _, WallSplatBehavior _)   -> behavior
    _                                            -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

startIdleBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = _data e
        cfg      = _config eData
        idleSecs = _idleSecs $ _hammer cfg
    in e
        { _data   = eData {_behavior = IdleBehavior idleSecs}
        , _vel    = zeroVel2
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        idleTtl' = idleTtl - timeStep
        behavior = updateBehaviorIfMatching e (IdleBehavior idleTtl')
    in e
        { _data = (_data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

startTeleportBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startTeleportBehavior enemy = mkEnemyUpdateBehaviorMsg enemy TeleportBehavior

updateTeleportBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateTeleportBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        dir             = E._dir e
        Pos2 posX posY  = E._pos e
        teleportOffsetX = _teleportOffsetX . _hammer . _config $ _data e
        posX'           = posX + teleportOffsetX * directionNeg dir
        vel             = Vel2 (0.1 * directionNeg dir) 0.0
    in e
        { _pos = Pos2 posX' posY
        , _vel = vel
        }

updateHurtBehavior :: Secs -> HurtType -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        hurtTtl' = hurtTtl - timeStep
        behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)
    in e
        { _data = (_data e) {_behavior = behavior}
        }

startAttackBehavior :: AttackDescription -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkDesc enemy = setDirMsg:setAtkMsg:enemyUpdateMsg
    where
        setAtkMsg      = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) (_msgId enemy)
        enemyUpdateMsg = mkEnemyUpdateBehaviorMsg enemy AttackBehavior

        enemyX    = vecX $ E._pos enemy
        dir       = case enemyKnownPlayerPos enemy of
            Just (Pos2 playerX _)
                | playerX < enemyX -> LeftDir
                | playerX > enemyX -> RightDir
            _                      -> E._dir enemy
        setDirMsg = mkMsgToEx (EnemyMsgSetDirection dir) (_msgId enemy) MsgAfterNormalOrder

startAttackLandBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackLandBehavior enemy = setAtkLandMsg:mkBodyParticleMsg:enemyUpdateMsg
    where
        atkDesc       = _meteorHammerLand (_attackDescs (_data enemy) :: EnemyAttackDescriptions)
        setAtkLandMsg = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) (_msgId enemy)

        pos               = E._pos enemy
        dir               = E._dir enemy
        mkBodyParticle    = loadSimpleParticle pos dir enemyUnderBodyZIndex atkMeteorBodyLandPath
        mkBodyParticleMsg = mkMsg $ ParticleMsgAddM mkBodyParticle

        enemyUpdateMsg = mkEnemyUpdateBehaviorMsg enemy AttackLandBehavior

createAttackLandsParticlesMessages :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackLandsParticlesMessages enemy = [mkMsg $ ParticleMsgAddM mkParticle]
    where
        pos        = E._pos enemy
        dir        = E._dir enemy
        mkParticle = loadSimpleParticle pos dir enemyBodyZIndex hammerSummonOutPath

updateSpawnBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []

startDeathBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeSecs enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeSecs NotInHangtime

launchedInHangtimeBehavior :: Secs -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedInHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        hangtimeTtl' = hangtimeTtl - timeStep
        behavior     = updateBehaviorIfMatching e (LaunchedBehavior hangtimeTtl' InHangtime)
    in e
        { _data = (_data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

launchedNotInHangtimeBehavior :: Secs -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedNotInHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let behavior = updateBehaviorIfMatching e (LaunchedBehavior hangtimeTtl NotInHangtime)
    in e
        { _data = (_data e) {_behavior = behavior}
        }

startFallenBehavior :: Secs -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        fallenTtl' = fallenTtl - timeStep
        behavior   = updateBehaviorIfMatching e (FallenBehavior fallenTtl')
    in e
        { _data = (_data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

startSitUpBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startSitUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy SitUpBehavior

startReFormBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startReFormBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData     = _data e
        x         = vecX $ E._pos e
        startPosY = _startPosY eData
    in e
        { _data   = eData {_behavior = ReFormBehavior}
        , _pos    = Pos2 x startPosY
        , _attack = Nothing
        }

startWallSplatBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _hammer enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        wallSplatTtl' = wallSplatTtl - timeStep
        behavior      = updateBehaviorIfMatching e (WallSplatBehavior wallSplatTtl')
    in e
        { _data = (_data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

startPatrolBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
startPatrolBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e)
        { _behavior        = PatrolBehavior
        , _willUseTeleport = False
        }
    }

updatePatrolBehavior :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
updatePatrolBehavior enemy = mkEnemyUpdateMsgM enemy $ \e ->
    let
        dir         = enemyFlippedDirIfWallOrGround e
        eData       = _data e
        cfg         = _config eData
        hammerCfg   = _hammer cfg
        patrolSpeed = _patrolSpeed hammerCfg
        vel         = Vel2 (patrolSpeed * directionNeg dir) 0.0

        reroll = case E._sprite e of
            Nothing  -> False
            Just spr ->
                let
                    loopCount                = spriteLoopFrameCount spr
                    loopCount'               = spriteLoopFrameCount $ updateSprite spr
                    rerollPerPatrolLoopCount = _rerollPerPatrolLoopCount hammerCfg
                in loopCount' > loopCount && loopCount' `mod` FrameIndex rerollPerPatrolLoopCount == 0
    in do
        willUseTeleport <- if
            | reroll    -> rollWillUseTeleport cfg
            | otherwise -> return $ _willUseTeleport eData

        return $ e
            { _data = eData {_willUseTeleport = willUseTeleport}
            , _dir  = dir
            , _vel  = vel
            }
