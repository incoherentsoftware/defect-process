module Enemy.All.Boss.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, modify)
import Data.Traversable       (for)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Attack
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Boss
import Constants
import Enemy as E
import Enemy.All.Boss.AttackDescriptions
import Enemy.All.Boss.Behavior
import Enemy.All.Boss.BlobProjectile
import Enemy.All.Boss.Data
import Enemy.All.Boss.HopProjectile
import Enemy.All.Boss.LankyProjectile
import Enemy.All.Boss.LightingFakeProjectile
import Enemy.All.Boss.Sprites
import Enemy.All.Boss.SummonFlyingSpawnerProjectile
import Enemy.All.Boss.SummonSpearsSpawnerProjectile
import Enemy.All.Boss.SummonWallSpawnerProjectile
import Enemy.All.Boss.TurretProjectile
import Enemy.All.Boss.Util
import FileCache
import Id
import InfoMsg.Util
import Msg
import Particle.All.Simple
import Util
import Window.Graphics
import World.ZIndex

soundFrameTagName  = FrameTagName "sound"  :: FrameTagName
darkenFrameTagName = FrameTagName "darken" :: FrameTagName

bossSpawnSoundPath = "event:/SFX Events/Enemy/Boss/spawn" :: FilePath
bossDeathSoundPath = "event:/SFX Events/Enemy/Boss/death" :: FilePath

deathEffectPath = PackResourceFilePath "data/enemies/boss-enemy-death.pack" "death-effect.spr" :: PackResourceFilePath

airGuardVel  = Vel2 0.0 0.1 :: Vel2
arenaCenterX = 1160         :: PosX

lankyProjPosMultiplierChoices =
    [ NE.fromList [0.05, 0.95]
    , NE.fromList [0.15, 0.25]
    , NE.fromList [0.35, 0.45]
    , NE.fromList [0.55, 0.65]
    , NE.fromList [0.75, 0.85]
    ] :: [NE.NonEmpty Float]

runBehaviorInstr :: MonadIO m => Bool -> BossEnemyBehaviorInstr -> Enemy BossEnemyData -> m [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy = do
    aiEnabledMsgs <- case cmd of
        StartIdleInstr                     -> return $ startIdleBehavior enemy
        UpdateIdleInstr idleTtl            -> return $ updateIdleBehavior idleTtl enemy
        FacePlayerInstr                    -> return $ facePlayerMessages enemy
        SetAttackInstr atkDesc             -> return $ setAttackMessages atkDesc enemy
        StartHpThresholdSummonFlyingInstr  -> return $ startHpThresholdSummonFlyingBehavior enemy
        StartHpThresholdSummonSpearsInstr  -> return $ startHpThresholdSummonSpearsBehavior enemy
        StartHpThresholdSummonWallsInstr   -> return $ startHpThresholdSummonWallsBehavior enemy
        StartHpThresholdPhaseOutInstr      -> startHpThresholdPhaseOutBehavior enemy
        StartHpThresholdAttackInstr        -> return $ startHpThresholdAttackBehavior enemy
        TeleportToPreHpThresholdPosInstr   -> return $ teleportToPreHpThresholdPosBehavior enemy
        StartAttackShortInstr              -> return $ startAttackShortBehavior enemy
        StartAttackMediumAirInstr          -> return $ startAttackMediumAirBehavior enemy
        StartAttackLongInstr               -> return $ startAttackLongBehavior enemy
        CreateBlobProjectileInstr          -> return $ createBlobProjectileMessages enemy
        CreateTurret1ProjectileInstr       -> return $ createTurret1ProjectileMessages enemy
        CreateTurret2ProjectileInstr       -> return $ createTurret2ProjectileMessages enemy
        CreateHopProjectilesInstr          -> return $ createHopProjectilesMessages enemy
        CreateLankyProjectilesInstr        -> return $ createLankyProjectilesMessages enemy
        StartGuardInstr                    -> return $ startGuardBehavior enemy
        StartAirGuardInstr                 -> return $ startAirGuardBehavior enemy
        UpdateAirGuardInstr                -> return $ updateAirGuardBehavior enemy
        UpdateHurtInstr hurtSecs hurtTtl   -> return $ updateHurtBehavior hurtSecs hurtTtl enemy
        StartLaunchedInstr hangtimeTtl     -> return $ startLaunchedBehavior hangtimeTtl enemy
        LaunchedHangtimeInstr hangtimeTtl  -> return $ launchedHangtimeBehavior hangtimeTtl enemy
        UpdateKneelingInstr kneelingTtl    -> return $ updateKneelingBehavior kneelingTtl enemy
        StartGetUpInstr                    -> return $ startGetUpBehavior enemy
        StartWallSplatInstr wallSplatTtl   -> return $ startWallSplatBehavior wallSplatTtl enemy
        UpdateWallSplatInstr wallSplatTtl  -> return $ updateWallSplatBehavior wallSplatTtl enemy
        UpdateSpawnInstr                   -> return $ updateSpawnBehavior enemy
        StartDeathInstr                    -> return $ startDeathBehavior enemy
        UpdateDeathInstr                   -> return $ updateDeathBehavior enemy
        StartAirDeathInstr                 -> return $ startAirDeathBehavior enemy
        UpdateAirDeathInstr                -> return $ updateAirDeathBehavior enemy
        SetDeadInstr                       -> return $ enemySetDeadMessages enemy
        PlaySoundContinuousInstr soundPath -> return $ playSoundContinuousMessages soundPath enemy
        TeleportToPlayerInstr              -> return $ teleportToPlayerMessages enemy

    let
        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior (_data enemy) of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                FacePlayerInstr           -> setIdleMsgs
                StartAttackShortInstr     -> setIdleMsgs
                StartAttackMediumAirInstr -> setIdleMsgs
                StartAttackLongInstr      -> setIdleMsgs
                _                         -> aiEnabledMsgs

    return $ if
        | aiEnabled -> aiEnabledMsgs
        | otherwise -> aiDisabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy BossEnemyData -> BossEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (E._data e) {_behavior = behavior}}

updateBehaviorIfMatching :: Enemy BossEnemyData -> BossEnemyBehavior -> BossEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ E._data enemy

facePlayerMessages :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
facePlayerMessages enemy = case vecX <$> enemyKnownPlayerPos enemy of
    Just playerX ->
        let
            x   = vecX $ E._pos enemy
            dir = if playerX < x then LeftDir else RightDir
        in [mkMsgToEx (EnemyMsgSetDirection dir) (E._msgId enemy) MsgAfterNormalOrder]
    Nothing      -> []

updateHurtBehavior :: Secs -> HurtType -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)}
    }
    where hurtTtl' = hurtTtl - timeStep

startLaunchedBehavior :: Secs -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = LaunchedBehavior hangtimeTtl}
    }

launchedHangtimeBehavior :: Secs -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

updateKneelingBehavior :: Secs -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateKneelingBehavior kneelingTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = KneelingBehavior $ kneelingTtl - timeStep

startGetUpBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = GetUpBehavior}
    }

startWallSplatBehavior :: Secs -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior wallSplatTtl enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        effectDrawScale = _wallImpactEffectDrawScale . _boss . _config $ _data enemy
        updateEnemyMsg  = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (E._data e) {_behavior = WallSplatBehavior wallSplatTtl}
            }

updateWallSplatBehavior :: Secs -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = behavior}
    , _vel  = zeroVel2
    }
    where behavior = WallSplatBehavior $ wallSplatTtl - timeStep

startIdleBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = E._data e
        idleSecs = _idleSecs . _boss $ _config eData
    in e
        { _data   = eData {_behavior = IdleBehavior idleSecs}
        , _vel    = zeroVel2
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

updateSpawnBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Nothing  -> []
    Just spr -> flip execState [] $ do
        when (soundFrameTagName `isSpriteFrameTag` spr && _frameChanged spr) $
            let pos = E._pos enemy
            in modify (mkMsg (AudioMsgPlaySound bossSpawnSoundPath pos):)

        when (darkenFrameTagName `isSpriteFrameTag` spr && _frameChanged spr) $
            let enemyId = E._msgId enemy
            in modify (mkMsg (NewThinkProjectileMsgAddM $ mkLightingFakeProjectile enemyId):)

setAttackMessages :: AttackDescription -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
setAttackMessages atkDesc enemy = [mkMsgTo (EnemyMsgSetAttackDesc atkDesc) (E._msgId enemy)]

teleportToPlayerMessages :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
teleportToPlayerMessages enemy = [mkMsgTo (EnemyMsgUpdate updatePosDir) (E._msgId enemy)]
    where
        pos                                    = E._pos enemy
        playerGroundPos@(Pos2 playerGroundX _) = maybe pos _groundBeneathPos (_knownPlayerInfo enemy)
        dir'
            | playerGroundX < arenaCenterX     = LeftDir
            | otherwise                        = RightDir

        teleportOffset  = _teleportOffset . _boss . _config $ _data enemy
        teleportOffset' = vecFlip teleportOffset dir'
        teleportPos     = playerGroundPos `vecAdd` teleportOffset'

        updatePosDir = \e -> (e :: Enemy BossEnemyData)
            { _pos = teleportPos
            , _dir = dir'
            }

playSoundContinuousMessages :: FilePath -> Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
playSoundContinuousMessages soundPath enemy = [mkMsg $ AudioMsgPlaySoundContinuous soundPath hashedId pos]
    where
        hashedId = hashId $ E._msgId enemy
        pos      = E._pos enemy

createBlobProjectileMessages :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
createBlobProjectileMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM (mkBlobProjectile pos dir enemyData)]
    where
        pos       = E._pos enemy
        dir       = E._dir enemy
        enemyData = E._data enemy

createTurret1ProjectileMessages :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
createTurret1ProjectileMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM (mkTurretProjectile pos dir enemyData)]
    where
        enemyData = _data enemy
        offset    = _turret1ProjOffset $ _boss (_config enemyData)
        dir       = E._dir enemy
        pos       = E._pos enemy `vecAdd` vecFlip offset dir

createTurret2ProjectileMessages :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
createTurret2ProjectileMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM (mkTurretProjectile pos dir enemyData)]
    where
        enemyData = _data enemy
        offset    = _turret2ProjOffset $ _boss (_config enemyData)
        dir       = E._dir enemy
        pos       = E._pos enemy `vecAdd` vecFlip offset dir

createHopProjectilesMessages :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
createHopProjectilesMessages enemy =
    [ mkMsg $ NewThinkProjectileMsgAddM (mkHopProjectile pos LeftDir enemyData)
    , mkMsg $ NewThinkProjectileMsgAddM (mkHopProjectile pos RightDir enemyData)
    ]
    where
        pos       = E._pos enemy
        enemyData = E._data enemy

createLankyProjectilesMessages :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
createLankyProjectilesMessages enemy = [mkMsg $ NewThinkProjectileMsgAddsM mkLankyProjs]
    where
        Pos2 _ y             = E._pos enemy
        enemyData            = E._data enemy
        knownInnerLeftWallX  = _knownInnerLeftWallX enemyData
        knownInnerRightWallX = _knownInnerRightWallX enemyData
        arenaWidth           = knownInnerRightWallX - knownInnerLeftWallX
        enemyId              = E._msgId enemy

        mkLankyProjs = do
            reappearAtkIdx <- randomChoice $ 0 NE.:| [0..length lankyProjPosMultiplierChoices - 1]
            for (zip [0..] lankyProjPosMultiplierChoices) $ \(i, multChoices) -> do
                mult <- randomChoice multChoices
                let
                    pos           = Pos2 (knownInnerLeftWallX + arenaWidth * mult) y
                    isReappearAtk = i == reappearAtkIdx
                mkLankyProjectile pos RightDir enemyId enemyData isReappearAtk

startHpThresholdSummonFlyingBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startHpThresholdSummonFlyingBehavior enemy = [mkMsg $ NewThinkProjectileMsgAddM mkSummonFlyingSpawnerProj]
    where
        pos                       = E._pos enemy
        enemyData                 = E._data enemy
        msgId                     = E._msgId enemy
        mkSummonFlyingSpawnerProj = mkSummonFlyingSpawnerProjectile pos enemyData msgId

startHpThresholdSummonSpearsBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startHpThresholdSummonSpearsBehavior enemy = [mkMsg $ NewThinkProjectileMsgAddM mkSummonSpearsSpawnerProj]
    where
        pos                       = E._pos enemy
        enemyData                 = E._data enemy
        msgId                     = E._msgId enemy
        mkSummonSpearsSpawnerProj = mkSummonSpearsSpawnerProjectile pos enemyData msgId

startHpThresholdSummonWallsBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startHpThresholdSummonWallsBehavior enemy = [mkMsg $ NewThinkProjectileMsgAddM mkSummonWallSpawnerProj]
    where
        pos                     = E._pos enemy
        enemyData               = E._data enemy
        msgId                   = E._msgId enemy
        mkSummonWallSpawnerProj = mkSummonWallSpawnerProjectile pos enemyData msgId

teleportToHpThresholdAttackPosMessage :: Enemy BossEnemyData -> Msg ThinkEnemyMsgsPhase
teleportToHpThresholdAttackPosMessage enemy = mkMsgTo (EnemyMsgUpdate updatePosDir) (E._msgId enemy)
    where
        Pos2 _ y             = E._pos enemy
        enemyData            = E._data enemy
        knownInnerRightWallX = _knownInnerRightWallX enemyData
        bodyOffset           = _hpThresholdBodyOffset . _boss . _config $ _data enemy
        bodyPos              = Pos2 knownInnerRightWallX y `vecAdd` bodyOffset

        updatePosDir = \e -> (e :: Enemy BossEnemyData)
            { _pos = bodyPos
            , _dir = LeftDir
            }

startHpThresholdAttackBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startHpThresholdAttackBehavior enemy = [setPosMsg, attackMsg] ++ behaviorMsgs
    where
        atkDesc      = _summonIn . _summonAttackDesc . _hpThresholdAttackData $ _data enemy
        setPosMsg    = teleportToHpThresholdAttackPosMessage enemy
        attackMsg    = mkMsgTo (EnemyMsgSetAttackDesc atkDesc) (E._msgId enemy)
        behaviorMsgs = mkEnemyUpdateBehaviorMsg enemy AttackBehavior

startHpThresholdPhaseOutBehavior :: MonadIO m => Enemy BossEnemyData -> m [Msg ThinkEnemyMsgsPhase]
startHpThresholdPhaseOutBehavior enemy = do
    let
        enemyData             = E._data enemy
        phaseOutAtkDesc       = _phaseOut $ _attackDescs enemyData
        attackMsg             = mkMsgTo (EnemyMsgSetAttackDesc phaseOutAtkDesc) (E._msgId enemy)
        summonAtkDescChoices  = _summonAttackDescChoices (_hpThresholdAttackData enemyData)
        summonAtkDesc         = NE.head summonAtkDescChoices
        summonAtkDescChoices' = NE.last summonAtkDescChoices NE.:| NE.init summonAtkDescChoices

        updateDataMsgs = mkEnemyUpdateMsg enemy $ \e ->
            let
                eData              = E._data e
                hpThresholdAtkData = _hpThresholdAttackData eData
                thresholds         = M.fromList
                    [ (dmgThreshold, usedAtkDesc')
                    | threshold@(dmgThreshold, usedAtkDesc) <- M.toList $ _thresholds hpThresholdAtkData
                    , let isNewThreshold = isAtUnusedHpThreshold threshold e
                    , let usedAtkDesc'   = if isNewThreshold then Just summonAtkDesc else usedAtkDesc
                    ]
            in e
                { _data = eData
                    { _behavior              = AttackBehavior
                    , _hpThresholdAttackData = hpThresholdAtkData
                        { _phaseOutPos             = E._pos enemy
                        , _thresholds              = thresholds
                        , _summonAttackDesc        = summonAtkDesc
                        , _summonAttackDescChoices = summonAtkDescChoices'
                        }
                    }
                }

    return $ attackMsg:updateDataMsgs

teleportToPreHpThresholdPosBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
teleportToPreHpThresholdPosBehavior enemy =
    [ mkMsgTo (EnemyMsgUpdate updatePosDir) enemyId
    , mkMsgTo (EnemyMsgSetAttackDesc phaseInAtkDesc) enemyId
    ]
        where
            enemyData         = E._data enemy
            pos@(Pos2 x _)    = _phaseOutPos $ _hpThresholdAttackData enemyData
            Pos2 playerX _    = maybe pos playerInfoPos (_knownPlayerInfo enemy)
            dir
                | playerX < x = LeftDir
                | otherwise   = RightDir

            updatePosDir = \e -> (e :: Enemy BossEnemyData)
                { _pos = pos
                , _dir = dir
                }

            phaseInAtkDesc = _phaseIn $ _attackDescs enemyData
            enemyId        = E._msgId enemy

startAttackShortBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackShortBehavior enemy = attackMsg:mkEnemyUpdateBehaviorMsg enemy AttackBehavior
    where
        atkDesc   = randomChoice $ _attackShortChoices (E._data enemy)
        attackMsg = mkMsgTo (EnemyMsgSetAttackDescM atkDesc) (E._msgId enemy)

startAttackMediumAirBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackMediumAirBehavior enemy = attackMsg:mkEnemyUpdateBehaviorMsg enemy AttackBehavior
    where
        atkDesc   = randomChoice $ _attackMediumAirChoices (E._data enemy)
        attackMsg = mkMsgTo (EnemyMsgSetAttackDescM atkDesc) (E._msgId enemy)

startAttackLongBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackLongBehavior enemy = attackMsg:mkEnemyUpdateBehaviorMsg enemy AttackBehavior
    where
        atkDesc   = randomChoice $ _attackLongChoices (E._data enemy)
        attackMsg = mkMsgTo (EnemyMsgSetAttackDescM atkDesc) (E._msgId enemy)

startGuardBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGuardBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = GuardBehavior}
    }

startAirGuardBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAirGuardBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = AirGuardBehavior}
    }

updateAirGuardBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateAirGuardBehavior enemy = case _behavior (_data enemy) of
    AirGuardBehavior -> [mkMsgToEx (EnemyMsgSetVelocity airGuardVel) (E._msgId enemy) MsgAfterNormalOrder]
    _                -> []

deathSoundMsg :: Enemy BossEnemyData -> Msg ThinkEnemyMsgsPhase
deathSoundMsg enemy = mkMsg $ AudioMsgPlaySound bossDeathSoundPath pos
    where
        x       = vecX $ E._pos enemy
        centerY = vecY $ hitboxCenter (enemyHitbox enemy)
        pos     = Pos2 x centerY

deathEffectMsg :: Enemy BossEnemyData -> Msg ThinkEnemyMsgsPhase
deathEffectMsg enemy = mkMsg $ ParticleMsgAddM (loadSimpleParticle pos dir bossUnderBodyZIndex deathEffectPath)
    where
        pos = E._pos enemy
        dir = E._dir enemy

startDeathBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startDeathBehavior enemy = [deathSoundMsg enemy, deathEffectMsg enemy] ++ updateMsgs
    where
        updateMsgs = mkEnemyUpdateMsg enemy $ \e -> e
            { _data   = (_data e) {_behavior = DeathBehavior}
            , _vel    = zeroVel2
            , _attack = Nothing
            }

updateDeathBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateDeathBehavior enemy = [mkMsgToEx (EnemyMsgSetVelocity zeroVel2) (E._msgId enemy) MsgAfterNormalOrder]

startAirDeathBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAirDeathBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data   = (_data e) {_behavior = AirDeathBehavior}
    , _attack = Nothing
    }

updateAirDeathBehavior :: Enemy BossEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateAirDeathBehavior enemy
    | justTouchedGround =
        [ deathSoundMsg enemy
        , deathEffectMsg enemy
        , mkMsgToEx (EnemyMsgSetVelocity zeroVel2) (E._msgId enemy) MsgAfterNormalOrder
        ]
    | otherwise         = []
    where
        enemyData         = E._data enemy
        airDeathLandSpr   = _airDeathLand $ _sprites enemyData
        justTouchedGround = case E._sprite enemy of
            Nothing  -> False
            Just spr -> spr == airDeathLandSpr && _frameIndex spr == 0 && _frameChanged spr
