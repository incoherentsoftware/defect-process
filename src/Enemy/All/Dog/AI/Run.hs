module Enemy.All.Dog.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)

import Attack
import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Dog
import Constants
import Enemy as E
import Enemy.All.Dog.Behavior
import Enemy.All.Dog.Data
import Enemy.All.Dog.Projectile
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> DogEnemyBehaviorInstr -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs'
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                          -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl                 -> updateIdleBehavior idleTtl enemy
            StartRunTowardsInstr                    -> startRunTowardsBehavior enemy
            UpdateRunTowardsInstr runTowardsTtl     -> updateRunTowardsBehavior runTowardsTtl enemy
            StartRunFromInstr                       -> startRunFromBehavior enemy
            UpdateRunFromInstr runFromTtl           -> updateRunFromBehavior runFromTtl enemy
            StartPaceForwardsInstr                  -> startPaceForwardsBehavior enemy
            UpdatePaceForwardsInstr paceForwardsTtl -> updatePaceForwardsBehavior paceForwardsTtl enemy
            StartPaceTurnAroundInstr                -> startPaceTurnAroundBehavior enemy
            UpdatePaceTurnAroundInstr               -> updatePaceTurnAroundBehavior enemy
            StartAttackInstr atkDesc                -> startAttackBehavior atkDesc enemy
            SetPostAttackCooldownInstr              -> setPostAttackCooldownMessages enemy
            CreateAttackProjInstr                   -> createAttackProjectileMessages enemy
            FlipDirectionInstr                      -> flipDirectionMessages enemy
            UpdateWillUseAttackProjInstr            -> updateWillUseAttackProjMessages enemy
            UpdateHurtInstr hurtTtl hurtType        -> updateHurtBehavior hurtTtl hurtType enemy
            StartLaunchedInstr hangtimeTtl          -> startLaunchedBehavior hangtimeTtl enemy
            LaunchedHangtimeInstr hangtimeTtl       -> launchedHangtimeBehavior hangtimeTtl enemy
            StartFallenInstr fallenTtl              -> startFallenBehavior fallenTtl enemy
            UpdateFallenInstr fallenTtl             -> updateFallenBehavior fallenTtl enemy
            StartGetUpInstr                         -> startGetUpBehavior enemy
            StartWallSplatInstr                     -> startWallSplatBehavior enemy
            UpdateWallSplatInstr wallSplatTtl       -> updateWallSplatBehavior wallSplatTtl enemy
            UpdateSpawnInstr                        -> updateSpawnBehavior enemy
            StartDeathInstr                         -> startDeathBehavior enemy
            SetDeadInstr                            -> enemySetDeadMessages enemy

        cfg                   = _dog $ _config (_data enemy)
        tauntedIdleSecs       = _tauntedIdleSecs cfg
        tauntedPaceSecs       = _tauntedPaceSecs cfg
        tauntedMaxRunFromSecs = _tauntedMaxRunFromSecs cfg

        aiEnabledMsgs' = case enemyTauntedStatus enemy of
            EnemyTauntedInactive -> aiEnabledMsgs
            EnemyTauntedActive   -> case cmd of
                UpdateIdleInstr idleTtl
                    | idleTtl > tauntedIdleSecs          -> updateIdleBehavior tauntedIdleSecs enemy
                UpdatePaceForwardsInstr paceForwardsTtl
                    | paceForwardsTtl > tauntedPaceSecs  -> updatePaceForwardsBehavior tauntedPaceSecs enemy
                UpdateRunFromInstr runFromTtl
                    | runFromTtl > tauntedMaxRunFromSecs -> updateRunTowardsBehavior tauntedMaxRunFromSecs enemy
                _                                        -> aiEnabledMsgs

        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior (_data enemy) of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                StartRunTowardsInstr      -> setIdleMsgs
                UpdateRunTowardsInstr _   -> setIdleMsgs
                StartRunFromInstr         -> setIdleMsgs
                UpdateRunFromInstr _      -> setIdleMsgs
                StartPaceForwardsInstr    -> setIdleMsgs
                UpdatePaceForwardsInstr _ -> setIdleMsgs
                StartPaceTurnAroundInstr  -> setIdleMsgs
                UpdatePaceTurnAroundInstr -> setIdleMsgs
                StartAttackInstr _        -> setIdleMsgs
                CreateAttackProjInstr     -> setIdleMsgs
                FlipDirectionInstr        -> setIdleMsgs
                _                         -> aiEnabledMsgs'

setPostAttackCooldownMessages :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
setPostAttackCooldownMessages enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData           = _data e
        postAtkCooldown = _postAttackCooldown $ _dog (_config eData)
    in e
        {_data = eData {_attackCooldown = postAtkCooldown}
        }

mkEnemyUpdateBehaviorMsg :: Enemy DogEnemyData -> DogEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (_data e) {_behavior = behavior}}

updateBehaviorIfMatching :: Enemy DogEnemyData -> DogEnemyBehavior -> DogEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

flipDirectionMessages :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
flipDirectionMessages enemy = mkEnemyUpdateMsg enemy $ \e -> e {_dir = flipDirection (E._dir e)}

startDeathBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

startLaunchedBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _dog enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

startAttackBehavior :: AttackDescription -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkDesc enemy = setBehaviorMsg ++ setAttackMsg
    where
        setBehaviorMsg = mkEnemyUpdateBehaviorMsg enemy AttackBehavior
        enemyId        = _msgId enemy
        setAttackMsg   = [mkMsgTo (EnemyMsgSetAttackDesc atkDesc) enemyId]

createAttackProjectileMessages :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
createAttackProjectileMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM (mkDogProjectile enemy)]

updateWillUseAttackProjMessages :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWillUseAttackProjMessages enemy = mkEnemyUpdateMsgM enemy $ \e -> do
    let eData       = _data e
    willUseAtkProj <- rollWillUseAttackProjectile $ _config eData
    return $ e
        { _data = eData {_willUseAttackProjectile = willUseAtkProj}
        }

updateHurtBehavior :: Secs -> HurtType -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior (hurtTtl - timeStep) hurtType

startRunTowardsBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startRunTowardsBehavior enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where
        cfg      = _dog . _config $ _data enemy
        runSpeed = _runSpeed cfg

        x              = vecX $ E._pos enemy
        runTowardsSecs = case enemyKnownPlayerPos enemy of
            Nothing               -> _fallbackRunSecs cfg
            Just (Pos2 playerX _) -> abs (playerX - x) / runSpeed
        behavior       = RunTowardsBehavior runTowardsSecs

updateRunTowardsBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateRunTowardsBehavior runTowardsTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData          = _data e
        runTowardsTtl' = runTowardsTtl - timeStep
        runSpeed       = _runSpeed . _dog $ _config eData
        velX           = runSpeed * directionNeg (E._dir enemy)
        velY           = vecY $ _vel e
    in e
        { _data = eData {_behavior = RunTowardsBehavior runTowardsTtl'}
        , _vel  = Vel2 velX velY
        }

startRunFromBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startRunFromBehavior enemy = mkEnemyUpdateMsgM enemy $ \e ->
    let
        eData          = _data e
        cfg            = _dog $ _config eData
        minRunFromSecs = _minRunFromSecs cfg
        maxRunFromSecs = _maxRunFromSecs cfg
    in do
        retreatSecs <- liftIO $ randomRIO (minRunFromSecs, maxRunFromSecs)
        return $ e
            { _data   = eData {_behavior = RunFromBehavior retreatSecs}
            , _dir    = flipDirection $ E._dir e
            , _attack = Nothing
            }

updateRunFromBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateRunFromBehavior runFromTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData       = _data e
        runFromTtl' = runFromTtl - timeStep
        runSpeed    = _runSpeed . _dog $ _config eData
        velX        = runSpeed * directionNeg (E._dir enemy)
        velY        = vecY $ _vel e
    in e
        { _data = eData {_behavior = RunFromBehavior runFromTtl'}
        , _vel  = Vel2 velX velY
        }

startIdleBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = _data e
        idleSecs = _idleSecs $ _dog (_config eData)
    in e
        { _data = eData {_behavior = IdleBehavior idleSecs}
        , _vel  = Vel2 0.0 (vecY $ _vel e)
        }

updateIdleBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

startFallenBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startGetUpBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startGetUpBehavior enemy = mkEnemyUpdateBehaviorMsg enemy GetUpBehavior

updateSpawnBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
        | spriteFinished spr                        -> startIdleBehavior enemy
    _                                               -> []

startPaceForwardsBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startPaceForwardsBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = _data e
        paceSecs = _paceSecs . _dog $ _config eData
        behavior = PaceBehavior $ PaceForwards paceSecs
    in e
        { _data   = eData {_behavior = behavior}
        , _attack = Nothing
        }

startPaceTurnAroundBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
startPaceTurnAroundBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data   = (_data e) {_behavior = PaceBehavior PaceTurnAround}
    , _attack = Nothing
    }

updatePaceForwardsBehavior :: Secs -> Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updatePaceForwardsBehavior paceForwardsTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData            = _data e
        paceForwardsTtl' = paceForwardsTtl - timeStep
        behavior         = PaceBehavior $ PaceForwards paceForwardsTtl'
        paceSpeed        = _paceSpeed . _dog $ _config eData
        dir              = E._dir e
        velX             = paceSpeed * directionNeg dir
        velY             = vecY $ _vel e
    in e
        { _data = eData {_behavior = behavior}
        , _vel  = Vel2 velX velY
        }

updatePaceTurnAroundBehavior :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
updatePaceTurnAroundBehavior enemy = [mkMsgToEx (EnemyMsgSetVelocity gravityVel) enemyId MsgAfterNormalOrder]
    where
        gravity    = _gravity $ _config (_data enemy)
        gravityVel = Vel2 0.0 (gravity * timeStep)
        enemyId    = E._msgId enemy
