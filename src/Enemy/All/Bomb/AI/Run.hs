module Enemy.All.Bomb.AI.Run
    ( runBehaviorInstr
    ) where

import qualified Data.Set as S

import Configs.All.Enemy
import Configs.All.Enemy.Bomb
import Constants
import Enemy as E
import Enemy.All.Bomb.AttackDescriptions
import Enemy.All.Bomb.Behavior
import Enemy.All.Bomb.Data
import InfoMsg.Util
import Msg
import Projectile
import Util
import Window.Graphics

explosionRegisteredCollisions = S.fromList
    [ ProjRegisteredPlayerCollision
    , ProjRegisteredRoomItemCollision
    , ProjRegisteredEnemyCollision
    ] :: S.Set ProjectileRegisteredCollision

runBehaviorInstr :: Bool -> BombEnemyBehaviorInstr -> Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | not aiEnabled                  = aiDisabledMsgs
    | isExplodeTimerActive enemyData = aiDisabledMsgs
    | otherwise                      = aiEnabledMsgs'
    where
        enemyData = E._data enemy

        aiEnabledMsgs = case cmd of
            StartIdleInstr                       -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl              -> updateIdleBehavior idleTtl enemy
            StartSearchInstr                     -> startSearchBehavior enemy
            UpdateSearchInstr searchTtl numTurns -> updateSearchBehavior searchTtl numTurns enemy
            StartSprintInstr                     -> startSprintBehavior enemy
            UpdateSprintInstr sprintTtl          -> updateSprintBehavior sprintTtl enemy
            StartExplodeInstr                    -> startExplodeBehavior enemy
            CreateExplosionInstr                 -> createExplosionMessages enemy
            UpdateHurtInstr hurtTtl hurtType     -> updateHurtBehavior hurtTtl hurtType enemy
            StartLaunchedInstr hangtimeTtl       -> startLaunchedBehavior hangtimeTtl enemy
            UpdateLaunchedInstr hangtimeTtl      -> updateLaunchedBehavior hangtimeTtl enemy
            StartWallSplatInstr                  -> startWallSplatBehavior enemy
            UpdateWallSplatInstr wallSplatTtl    -> updateWallSplatBehavior wallSplatTtl enemy
            UpdateSpawnInstr                     -> updateSpawnBehavior enemy
            SetDeadInstr                         -> enemySetDeadMessages enemy

        aiEnabledMsgs' = case enemyTauntedStatus enemy of
            EnemyTauntedInactive -> aiEnabledMsgs
            EnemyTauntedActive   -> case cmd of
                StartIdleInstr        -> startSprintBehavior enemy
                UpdateIdleInstr _     -> startSprintBehavior enemy
                StartSearchInstr      -> startSprintBehavior enemy
                UpdateSearchInstr _ _ -> startSprintBehavior enemy
                _                     -> aiEnabledMsgs

        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior enemyData of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                StartSearchInstr    -> setIdleMsgs
                StartSprintInstr    -> setIdleMsgs
                UpdateSprintInstr _ -> setIdleMsgs
                StartExplodeInstr   -> setIdleMsgs
                _                   -> aiEnabledMsgs'

mkEnemyUpdateBehaviorMsg :: Enemy BombEnemyData -> BombEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { E._data = (E._data e) {_behavior = behavior}
    }

updateBehaviorIfMatching :: Enemy BombEnemyData -> BombEnemyBehavior -> BombEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ E._data enemy

createExplosionMessages :: Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
createExplosionMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM mkExplosionProj]
    where
        pos              = E._pos enemy
        dir              = E._dir enemy
        explosionAtkDesc = _attackExplosion $ _attackDescs (E._data enemy)
        mkExplosionProj  =
            mkEnemyAttackProjectileEx pos dir explosionAtkDesc (enemyTauntedStatus enemy) explosionRegisteredCollisions

updateSpawnBehavior :: Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []

startExplodeBehavior :: Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
startExplodeBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData            = E._data e
        explodeTimerSecs = _attackExplodeTimerSecs $ _bomb (_config eData)
    in e
        { E._data = eData {_explodeTimerTtl = Just explodeTimerSecs}
        }

startLaunchedBehavior :: Secs -> Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy (LaunchedBehavior hangtimeTtl)

updateLaunchedBehavior :: Secs -> Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { E._data = (E._data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

updateHurtBehavior :: Secs -> HurtType -> Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { E._data = (E._data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior (hurtTtl - timeStep) hurtType

startIdleBehavior :: Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData    = E._data e
        idleSecs = _idleSecs . _bomb $ _config eData
    in e
        { E._data   = eData {_behavior = IdleBehavior idleSecs}
        , _vel    = Vel2 0.0 (vecY $ E._vel e)
        , _attack = Nothing
        }

updateIdleBehavior :: Secs -> Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

startSearchBehavior :: Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
startSearchBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData          = E._data e
        searchTurnSecs = _searchTurnSecs . _bomb $ _config eData
    in e
        { E._data = eData {_behavior = SearchBehavior searchTurnSecs 0}
        }

updateSearchBehavior :: Secs -> Int -> Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSearchBehavior searchTtl numTurns enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        searchTtl'     = searchTtl - timeStep
        eData          = E._data e
        searchTurnSecs = _searchTurnSecs . _bomb $ _config eData
        dir            = E._dir enemy

        (behavior, dir')
            | searchTtl' <= 0.0 = (SearchBehavior searchTurnSecs (numTurns + 1), flipDirection dir)
            | otherwise         = (SearchBehavior searchTtl' numTurns, dir)
    in e
        { E._data = eData {_behavior = behavior}
        , _dir  = dir'
        }

startSprintBehavior :: Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
startSprintBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData         = E._data e
        maxSprintSecs = _maxSprintSecs . _bomb $ _config eData
    in e
        { _data   = eData {_behavior = SprintBehavior maxSprintSecs}
        , _attack = Nothing
        }

updateSprintBehavior :: Secs -> Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSprintBehavior sprintTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData      = E._data e
        cfg        = _bomb $ _config eData
        sprintTtl' = sprintTtl - timeStep
        vel        = Vel2 (_sprintSpeed cfg * directionNeg dir) 0.0

        distanceX     = case vecX . playerInfoPos <$> _knownPlayerInfo enemy of
            Just playerX -> abs $ playerX - vecX (E._pos enemy)
            Nothing      -> 0.0
        isMaxDistance = distanceX >= _maxSprintWrongWayDistanceX cfg

        dir                                                    = E._dir enemy
        dir'
            | isMaxDistance && not (isEnemyFacingPlayer enemy) = flipDirection dir
            | otherwise                                        = enemyFlippedDirIfWallOrGround e

    in e
        { _data = eData {_behavior = SprintBehavior sprintTtl'}
        , _dir  = dir'
        , _vel  = vel
        }

startWallSplatBehavior :: Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = enemyWallImpactMessages effectDrawScale enemy ++ updateEnemyMsg
    where
        enemyCfg        = _config $ E._data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _bomb enemyCfg

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { E._data = (E._data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { E._data = (E._data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }
