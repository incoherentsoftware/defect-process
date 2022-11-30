module Enemy.All.Bomb.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)

import Attack.Util
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Bomb
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Bomb.AI.Run
import Enemy.All.Bomb.Behavior
import Enemy.All.Bomb.Data
import Id
import Msg
import Util

fuseSoundFilePath = "event:/SFX Events/Enemy/Bomb/fuse-c" :: FilePath

thinkAI :: ConfigsRead m => EnemyThinkAI BombEnemyData m
thinkAI enemy =
    let
        enemyData  = _data enemy
        gravity    = _gravity $ _config enemyData
        gravityVel = Vel2 0.0 (gravity * timeStep)

        enemyId         = _msgId enemy
        behavior        = _behavior enemyData
        isSpawnBehavior = behavior == SpawnBehavior
        inWallSplat     = isWallSplatBehavior behavior
        inHangtimeVel   = enemyInHangtimeVel enemy (_config enemyData)
        inHangtime      = inHangtimeVel && case behavior of
            LaunchedBehavior hangtimeTtl
                | hangtimeTtl > 0.0 -> True
            HurtBehavior _ AirHurt  -> True
            _                       -> False
    in do
        aiEnabled <- not <$> readSettingsConfig _debug _disableAI

        return . flip execState [] $ do
            unless (inHangtime || isSpawnBehavior || inWallSplat) $
                modify (mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder:)

            unless (isSpawnBehavior || isExplodeTimerActive enemyData) $
                let
                    pos      = E._pos enemy
                    hashedId = hashId enemyId
                in modify (mkMsg (AudioMsgPlaySoundContinuous fuseSoundFilePath hashedId pos):)

            let
                runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
                behaviorInstrs    = thinkBehaviorInstrs enemy
            modify (++ concatMap runBehaviorInstr' behaviorInstrs)

            modify (++ mkEnemyUpdateDataMsg enemy)

mkEnemyUpdateDataMsg :: Enemy BombEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsg enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData           = _data e
        explodeTimerTtl = subtract timeStep <$> _explodeTimerTtl eData
    in e
        { _data = eData
            { _prevBehavior    = prevBehavior
            , _explodeTimerTtl = explodeTimerTtl
            }
        }
    where prevBehavior = _behavior $ _data enemy

inAttackExplodeRange :: Enemy BombEnemyData -> Bool
inAttackExplodeRange enemy = case enemyKnownPlayerPos enemy of
    Nothing                     -> False
    Just (Pos2 playerX playerY) ->
        let
            Pos2 x y           = E._pos enemy
            attackExplodeRange = _attackExplodeRange . _bomb . _config $ E._data enemy
            inRangeX           = abs (playerX - x) <= attackExplodeRange
            inRangeY           = abs (playerY - y) <= attackExplodeRange
        in inRangeX && inRangeY

thinkSearchBehaviorInstrs :: Secs -> Int -> Enemy BombEnemyData -> [BombEnemyBehaviorInstr]
thinkSearchBehaviorInstrs searchTtl numTurns enemy
    | numTurns >= minSearchTurns && isFacingPlayer && isNextTurn = [StartSprintInstr]
    | otherwise                                                  = [UpdateSearchInstr searchTtl numTurns]
    where
        minSearchTurns = _minSearchTurns . _bomb . _config $ _data enemy
        isFacingPlayer = isEnemyFacingPlayer enemy
        isNextTurn     = searchTtl - timeStep <= 0.0

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy BombEnemyData -> [BombEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    FallenHurt
        | isExplodeTimerInactive -> [StartExplodeInstr]
    KnockDownHurt
        | isExplodeTimerInactive -> [StartExplodeInstr]
    WallHurt
        | isExplodeTimerInactive -> [StartExplodeInstr]

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed -> [StartExplodeInstr, StartWallSplatInstr]
        | inAir && (sprFinished || (inHangtimeVel && isLaunchUpHurt))  -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                -> [StartIdleInstr]
        | otherwise                                                    -> [UpdateHurtInstr hurtTtl hurtType]

    where
        sprFinished            = enemySpriteFinished enemy
        inAir                  = not $ enemyTouchingGround enemy
        velX                   = vecX $ _vel enemy
        touchingWall           = enemyTouchingWall enemy
        isLaunchUpHurt         = hurtType == LaunchUpHurt
        enemyData              = _data enemy
        isExplodeTimerInactive = not $ isExplodeTimerActive enemyData

        cfg                     = _config enemyData
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg
        minHangtimeSecs         = _minHangtimeSecs cfg

thinkBehaviorInstrs :: Enemy BombEnemyData -> [BombEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    _
        | Just explodeTimerTtl <- _explodeTimerTtl enemyData, explodeTimerTtl <= 0.0 ->
            [SetDeadInstr, CreateExplosionInstr]
        | isHealthZero' && not (isExplodeTimerActive enemyData)                      -> [StartExplodeInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | enemyTouchingGround enemy         -> [StartSprintInstr]

    SearchBehavior searchTtl numTurns -> thinkSearchBehaviorInstrs searchTtl numTurns enemy

    SprintBehavior sprintTtl
        | inAttackExplodeRange enemy || sprintTtl <= 0.0 -> [StartExplodeInstr, StartIdleInstr]
        | isHealthZero'                                  -> [StartIdleInstr]
        | otherwise                                      -> [UpdateSprintInstr sprintTtl]

    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> [StartLaunchedInstr hangtimeTtl]
        | hangtimeTtl > 0.0 && inHangtimeVel    -> [UpdateLaunchedInstr hangtimeTtl]

    WallSplatBehavior wallSplatTtl
        | not (isWallSplatBehavior prevBehavior) -> [StartWallSplatInstr]
        | wallSplatTtl > 0.0                     -> [UpdateWallSplatInstr wallSplatTtl]
        | otherwise                              -> [StartLaunchedInstr 0.0]

    SpawnBehavior
        | sprFinished -> [StartSearchInstr]
        | otherwise   -> [UpdateSpawnInstr]

    _ -> []

    where
        enemyData     = _data enemy
        prevBehavior  = _prevBehavior enemyData
        isHealthZero' = isHealthZero $ E._health enemy
        sprFinished   = enemySpriteFinished enemy
        cfg           = _config enemyData
        inHangtimeVel = enemyInHangtimeVel enemy cfg
