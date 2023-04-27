module Enemy.All.Dog.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Dog
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Dog.AI.Run
import Enemy.All.Dog.AttackDescriptions
import Enemy.All.Dog.Behavior
import Enemy.All.Dog.Data
import Msg
import Util

enemyGravityVel :: DogEnemyData -> Vel2
enemyGravityVel enemyData = Vel2 0.0 (gravity * timeStep)
    where gravity = _gravity $ _config enemyData

thinkAI :: ConfigsRead m => EnemyThinkAI DogEnemyData m
thinkAI enemy =
    let
        enemyData     = _data enemy
        gravityVel    = enemyGravityVel enemyData
        enemyId       = _msgId enemy
        behavior      = _behavior enemyData
        inWallSplat   = isWallSplatBehavior behavior || isWallHurtBehavior behavior
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        inHangtime    = inHangtimeVel && case behavior of
            LaunchedBehavior hangtimeTtl
                | hangtimeTtl > 0.0 -> True
            HurtBehavior _ AirHurt  -> True
            _                       -> False
    in do
        aiEnabled <- not <$> readSettingsConfig _debug _disableAI

        return . flip execState [] $ do
            unless (behavior `elem` [SpawnBehavior, DeathBehavior] || inWallSplat || inHangtime) $
                modify (mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder:)

            let
                runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
                behaviorInstrs    = thinkBehaviorInstrs enemy
            modify (++ concatMap runBehaviorInstr' behaviorInstrs)

            modify (++ mkEnemyUpdateDataMsg enemy)

mkEnemyUpdateDataMsg :: Enemy DogEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsg enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = _data e
    in e
        { _data = eData
            { _attackCooldown = max 0.0 (_attackCooldown eData - timeStep)
            , _prevBehavior   = prevBehavior
            }
        }
    where prevBehavior = _behavior $ _data enemy

isAttackableBehavior :: Enemy DogEnemyData -> Bool
isAttackableBehavior enemy = _attackCooldown enemyData <= 0.0 && isValidBehavior
    where
        enemyData       = _data enemy
        isValidBehavior = case _behavior enemyData of
            RunTowardsBehavior _ -> True
            RunFromBehavior _    -> True
            _                    -> False

canAttackPlayerAtDistanceX :: Distance -> Enemy DogEnemyData -> Bool
canAttackPlayerAtDistanceX atkDistX enemy
    | not (isAttackableBehavior enemy) = False
    | otherwise                        = case enemyKnownPlayerPos enemy of
        Nothing               -> False
        Just (Pos2 playerX _) ->
            let
                x               = vecX $ E._pos enemy
                distPlayerX     = abs $ playerX - x
                dir             = E._dir enemy
                onGround        = enemyTouchingGround enemy
                facingPlayer    = if playerX > x then dir == RightDir else dir == LeftDir
                inAtkRange = distPlayerX <= atkDistX
            in onGround && facingPlayer && inAtkRange

canAttackHeadbuttPlayer :: Enemy DogEnemyData -> Bool
canAttackHeadbuttPlayer enemy = canAttackPlayerAtDistanceX headbuttRangeX enemy
    where headbuttRangeX = _headbuttRangeX . _dog . _config $ _data enemy

canAttackShootPlayer :: Enemy DogEnemyData -> Bool
canAttackShootPlayer enemy
    | willUseAtkProj = canAttackPlayerAtDistanceX shootRangeX enemy
    | otherwise      = False
    where
        enemyData      = _data enemy
        willUseAtkProj = _willUseAttackProjectile enemyData
        shootRangeX    = _shootRangeX . _dog $ _config enemyData

thinkAttackBehaviorInstrs :: Enemy DogEnemyData -> [DogEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Nothing  -> [StartRunFromInstr, SetPostAttackCooldownInstr]
    Just atk ->
        let
            atkDescs            = _attackDescs $ _data enemy
            shootChargeAtkDesc  = _shootCharge atkDescs
            shootReleaseAtkDesc = _shootRelease atkDescs
            headbuttAtkDesc     = _headbutt atkDescs
            headbuttLandAtkDesc = _headbuttLand atkDescs

            atkDone        = _done atk
            atkIsLastFrame = attackIsLastFrameIndex atk
            atkDesc        = _description atk
            isAtkHeadbutt  = atkDesc == headbuttAtkDesc
            onGround       = enemyTouchingGround enemy
        in if
            | atkDesc == shootChargeAtkDesc && atkDone    ->
                [CreateAttackProjInstr, StartAttackInstr shootReleaseAtkDesc]
            | isAtkHeadbutt && atkIsLastFrame && onGround -> [StartAttackInstr headbuttLandAtkDesc]
            | not isAtkHeadbutt && atkDone                ->
                [UpdateWillUseAttackProjInstr, StartRunFromInstr, SetPostAttackCooldownInstr]
            | otherwise                                   -> []

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy DogEnemyData -> [DogEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    FallenHurt
        | sprFinished -> [StartFallenInstr hurtTtl']
    KnockDownHurt
        | sprFinished -> [StartFallenInstr hurtTtl']
    WallHurt
        | sprFinished -> [StartLaunchedInstr 0.0]

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed -> [StartWallSplatInstr]
        | inAir && (sprFinished || (inHangtimeVel && isLaunchUpHurt))  -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                -> [StartIdleInstr]
        | otherwise                                                    -> [UpdateHurtInstr hurtTtl hurtType]

    where
        sprFinished    = enemySpriteFinished enemy
        hurtTtl'       = hurtTtl - timeStep
        inAir          = not $ enemyTouchingGround enemy
        velX           = vecX $ _vel enemy
        touchingWall   = enemyTouchingWall enemy
        isLaunchUpHurt = hurtType == LaunchUpHurt

        cfg                     = _config $ _data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg
        minHangtimeSecs         = _minHangtimeSecs cfg

thinkPaceBehaviorInstrs :: PaceStatus -> Enemy DogEnemyData -> [DogEnemyBehaviorInstr]
thinkPaceBehaviorInstrs paceStatus enemy = case paceStatus of
    PaceForwards paceForwardsTtl
        | facingPlayer                           -> [StartRunTowardsInstr]
        | paceForwardsTtl <= 0.0 || isFlippedDir -> [StartPaceTurnAroundInstr]
        | otherwise                              -> [UpdatePaceForwardsInstr paceForwardsTtl]

    PaceTurnAround
        | enemySpriteFinished enemy -> [FlipDirectionInstr, StartPaceForwardsInstr]
        | otherwise                 -> [UpdatePaceTurnAroundInstr]

    where
        dir          = E._dir enemy
        isFlippedDir = enemyFlippedDirIfWallOrGround enemy /= dir
        x            = vecX $ E._pos enemy
        facingPlayer = case enemyKnownPlayerPos enemy of
            Nothing           -> False
            Just (Pos2 playerX _)
                | playerX > x -> dir == RightDir
                | otherwise   -> dir == LeftDir

thinkBehaviorInstrs :: Enemy DogEnemyData -> [DogEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | canAttackHeadbuttPlayer enemy                    -> [StartAttackInstr headbuttAtkDesc]
        | canAttackShootPlayer enemy                       -> [StartAttackInstr shootChargeAtkDesc]
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | onGround                          -> [StartPaceForwardsInstr]

    RunTowardsBehavior runTowardsTtl
        | isFlippedDir        -> [StartPaceTurnAroundInstr]
        | runTowardsTtl > 0.0 -> [UpdateRunTowardsInstr runTowardsTtl]
        | otherwise           -> [StartPaceForwardsInstr]

    RunFromBehavior runFromTtl
        | isFlippedDir     -> [StartPaceTurnAroundInstr]
        | runFromTtl > 0.0 -> [UpdateRunFromInstr runFromTtl]
        | otherwise        -> [StartPaceForwardsInstr]

    PaceBehavior paceStatus       -> thinkPaceBehaviorInstrs paceStatus enemy
    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> []
        | hangtimeTtl > 0.0 && inHangtimeVel    -> [LaunchedHangtimeInstr hangtimeTtl]

    FallenBehavior fallenTtl
        | not (isFallenBehavior prevBehavior) -> [StartFallenInstr fallenTtl]
        | fallenTtl > 0.0                     -> [UpdateFallenInstr fallenTtl]
        | otherwise                           -> [StartGetUpInstr]

    GetUpBehavior
        | prevBehavior /= GetUpBehavior -> [StartGetUpInstr]
        | sprFinished                   -> [StartIdleInstr]

    WallSplatBehavior wallSplatTtl
        | not (isWallSplatBehavior prevBehavior) -> [StartWallSplatInstr]
        | wallSplatTtl > 0.0                     -> [UpdateWallSplatInstr wallSplatTtl]
        | otherwise                              -> [StartLaunchedInstr 0.0]

    SpawnBehavior
        | sprFinished -> [StartIdleInstr]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]

    _ -> []

    where
        health        = E._health enemy
        dir           = E._dir enemy
        isFlippedDir  = enemyFlippedDirIfWallOrGround enemy /= dir
        enemyData     = _data enemy
        prevBehavior  = _prevBehavior enemyData
        sprFinished   = enemySpriteFinished enemy
        cfg           = _config enemyData
        inHangtimeVel = enemyInHangtimeVel enemy cfg
        onGround      = enemyTouchingGround enemy

        atkDescs           = _attackDescs enemyData
        headbuttAtkDesc    = _headbutt atkDescs
        shootChargeAtkDesc = _shootCharge atkDescs
