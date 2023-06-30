module Enemy.All.Spear.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Spear
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Spear.AI.Run
import Enemy.All.Spear.AttackDescriptions
import Enemy.All.Spear.AttackType
import Enemy.All.Spear.Behavior
import Enemy.All.Spear.Data
import Enemy.All.Spear.Util
import Msg
import Util
import Window.Graphics

projReleaseFrameTagName = FrameTagName "projRelease" :: FrameTagName

thinkAI :: ConfigsRead m => EnemyThinkAI SpearEnemyData m
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

            modify (++ mkEnemyUpdateDataMsgs enemy)

enemyGravityVel :: SpearEnemyData -> Vel2
enemyGravityVel enemyData = Vel2 0.0 (gravity * timeStep)
    where gravity = _gravity $ _config enemyData

mkEnemyUpdateDataMsgs :: Enemy SpearEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData                                      = _data e
        atkCooldownMultiplier                      = attackCooldownMultiplier e
        (throwAtkCooldownTtl, shoveAtkCooldownTtl) =
            case (enemyTauntedPrevStatus e, enemyTauntedStatus e) of
                (EnemyTauntedInactive, EnemyTauntedActive) ->
                    ( _throwAtkCooldownTtl eData * atkCooldownMultiplier
                    , _shoveAtkCooldownTtl eData * atkCooldownMultiplier
                    )
                _                                          ->
                    ( max 0.0 (_throwAtkCooldownTtl eData - timeStep)
                    , max 0.0 (_shoveAtkCooldownTtl eData - timeStep)
                    )
    in e
        { _data = eData
            { _throwAtkCooldownTtl = throwAtkCooldownTtl
            , _shoveAtkCooldownTtl = shoveAtkCooldownTtl
            , _prevBehavior        = prevBehavior
            }
        }
    where prevBehavior = _behavior $ _data enemy

isAttackableBehavior :: Enemy SpearEnemyData -> Bool
isAttackableBehavior enemy = case _behavior (_data enemy) of
    IdleBehavior _    -> True
    WalkBehavior _    -> True
    RetreatBehavior _ -> True
    _                 -> False

canAttackPlayerAtDistanceX :: Distance -> Enemy SpearEnemyData -> Bool
canAttackPlayerAtDistanceX atkDistX enemy = case enemyKnownPlayerPos enemy of
    Just (Pos2 playerX _)
        | isAttackableBehavior enemy ->
            let
                x                 = vecX $ E._pos enemy
                dir               = E._dir enemy
                onGround          = enemyTouchingGround enemy
                facingPlayer
                    | playerX > x = dir == RightDir
                    | otherwise   = dir == LeftDir
                inAtkRange        = abs (playerX - x) <= atkDistX
            in onGround && facingPlayer && inAtkRange

    _ -> False

canAttackShovePlayer :: Enemy SpearEnemyData -> Bool
canAttackShovePlayer enemy = offCooldown && canAttackPlayerAtDistanceX shoveAtkRange enemy
    where
        enemyData     = _data enemy
        offCooldown   = _shoveAtkCooldownTtl enemyData <= 0.0
        shoveAtkRange = _shoveAtkRange $ _spear (_config enemyData)

canAttackThrowPlayer :: Enemy SpearEnemyData -> Bool
canAttackThrowPlayer enemy = offCooldown && canAttackPlayerAtDistanceX throwAtkRange enemy
    where
        enemyData     = _data enemy
        offCooldown   = _throwAtkCooldownTtl enemyData <= 0.0
        throwAtkRange = _throwAtkRange $ _spear (_config enemyData)

thinkAttackBehaviorInstrs :: Enemy SpearEnemyData -> [SpearEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Just atk
        | _done atk ->
            let throwAtkDesc = _throw $ _attackDescs (E._data enemy)
            in if
                | _description atk == throwAtkDesc -> [StartRetreatInstr]
                | otherwise                        -> [StartIdleInstr]

        | projReleaseFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk ->
            [CreateAttackProjInstr, SetThrowAtkCooldownInstr]

    _ -> []

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy SpearEnemyData -> [SpearEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    FallenHurt
        | sprFinished -> [StartFallenInstr hurtTtl']
    KnockDownHurt
        | sprFinished -> [StartFallenInstr hurtTtl']
    WallHurt
        | sprFinished -> [StartLaunchedInstr 0.0]

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed          -> [StartWallSplatInstr]
        | inAir && (sprFinished || (inHangtimeVel && hurtType == LaunchUpHurt)) -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                         -> [StartIdleInstr]
        | otherwise                                                             -> [UpdateHurtInstr hurtTtl hurtType]

    where
        enemyData               = _data enemy
        cfg                     = _config enemyData
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        minHangtimeSecs         = _minHangtimeSecs cfg

        inAir         = not $ enemyTouchingGround enemy
        touchingWall  = enemyTouchingWall enemy
        velX          = vecX $ _vel enemy
        sprFinished   = enemySpriteFinished enemy
        hurtTtl'      = hurtTtl - timeStep
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)

thinkBehaviorInstrs :: Enemy SpearEnemyData -> [SpearEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]
        | canAttackShovePlayer enemy                       -> [StartAttackInstr ShoveAttackType]
        | canAttackThrowPlayer enemy                       -> [StartAttackInstr ThrowAttackType]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | onGround                          -> [StartWalkInstr]

    WalkBehavior walkTtl
        | not (isWalkBehavior prevBehavior) -> [StartWalkInstr]
        | walkTtl > 0.0                     -> [UpdateWalkInstr walkTtl]
        | otherwise                         -> [StartIdleInstr]

    RetreatBehavior retreatTtl
        | not (isRetreatBehavior prevBehavior) -> [StartRetreatInstr]
        | retreatTtl > 0.0                     -> [UpdateRetreatInstr retreatTtl]
        | otherwise                            -> [StartIdleInstr]

    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl
        | not (isLaunchedBehavior prevBehavior) -> [StartLaunchedInstr hangtimeTtl]
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
        enemyData     = _data enemy
        prevBehavior  = _prevBehavior enemyData
        sprFinished   = enemySpriteFinished enemy
        cfg           = _config enemyData
        inHangtimeVel = enemyInHangtimeVel enemy cfg
        onGround      = enemyTouchingGround enemy
