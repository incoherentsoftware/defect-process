module Enemy.All.Flying.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Flying
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Flying.AI.Run
import Enemy.All.Flying.AttackDescriptions
import Enemy.All.Flying.Behavior
import Enemy.All.Flying.Data
import Msg
import Util
import Window.Graphics

thinkAI :: ConfigsRead m => EnemyThinkAI FlyingEnemyData m
thinkAI enemy = do
    aiEnabled <- not <$> readSettingsConfig _debug _disableAI

    return . flip execState [] $ do
        modify (++ mkGravityMsg enemy)

        let
            runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
            behaviorInstrs    = thinkBehaviorInstrs enemy
        modify (++ concatMap runBehaviorInstr' behaviorInstrs)

        modify (++ mkEnemyUpdateDataMsg enemy)

mkGravityMsg :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkGravityMsg enemy = case _behavior (E._data enemy) of
    HurtBehavior _ hurtType
        | hurtType `elem` [LaunchUpHurt, FallenHurt, KnockDownHurt] -> gravityMsg
    LaunchedBehavior _ NotInHangtime                                -> gravityMsg
    FallenBehavior _                                                -> gravityMsg
    _                                                               -> []
    where
        gravity    = _gravity $ _config (E._data enemy)
        gravityVel = Vel2 0.0 (gravity * timeStep)
        enemyId    = E._msgId enemy
        gravityMsg = [mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder]

mkEnemyUpdateDataMsg :: Enemy FlyingEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsg enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = E._data e
    in e
        { _data = eData
            { _attackCooldown = max 0.0 (_attackCooldown eData - timeStep)
            , _prevBehavior   = prevBehavior
            }
        }
    where prevBehavior = _behavior $ E._data enemy

isBelowStartPosY :: Enemy FlyingEnemyData -> Bool
isBelowStartPosY enemy = vecY (E._pos enemy) > _startPosY (E._data enemy)

thinkAttackBehaviorInstrs :: Enemy FlyingEnemyData -> [FlyingEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Nothing  -> [StartIdleInstr]
    Just atk ->
        let
            enemyData           = E._data enemy
            atkDescs            = _attackDescs enemyData
            atkShootDesc        = _shoot atkDescs
            isAtkShoot          = _description atk == atkShootDesc
            atkSprite           = attackSprite atk
            cfg                 = _flying $ _config enemyData
            isShootReleaseFrame = _frameIndex atkSprite == _shootReleaseFrameIndex cfg
            frameChanged        = _frameChanged atkSprite
        in if
            | _done atk                                         -> [StartIdleInstr]
            | isAtkShoot && isShootReleaseFrame && frameChanged -> [CreateAttackProjInstr]
            | otherwise                                         -> []

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy FlyingEnemyData -> [FlyingEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    FlyingHurt
        | sprFinished && hurtTtl <= 0.0 -> if
            | isBelowStartPosY enemy -> [StartFlyUpwardsInstr]
            | otherwise              -> [StartIdleInstr]
    FallenHurt
        | sprFinished -> [StartFallenInstr hurtTtl']

    WallHurt
        | sprFinished && hurtTtl <= 0.0 -> [StartLaunchedInstr 0.0]
        | otherwise                     -> [UpdateHurtInstr hurtTtl hurtType]

    -- let enemy hit ground
    KnockDownHurt -> []

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed -> [StartWallSplatInstr]
        | inAir && inHangtimeVel && hurtType == LaunchUpHurt           -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                -> case hurtType of
            FlyingHurt   -> [StartIdleInstr]
            LaunchedHurt -> [StartLaunchedInstr 0.0]
            _            -> [StartLaunchedInstr minHangtimeSecs]
        | otherwise                                                    -> [UpdateHurtInstr hurtTtl hurtType]

    where
        inAir                   = not $ enemyTouchingGround enemy
        touchingWall            = enemyTouchingWall enemy
        velX                    = vecX $ E._vel enemy
        sprFinished             = enemySpriteFinished enemy
        hurtTtl'                = hurtTtl - timeStep
        cfg                     = _config $ E._data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        minHangtimeSecs         = _minHangtimeSecs cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg

canAttack :: Enemy FlyingEnemyData -> Bool
canAttack enemy = isIdleBehavior (_behavior enemyData) && isAttackableFrame && _attackCooldown enemyData <= 0.0
    where
        enemyData            = E._data enemy
        idleAttackFrameIndex = _idleAttackFrameIndex $ _flying (_config enemyData)
        sprFrameIndex        = maybe (-1) _frameIndex (E._sprite enemy)
        isAttackableFrame    = sprFrameIndex `mod` idleAttackFrameIndex == 0

inAttackShockRange :: Enemy FlyingEnemyData -> Bool
inAttackShockRange enemy = case enemyKnownPlayerCenterPos enemy of
    Nothing        -> False
    Just playerPos ->
        let
            enPos      = E._pos enemy
            shockRange = _shockRange . _flying . _config $ E._data enemy
        in vecDist playerPos enPos <= shockRange

inAttackShootRange :: Enemy FlyingEnemyData -> Bool
inAttackShootRange enemy = case enemyKnownPlayerCenterPos enemy of
    Nothing               -> False
    Just (Pos2 playerX _) ->
        let
            x              = vecX $ E._pos enemy
            distPlayerX    = abs $ playerX - x
            dir            = E._dir enemy
            facingPlayer   = dir == if playerX > x then RightDir else LeftDir
            shootDistanceX = _shootDistanceX . _flying . _config $ E._data enemy
        in facingPlayer && distPlayerX <= shootDistanceX

thinkBehaviorInstrs :: Enemy FlyingEnemyData -> [FlyingEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    _
        | canAttack' && inAttackShockRange enemy && allowShockAttack ->
            [FacePlayerInstr, StartAttackInstr shockAtkDesc]
        | canAttack' && inAttackShootRange enemy                     ->
            [FacePlayerInstr, StartAttackInstr shootAtkDesc]

    IdleBehavior idleTtl          -> [FacePlayerInstr, UpdateIdleInstr idleTtl]
    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl _
        | hangtimeTtl > 0.0 && inHangtimeVel -> [LaunchedInHangtimeInstr hangtimeTtl]
        | otherwise                          -> [LaunchedNotInHangtimeInstr hangtimeTtl]

    FallenBehavior fallenTtl
        | fallenTtl > 0.0 -> [UpdateFallenInstr fallenTtl]
        | otherwise       -> [StartGetUpInstr]

    GetUpBehavior
        | prevBehavior /= GetUpBehavior -> [StartGetUpInstr]
        | sprFinished                   -> [StartFlyUpwardsInstr]

    FlyUpwardsBehavior
        | isBelowStartPosY enemy -> [UpdateFlyUpwardsInstr]
        | otherwise              -> [StartIdleInstr]

    WallSplatBehavior wallSplatTtl
        | wallSplatTtl > 0.0 -> [UpdateWallSplatInstr wallSplatTtl]
        | otherwise          -> [StartLaunchedInstr 0.0]

    SpawnBehavior
        | sprFinished -> [StartIdleInstr]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]

    _ -> []

    where
        health        = E._health enemy
        enemyData     = E._data enemy
        prevBehavior  = _prevBehavior enemyData
        sprFinished   = enemySpriteFinished enemy
        cfg           = _config enemyData
        inHangtimeVel = enemyInHangtimeVel enemy cfg

        canAttack'       = canAttack enemy
        allowShockAttack = _prevAttackType enemyData /= ShockAttackType || _shootDistanceX (_flying cfg) <= 0.0

        atkDescs     = _attackDescs enemyData
        shockAtkDesc = _shock atkDescs
        shootAtkDesc = _shoot atkDescs
