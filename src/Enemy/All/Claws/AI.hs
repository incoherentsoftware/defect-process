module Enemy.All.Claws.AI
    ( thinkAI
    ) where

import Control.Monad       (unless)
import Control.Monad.State (execState, modify)
import Data.Maybe          (isJust)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Claws
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Claws.AI.Run
import Enemy.All.Claws.AttackDescriptions
import Enemy.All.Claws.Behavior
import Enemy.All.Claws.Data
import Msg
import Util
import Window.Graphics

projectileReleaseFrameTag = FrameTagName "projRelease" :: FrameTagName

thinkAI :: ConfigsRead m => EnemyThinkAI ClawsEnemyData m
thinkAI enemy =
    let
        enemyData  = _data enemy
        gravity    = _gravity $ _config enemyData
        gravityVel = Vel2 0.0 (gravity * timeStep)

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

mkEnemyUpdateDataMsg :: Enemy ClawsEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsg enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = _data e
    in e
        { _data = eData
            { _attackCooldown = _attackCooldown eData - timeStep
            , _prevBehavior   = prevBehavior
            }
        }
    where prevBehavior = _behavior $ _data enemy

canAttackPlayerAtDistanceX :: Distance -> Enemy ClawsEnemyData -> Bool
canAttackPlayerAtDistanceX atkDistX enemy
    | not isAttackableBehavior || _attackCooldown enemyData > 0.0 = False
    | otherwise                                                   = case enemyKnownPlayerPos enemy of
        Nothing                     -> False
        Just (Pos2 playerX playerY) ->
            let
                attackRangeY = _attackRangeY . _claws $ _config enemyData

                Pos2 x y          = E._pos enemy
                distPlayerX       = abs $ playerX - x
                distPlayerY       = abs $ playerY - y
                dir               = E._dir enemy
                onGround          = enemyTouchingGround enemy
                facingPlayer
                    | playerX > x = dir == RightDir
                    | otherwise   = dir == LeftDir
                inAtkRange        = distPlayerX <= atkDistX && distPlayerY <= attackRangeY
            in onGround && facingPlayer && inAtkRange
    where
        enemyData            = _data enemy
        isAttackableBehavior = case _behavior enemyData of
            IdleBehavior _    -> True
            AdvanceBehavior _ -> True
            DashBehavior      -> True
            _                 -> False

canAttackSlashPlayer :: Enemy ClawsEnemyData -> Bool
canAttackSlashPlayer enemy = canAttackPlayerAtDistanceX slashRangeX enemy
    where slashRangeX = _slashRangeX . _claws . _config $ _data enemy

canAttackProjectilePlayer :: Enemy ClawsEnemyData -> Bool
canAttackProjectilePlayer enemy
    | willUseAtkProj = canAttackPlayerAtDistanceX projectileRangeX enemy
    | otherwise         = False
    where
        enemyData        = _data enemy
        willUseAtkProj   = _willUseAttackProjectile enemyData
        projectileRangeX = _projectileRangeX . _claws $ _config enemyData

thinkAttackBehaviorInstrs :: Enemy ClawsEnemyData -> [ClawsEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Nothing  -> []
    Just atk ->
        let
            enemyData          = _data enemy
            releaseProjAtkDesc = _releaseProjectile $ _attackDescs enemyData
            isReleaseProjAtk   = _description atk == releaseProjAtkDesc
            isProjReleaseFrame = projectileReleaseFrameTag `isAttackFrameTag` atk
            frameChanged       = attackFrameChanged atk
        in if
            | isReleaseProjAtk && isProjReleaseFrame && frameChanged -> [CreateAttackProjInstr]
            | _done atk                                              ->
                [UpdateWillUseAttackProjInstr, StartRetreatInstr]
            | otherwise                                              -> []

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy ClawsEnemyData -> [ClawsEnemyBehaviorInstr]
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

thinkBehaviorInstrs :: Enemy ClawsEnemyData -> [ClawsEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | canAttackSlashPlayer enemy                       -> [StartAttackInstr slashAtkDesc]
        | canAttackProjectilePlayer enemy                  -> [StartAttackInstr releaseProjAtkDesc]
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | onGround && playerSighted         -> if
            | _willUseDash enemyData -> [StartDashInstr]
            | otherwise              -> [StartAdvanceInstr]

    AdvanceBehavior advanceTtl
        | not (isAdvanceBehavior prevBehavior) -> [StartAdvanceInstr]
        | advanceTtl > 0.0                     -> [UpdateAdvanceInstr advanceTtl]
        | otherwise                            -> [StartIdleInstr]

    RetreatBehavior retreatTtl
        | not (isRetreatBehavior prevBehavior) -> [StartRetreatInstr]
        | retreatTtl > 0.0                     -> [UpdateRetreatInstr retreatTtl]
        | otherwise                            -> [StartIdleInstr]

    DashBehavior -> case _attack enemy of
        Nothing         -> [StartAdvanceInstr]
        Just atk
            | _done atk -> [UpdateWillUseDashInstr, StartAdvanceInstr]
            | otherwise -> []

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
        enemyData     = _data enemy
        prevBehavior  = _prevBehavior enemyData
        sprFinished   = enemySpriteFinished enemy
        cfg           = _config enemyData
        inHangtimeVel = enemyInHangtimeVel enemy cfg
        onGround      = enemyTouchingGround enemy
        playerSighted = isJust $ enemyKnownPlayerPos enemy

        atkDescs           = _attackDescs enemyData
        slashAtkDesc       = _slash atkDescs
        releaseProjAtkDesc = _releaseProjectile atkDescs
