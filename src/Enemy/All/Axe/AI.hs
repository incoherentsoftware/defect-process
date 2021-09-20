module Enemy.All.Axe.AI
    ( thinkAI
    ) where

import Control.Monad       (unless)
import Control.Monad.State (execState, modify)
import Data.Maybe          (isJust)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Axe
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Axe.AI.Run
import Enemy.All.Axe.AttackDescriptions
import Enemy.All.Axe.Behavior
import Enemy.All.Axe.Data
import Msg
import Util

thinkAI :: ConfigsRead m => EnemyUpdateAI AxeEnemyData m
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

mkEnemyUpdateDataMsg :: Enemy AxeEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsg enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = _data e
    in e
        { _data = eData
            { _attackCooldown = _attackCooldown eData - timeStep
            , _prevBehavior   = prevBehavior
            }
        }
    where prevBehavior = _behavior $ _data enemy

canAttackPlayerAtDistance :: Distance -> Enemy AxeEnemyData -> Bool
canAttackPlayerAtDistance atkDistanceX enemy
    | not isAttackableBehavior = False
    | otherwise                = case enemyKnownPlayerPos enemy of
        Nothing                     -> False
        Just (Pos2 playerX playerY) ->
            let
                atkDistanceY  = _attackDistanceY . _axe $ _config enemyData
                Pos2 x y      = E._pos enemy
                distPlayerX   = abs $ playerX - x
                distPlayerY   = abs $ playerY - y
                dir           = E._dir enemy
                onGround      = enemyTouchingGround enemy
                facingPlayer  = if playerX > x then dir == RightDir else dir == LeftDir
                inAtkDistance = distPlayerX <= atkDistanceX && distPlayerY <= atkDistanceY
            in onGround && facingPlayer && inAtkDistance
    where
        enemyData            = _data enemy
        isAttackableBehavior = case _behavior enemyData of
            IdleBehavior _    -> True
            AdvanceBehavior _ -> True
            _                 -> False

canAttackSlashPlayer :: Enemy AxeEnemyData -> Bool
canAttackSlashPlayer enemy
    | _attackCooldown enemyData > 0.0 || willUseLungeChance >= 1.0 = False
    | otherwise                                                    = canAttackPlayerAtDistance strikingDistanceX enemy
    where
        enemyData          = _data enemy
        axeCfg             = _axe $ _config enemyData
        strikingDistanceX  = _slashDistanceX axeCfg
        willUseLungeChance = _willUseLungeChance axeCfg

canAttackLungePlayer :: Enemy AxeEnemyData -> Bool
canAttackLungePlayer enemy
    | _attackCooldown enemyData > 0.0 || not willUseAttackLunge = False
    | otherwise                                                 = canAttackPlayerAtDistance lungeDistanceX enemy
    where
        enemyData          = _data enemy
        willUseAttackLunge = _willUseAttackLunge enemyData
        lungeDistanceX     = _lungeDistanceX . _axe $ _config enemyData

thinkAttackBehaviorInstrs :: Enemy AxeEnemyData -> [AxeEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Just atk
        | _description atk == lungeAtkDesc -> if
            | _done atk && enemyTouchingGround enemy -> [StartAttackInstr lungeLandAtkDesc]
            | otherwise                              -> []
        | _done atk                        -> [UpdateWillUseAttackLungeInstr, StartRetreatInstr]
    _                                      -> []
    where
        atkDescs         = _attackDescs $ _data enemy
        lungeAtkDesc     = _attackLunge atkDescs
        lungeLandAtkDesc = _attackLungeLand atkDescs

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy AxeEnemyData -> [AxeEnemyBehaviorInstr]
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

thinkBehaviorInstrs :: Enemy AxeEnemyData -> [AxeEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | canAttackSlashPlayer enemy                       -> [StartAttackInstr slashAtkDesc]
        | canAttackLungePlayer enemy                       -> [StartAttackInstr lungeAtkDesc]
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior idleTtl
        | not (isIdleBehavior prevBehavior) -> [StartIdleInstr]
        | idleTtl > 0.0                     -> [UpdateIdleInstr idleTtl]
        | onGround && playerSighted         -> [StartAdvanceInstr]

    AdvanceBehavior advanceTtl
        | not (isAdvanceBehavior prevBehavior) -> [StartAdvanceInstr]
        | advanceTtl > 0.0                     -> [UpdateAdvanceInstr advanceTtl]
        | otherwise                            -> [StartIdleInstr]

    RetreatBehavior retreatTtl
        | not (isRetreatBehavior prevBehavior) -> [StartRetreatInstr]
        | retreatTtl > 0.0                     -> [UpdateRetreatInstr retreatTtl]
        | otherwise                            -> [StartIdleInstr]

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

        atkDescs     = _attackDescs (_data enemy)
        slashAtkDesc = _attackSlash atkDescs
        lungeAtkDesc = _attackLunge atkDescs
