module Enemy.All.Turret.AI
    ( thinkAI
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Turret
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Turret.AI.Run
import Enemy.All.Turret.Behavior
import Enemy.All.Turret.Data
import Msg
import Util

thinkAI :: (ConfigsRead m, MonadIO m) => EnemyThinkAI TurretEnemyData m
thinkAI enemy = do
    aiEnabled <- not <$> readSettingsConfig _debug _disableAI

    let
        runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
        behaviorInstrs    = thinkBehaviorInstrs enemy
    behaviorMsgs <- concat <$> traverse runBehaviorInstr' behaviorInstrs

    return $ mkEnemyUpdateDataMsgs enemy ++ behaviorMsgs ++ mkGravityMsgs enemy

mkGravityMsgs :: Enemy TurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkGravityMsgs enemy
    | _behavior enemyData `elem` [SpawnBehavior, DeathBehavior] = []
    | otherwise                                                 =
        let
            gravity    = _gravity $ _config enemyData
            gravityVel = Vel2 0.0 (gravity * timeStep)
        in [mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) (E._msgId enemy) MsgEndOrder]
    where enemyData = E._data enemy

mkEnemyUpdateDataMsgs :: Enemy TurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData        = E._data e
        atkCooldown  = _attackCooldown eData
        behavior     = _behavior $ E._data enemy
        atkCooldown' = case behavior of
            SpawnBehavior -> atkCooldown
            _             -> max 0.0 (atkCooldown - timeStep)
    in e
        { _data = eData
            { _attackCooldown = atkCooldown'
            , _prevBehavior   = behavior
            }
        }

canAttackPlayer :: Enemy TurretEnemyData -> Bool
canAttackPlayer enemy = case enemyKnownPlayerPos enemy of
    Nothing                     -> False
    Just (Pos2 playerX playerY) ->
        let
            enemyData            = E._data enemy
            isAttackableBehavior = _behavior enemyData == IdleBehavior

            offCooldown       = _attackCooldown enemyData <= 0.0
            Pos2 x y          = E._pos enemy
            dir               = E._dir enemy
            facingPlayer
                | playerX > x = dir == RightDir
                | otherwise   = dir == LeftDir

            cfg            = _turret $ _config enemyData
            inAggroRange   = abs (playerX - x) <= _aggroRange cfg
            inAttackRangeY = abs (playerY - y) <= _attackRangeY cfg
        in isAttackableBehavior && offCooldown && facingPlayer && inAggroRange && inAttackRangeY

thinkBehaviorInstrs :: Enemy TurretEnemyData -> [TurretEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]
        | canAttackPlayer enemy                            -> [StartAttackInstr]

    IdleBehavior
        | prevBehavior /= IdleBehavior -> [StartIdleInstr]
        | otherwise                    -> [UpdateIdleInstr]

    AttackBehavior
        | atkFinished -> [StartIdleInstr, SetPostAttackCooldownInstr]

    HurtBehavior hurtTtl
        | sprFinished && hurtTtl <= 0.0 -> [StartIdleInstr]
        | otherwise                     -> [UpdateHurtInstr hurtTtl]

    SpawnBehavior
        | sprFinished -> [StartIdleInstr]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]

    _ -> []

    where
        health       = E._health enemy
        enemyData    = E._data enemy
        prevBehavior = _prevBehavior enemyData
        sprFinished  = enemySpriteFinished enemy
        atkFinished  = maybe True _done (_attack enemy)
