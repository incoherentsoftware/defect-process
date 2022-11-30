module Enemy.All.BubbleTurret.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.BubbleTurret
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.BubbleTurret.AI.Run
import Enemy.All.BubbleTurret.Behavior
import Enemy.All.BubbleTurret.Data
import Msg
import Util
import Window.Graphics

summonBubbleFrameTagName = FrameTagName "summonBubble" :: FrameTagName

thinkAI :: ConfigsRead m => EnemyThinkAI BubbleTurretEnemyData m
thinkAI enemy = do
    aiEnabled <- not <$> readSettingsConfig _debug _disableAI

    return . flip execState [] $ do
        let enemyData  = E._data enemy
        unless (_behavior enemyData `elem` [SpawnBehavior, DeathBehavior]) $
            let
                gravity    = _gravity $ _config enemyData
                gravityVel = Vel2 0.0 (gravity * timeStep)
                gravityMsg = mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) (E._msgId enemy) MsgEndOrder
            in modify (gravityMsg:)

        let
            runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
            behaviorInstrs    = thinkBehaviorInstrs enemy
        modify (++ concatMap runBehaviorInstr' behaviorInstrs)

        modify (++ mkEnemyUpdateDataMsgs enemy)

mkEnemyUpdateDataMsgs :: Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsgs enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData        = E._data e
        atkCooldown  = _attackCooldown eData
        atkCooldown' = max 0.0 (atkCooldown - timeStep)
        prevBehavior = _behavior $ E._data enemy
    in e
        { _data = eData
            { _attackCooldown = atkCooldown'
            , _prevBehavior   = prevBehavior
            }
        }

canAttackPlayer :: Enemy BubbleTurretEnemyData -> Bool
canAttackPlayer enemy = case enemyKnownPlayerPos enemy of
    Nothing               -> False
    Just (Pos2 playerX _) ->
        let
            enemyData            = E._data enemy
            isAttackableBehavior = case _behavior enemyData of
                IdleBehavior -> True
                _            -> False

            offCooldown       = _attackCooldown enemyData <= 0.0
            x                 = vecX $ E._pos enemy
            dir               = E._dir enemy
            facingPlayer
                | playerX > x = dir == RightDir
                | otherwise   = dir == LeftDir
            aggroRange        = _aggroRange . _bubbleTurret $ _config enemyData
            inAggroRange      = abs (playerX - x) <= aggroRange
        in isAttackableBehavior && offCooldown && facingPlayer && inAggroRange

thinkAttackBehaviorInstrs :: Enemy BubbleTurretEnemyData -> [BubbleTurretEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy = case _attack enemy of
    Just atk
        | _done atk                                                                 -> [StartIdleInstr]
        | attackFrameChanged atk && summonBubbleFrameTagName `isAttackFrameTag` atk -> [CreateBubbleProjInstr]
    _                                                                               -> []

thinkBehaviorInstrs :: Enemy BubbleTurretEnemyData -> [BubbleTurretEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]
        | canAttackPlayer enemy                            -> [StartAttackInstr]

    IdleBehavior
        | prevBehavior /= IdleBehavior -> [StartIdleInstr]
        | otherwise                    -> [UpdateIdleInstr]

    AttackBehavior -> thinkAttackBehaviorInstrs enemy

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
        enemyData    = _data enemy
        prevBehavior = _prevBehavior enemyData
        sprFinished  = enemySpriteFinished enemy
