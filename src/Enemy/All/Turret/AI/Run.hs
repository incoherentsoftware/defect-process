module Enemy.All.Turret.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (MonadIO)

import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Turret
import Constants
import Enemy as E
import Enemy.All.Turret.AttackDescriptions
import Enemy.All.Turret.AttackProjectile
import Enemy.All.Turret.Behavior
import Enemy.All.Turret.Data
import Msg
import Projectile as P
import Util
import Window.Graphics

runBehaviorInstr
    :: MonadIO m
    => Bool
    -> TurretEnemyBehaviorInstr
    -> Enemy TurretEnemyData
    -> m [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr             -> return $ startIdleBehavior enemy
            UpdateIdleInstr            -> return $ updateIdleBehavior enemy
            StartAttackInstr           -> startAttackBehavior enemy
            SetPostAttackCooldownInstr -> return $ setPostAttackCooldown enemy
            UpdateHurtInstr hurtTtl    -> return $ updateHurtBehavior hurtTtl enemy
            UpdateSpawnInstr           -> return $ updateSpawnBehavior enemy
            StartDeathInstr            -> return $ startDeathBehavior enemy
            SetDeadInstr               -> return $ enemySetDeadMessages enemy

        aiDisabledMsgs =
            let
                setIdleMsgs = return $ case _behavior (E._data enemy) of
                    IdleBehavior -> []
                    _            -> startIdleBehavior enemy

            in case cmd of
                StartAttackInstr -> setIdleMsgs
                UpdateIdleInstr  -> return []
                _                -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy TurretEnemyData -> TurretEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (E._data e) {_behavior = behavior}}

updateBehaviorIfMatching :: Enemy TurretEnemyData -> TurretEnemyBehavior -> TurretEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _, HurtBehavior _) -> behavior
    _                                -> existingBehavior
    where existingBehavior = _behavior $ E._data enemy

startDeathBehavior :: Enemy TurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
startDeathBehavior enemy = deathSoundMsg:updateMsg
    where
        x             = vecX $ E._pos enemy
        centerY       = vecY $ hitboxCenter (enemyHitbox enemy)
        pos           = Pos2 x centerY
        deathSoundMsg = mkMsg $ AudioMsgPlaySound enemyDeathSoundPath pos

        updateMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data   = (E._data e) {_behavior = DeathBehavior}
            , _vel    = zeroVel2
            , _attack = Nothing
            }

attackProjOffset :: Enemy TurretEnemyData -> Pos2
attackProjOffset enemy = E._pos enemy `vecAdd` Pos2 attackMouthOffsetX' attackMouthOffsetY
    where
        Pos2 attackMouthOffsetX attackMouthOffsetY = _attackMouthOffset . _turret . _config $ E._data enemy
        attackMouthOffsetX'                        = attackMouthOffsetX * directionNeg (E._dir enemy)

startAttackBehavior :: MonadIO m => Enemy TurretEnemyData -> m [Msg ThinkEnemyMsgsPhase]
startAttackBehavior enemy =
    let
        enemyData = E._data enemy
        atk1Desc  = _attack1 $ _attackDescs enemyData
        enemyId   = E._msgId enemy
        attackMsg = mkMsgTo (EnemyMsgSetAttackDesc atk1Desc) enemyId
        pos       = attackProjOffset enemy
        dir       = E._dir enemy
    in do
        Some attackProj <- mkTurretAttackProjectile pos dir enemyId enemyData

        let
            attackProjId  = P._msgId attackProj
            attackProjMsg = mkMsg $ NewThinkProjectileMsgAdd (Some attackProj)

            behaviorDataMsgs = mkEnemyUpdateMsg enemy $ \e -> e
                { _data = (E._data e)
                    { _attackProjMsgId = attackProjId
                    , _behavior        = AttackBehavior
                    }
                }

        return $ attackMsg:attackProjMsg:behaviorDataMsgs

setPostAttackCooldown :: Enemy TurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
setPostAttackCooldown enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData              = E._data e
        postAttackCooldown = _postAttackCooldown $ _turret (_config eData)
    in e
        { _data = eData {_attackCooldown = postAttackCooldown}
        }

startIdleBehavior :: Enemy TurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = clearAtkMsg:mkEnemyUpdateBehaviorMsg enemy IdleBehavior
    where clearAtkMsg = mkMsgTo EnemyMsgClearAttack (E._msgId enemy)

updateIdleBehavior :: Enemy TurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData = E._data e
        cfg   = _turret $ _config eData
    in case _turnAroundTimerTtl eData of
        _
            | isEnemyFacingPlayer e -> e
                { _data = eData {_turnAroundTimerTtl = Nothing}
                }

        Just ttl ->
            let ttl' = ttl - timeStep
            in if
                | ttl' <= 0.0 -> e
                    { _data = eData
                        { _attackCooldown     = max (_turnAroundAttackCooldown cfg) (_attackCooldown eData)
                        , _turnAroundTimerTtl = Just ttl'
                        }
                    , _dir  = flipDirection $ E._dir e
                    }
                | otherwise   -> e
                    { _data = eData {_turnAroundTimerTtl = Just ttl'}
                    }

        Nothing -> e
            { _data = eData {_turnAroundTimerTtl = Just $ _turnAroundTimerSecs cfg}
            }

updateSpawnBehavior :: Enemy TurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
        | spriteFinished spr                        -> startIdleBehavior enemy
    _                                               -> []

updateHurtBehavior :: Secs -> Enemy TurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior $ hurtTtl - timeStep
