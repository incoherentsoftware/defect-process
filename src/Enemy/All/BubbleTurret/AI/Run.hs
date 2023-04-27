module Enemy.All.BubbleTurret.AI.Run
    ( runBehaviorInstr
    ) where

import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.BubbleTurret
import Constants
import Enemy as E
import Enemy.All.BubbleTurret.AttackDescriptions
import Enemy.All.BubbleTurret.Behavior
import Enemy.All.BubbleTurret.BubbleProjectile
import Enemy.All.BubbleTurret.Data
import Msg
import Util
import Window.Graphics

runBehaviorInstr :: Bool -> BubbleTurretEnemyBehaviorInstr -> Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr          -> startIdleBehavior enemy
            UpdateIdleInstr         -> updateIdleBehavior enemy
            StartAttackInstr        -> startAttackBehavior enemy
            CreateBubbleProjInstr   -> createBubbleProjMessages enemy
            UpdateHurtInstr hurtTtl -> updateHurtBehavior hurtTtl enemy
            UpdateSpawnInstr        -> updateSpawnBehavior enemy
            StartDeathInstr         -> startDeathBehavior enemy
            SetDeadInstr            -> enemySetDeadMessages enemy

        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior (E._data enemy) of
                    IdleBehavior -> []
                    _            -> startIdleBehavior enemy
            in case cmd of
                StartAttackInstr      -> setIdleMsgs
                CreateBubbleProjInstr -> setIdleMsgs
                _                     -> aiEnabledMsgs

mkEnemyUpdateBehaviorMsg :: Enemy BubbleTurretEnemyData -> BubbleTurretEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = behavior}
    }

updateBehaviorIfMatching :: Enemy BubbleTurretEnemyData -> BubbleTurretEnemyBehavior -> BubbleTurretEnemyBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _, HurtBehavior _) -> behavior
    _                                -> existingBehavior
    where existingBehavior = _behavior $ E._data enemy

startDeathBehavior :: Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
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

updateHurtBehavior :: Secs -> Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = updateBehaviorIfMatching e behavior}
    }
    where behavior = HurtBehavior $ hurtTtl - timeStep

startAttackBehavior :: Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior enemy = attackMsg:behaviorDataMsgs
    where
        enemyData        = E._data enemy
        atk1Desc         = _attack1 $ _attackDescs enemyData
        enemyId          = E._msgId enemy
        attackMsg        = mkMsgTo (EnemyMsgSetAttackDesc atk1Desc) enemyId
        behaviorDataMsgs = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (E._data e)
                { _behavior       = AttackBehavior
                , _attackCooldown = _bubbleAttackCooldown . _bubbleTurret $ _config enemyData
                }
            }

attackProjOffset :: Enemy BubbleTurretEnemyData -> Pos2
attackProjOffset enemy = E._pos enemy `vecAdd` Pos2 mouthOffsetX' mouthOffsetY
    where
        Pos2 mouthOffsetX mouthOffsetY = _bubbleAttackMouthOffset . _bubbleTurret . _config $ E._data enemy
        mouthOffsetX'                  = mouthOffsetX * directionNeg (E._dir enemy)

createBubbleProjMessages :: Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
createBubbleProjMessages enemy = [mkMsg $ NewThinkProjectileMsgAddM mkBubbleProj]
    where
        pos          = attackProjOffset enemy
        dir          = E._dir enemy
        mkBubbleProj = mkBubbleProjectile pos dir (enemyTauntedStatus enemy)

startIdleBehavior :: Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = clearAtkMsg:mkEnemyUpdateBehaviorMsg enemy IdleBehavior
    where clearAtkMsg = mkMsgTo EnemyMsgClearAttack (E._msgId enemy)

updateIdleBehavior :: Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData = E._data e
        cfg   = _bubbleTurret $ _config eData
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

updateSpawnBehavior :: Enemy BubbleTurretEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []
