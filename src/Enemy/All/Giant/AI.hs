module Enemy.All.Giant.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify, unless)
import Data.Maybe          (fromMaybe, isJust)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Giant
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Giant.AI.Run
import Enemy.All.Giant.AttackDescriptions
import Enemy.All.Giant.Behavior
import Enemy.All.Giant.Data
import Msg
import Util

thinkAI :: ConfigsRead m => EnemyThinkAI GiantEnemyData m
thinkAI enemy =
    let
        enemyData  = _data enemy
        gravity    = _gravity $ _config enemyData
        gravityVel = Vel2 0.0 (gravity * timeStep)
        enemyId    = _msgId enemy
    in do
        aiEnabled <- not <$> readSettingsConfig _debug _disableAI

        return . flip execState [] $ do
            unless (_behavior enemyData `elem` [SpawnBehavior, DeathBehavior]) $
                modify (mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder:)

            let
                runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
                behaviorInstrs    = thinkBehaviorInstrs enemy
            modify (++ concatMap runBehaviorInstr' behaviorInstrs)

            modify (++ mkEnemyUpdateDataMsg enemy)

mkEnemyUpdateDataMsg :: Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateDataMsg enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = _data e
    in e
        { _data = eData
            { _attackCooldown = max 0.0 (_attackCooldown eData - timeStep)
            , _prevBehavior   = prevBehavior
            }
        }
    where prevBehavior = _behavior $ _data enemy

isAttackableBehavior :: Enemy GiantEnemyData -> Bool
isAttackableBehavior enemy
    | not (enemyTouchingGround enemy) || _attackCooldown enemyData > 0.0 = False
    | otherwise                                                          = case _behavior enemyData of
        IdleBehavior _    -> True
        AdvanceBehavior _ -> True
        RetreatBehavior _ -> True
        _                 -> False
    where enemyData = _data enemy

canAttackPunchPlayer :: Enemy GiantEnemyData -> Bool
canAttackPunchPlayer enemy = case enemyKnownPlayerPos enemy of
    Just (Pos2 playerX playerY)
        | isAttackableBehavior enemy ->
            let
                cfg                 = _giant . _config $ _data enemy
                punchRangeX         = _punchRangeX cfg
                punchMinRangeYAbove = _punchMinRangeYAbove cfg

                Pos2 x y          = E._pos enemy
                dir               = E._dir enemy
                facingPlayer
                    | playerX > x = dir == RightDir
                    | otherwise   = dir == LeftDir
                distPlayerX       = abs $ playerX - x
                inPunchRangeY     = y - playerY >= punchMinRangeYAbove
                inPunchRange      = distPlayerX <= punchRangeX && inPunchRangeY
            in facingPlayer && inPunchRange

    _ -> False

canAttackSmashPlayer :: Enemy GiantEnemyData -> Bool
canAttackSmashPlayer enemy = case enemyKnownPlayerPos enemy of
    Just (Pos2 playerX playerY)
        | isAttackableBehavior enemy ->
            let
                cfg         = _giant $ _config (_data enemy)
                Pos2 x y    = E._pos enemy
                distPlayerX = abs $ playerX - x
                distPlayerY = abs $ playerY - y
            in distPlayerX <= _smashRangeX cfg && distPlayerY <= _smashRangeY cfg

    _ -> False

thinkBehaviorInstrs :: Enemy GiantEnemyData -> [GiantEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | canAttackPunchPlayer enemy                       -> [StartAttackInstr punchAtkDesc]
        | canAttackSmashPlayer enemy                       -> [StartAttackInstr smashAtkDesc]
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

    AttackBehavior
        | atkFinished -> [StartRetreatInstr, SetPostAttackCooldownInstr]

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
        onGround      = enemyTouchingGround enemy
        playerSighted = isJust $ enemyKnownPlayerPos enemy
        atkFinished   = fromMaybe True (_done <$> _attack enemy)

        atkDescs     = _attackDescs (_data enemy)
        punchAtkDesc = _punch atkDescs
        smashAtkDesc = _smash atkDescs
