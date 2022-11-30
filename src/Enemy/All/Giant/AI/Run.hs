module Enemy.All.Giant.AI.Run
    ( runBehaviorInstr
    ) where

import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)

import Attack
import Collision
import Configs.All.Enemy
import Configs.All.Enemy.Giant
import Constants
import Enemy as E
import Enemy.All.Giant.Behavior
import Enemy.All.Giant.Data
import Msg
import Util
import Window.Graphics

hurtSoundPath = "event:/SFX Events/Enemy/Giant/hurt" :: FilePath

runBehaviorInstr :: Bool -> GiantEnemyBehaviorInstr -> Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr aiEnabled cmd enemy
    | aiEnabled = aiEnabledMsgs
    | otherwise = aiDisabledMsgs
    where
        aiEnabledMsgs = case cmd of
            StartIdleInstr                -> startIdleBehavior enemy
            UpdateIdleInstr idleTtl       -> updateIdleBehavior idleTtl enemy
            StartAdvanceInstr             -> startAdvanceBehavior enemy
            UpdateAdvanceInstr advanceTtl -> updateAdvanceBehavior advanceTtl enemy
            StartRetreatInstr             -> startRetreatBehavior enemy
            UpdateRetreatInstr retreatTtl -> updateRetreatBehavior retreatTtl enemy
            StartAttackInstr atkDesc      -> startAttackBehavior atkDesc enemy
            SetPostAttackCooldownInstr    -> setPostAttackCooldownMessages enemy
            UpdateSpawnInstr              -> updateSpawnBehavior enemy
            StartDeathInstr               -> startDeathBehavior enemy
            SetDeadInstr                  -> enemySetDeadMessages enemy

        aiDisabledMsgs =
            let
                setIdleMsgs = case _behavior (_data enemy) of
                    IdleBehavior _ -> []
                    _              -> startIdleBehavior enemy
            in case cmd of
                StartAdvanceInstr    -> setIdleMsgs
                UpdateAdvanceInstr _ -> setIdleMsgs
                StartRetreatInstr    -> setIdleMsgs
                UpdateRetreatInstr _ -> setIdleMsgs
                StartAttackInstr _   -> setIdleMsgs
                _                    -> aiEnabledMsgs

setPostAttackCooldownMessages :: Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
setPostAttackCooldownMessages enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        eData           = _data e
        postAtkCooldown = _postAttackCooldown $ _giant (_config eData)
    in e
        {_data = eData {_attackCooldown = postAtkCooldown}
        }

mkEnemyUpdateBehaviorMsg :: Enemy GiantEnemyData -> GiantEnemyBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    {_data = (_data e) {_behavior = behavior}
    }

startIdleBehavior :: Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = behavior}
    , _vel   = Vel2 0.0 (vecY $ _vel e)
    }
    where
        idleSecs = _idleSecs . _giant . _config $ _data enemy
        behavior = IdleBehavior idleSecs

updateIdleBehavior :: Secs -> Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateIdleBehavior idleTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = IdleBehavior $ idleTtl - timeStep

startAttackBehavior :: AttackDescription -> Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAttackBehavior atkDesc enemy = behaviorMsg ++ attackMsg
    where
        behaviorMsg  = mkEnemyUpdateBehaviorMsg enemy AttackBehavior
        enemyId      = _msgId enemy
        attackMsg    = [mkMsgTo (EnemyMsgSetAttackDesc atkDesc) enemyId]

startAdvanceBehavior :: Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
startAdvanceBehavior enemy = mkEnemyUpdateMsgM enemy updateAdvance
    where
        cfg            = _giant . _config $ _data enemy
        minAdvanceSecs = _minAdvanceSecs cfg
        maxAdvanceSecs = _maxAdvanceSecs cfg
        advanceSpeed   = _advanceSpeed cfg

        x    = vecX $ E._pos enemy
        dir  = case enemyKnownPlayerPos enemy of
            Nothing               -> E._dir enemy
            Just (Pos2 playerX _) -> if playerX > x then RightDir else LeftDir
        velX = advanceSpeed * directionNeg dir
        velY = vecY $ _vel enemy

        updateAdvance = \e -> do
            advanceSecs <- liftIO $ randomRIO (minAdvanceSecs, maxAdvanceSecs)
            return $ enemy
                { _data = (_data e) {_behavior = AdvanceBehavior advanceSecs}
                , _dir  = dir
                , _vel  = Vel2 velX velY
                }

updateAdvanceBehavior :: Secs -> Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateAdvanceBehavior advanceTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        behavior     = AdvanceBehavior $ advanceTtl - timeStep
        eData        = _data e
        advanceSpeed = _advanceSpeed . _giant $ _config eData
        dir          = enemyFlippedDirIfWallOrGround e
        velX         = advanceSpeed * directionNeg dir
        velY         = vecY $ _vel e
    in e
        { _data = eData {_behavior = behavior}
        , _dir  = dir
        , _vel  = Vel2 velX velY
        }

startRetreatBehavior :: Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
startRetreatBehavior enemy = mkEnemyUpdateMsgM enemy updateRetreat
    where
        cfg            = _giant $ _config (_data enemy)
        retreatSpeed   = _retreatSpeed cfg
        minRetreatSecs = _minRetreatSecs cfg
        maxRetreatSecs = _maxRetreatSecs cfg

        dir                             = E._dir enemy
        dir'
            | isEnemyFacingPlayer enemy = dir
            | otherwise                 = flipDirection dir

        velX = retreatSpeed * directionPos dir'
        velY = vecY $ _vel enemy

        updateRetreat = \e -> do
            retreatSecs <- liftIO $ randomRIO (minRetreatSecs, maxRetreatSecs)
            return $ e
                { _data   = (_data e)
                    { _behavior     = RetreatBehavior retreatSecs
                    , _prevBehavior = RetreatBehavior retreatSecs
                    }
                , _dir    = dir'
                , _vel    = Vel2 velX velY
                , _attack = Nothing
                }

updateRetreatBehavior :: Secs -> Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateRetreatBehavior retreatTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = RetreatBehavior $ retreatTtl - timeStep

startDeathBehavior :: Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
startDeathBehavior enemy = deathSoundMsgs ++ updateMsg
    where
        x                = vecX $ E._pos enemy
        centerY          = vecY $ hitboxCenter (enemyHitbox enemy)
        pos              = Pos2 x centerY
        deathSoundMsgs   =
            [ mkMsg $ AudioMsgPlaySound enemyDeathSoundPath pos
            , mkMsg $ AudioMsgPlaySound hurtSoundPath pos
            ]

        updateMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data   = (_data e) {_behavior = DeathBehavior}
            , _vel    = zeroVel2
            , _attack = Nothing
            }

updateSpawnBehavior :: Enemy GiantEnemyData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
        | spriteFinished spr                        -> startIdleBehavior enemy
    _                                               -> []
