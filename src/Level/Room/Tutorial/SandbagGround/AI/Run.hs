module Level.Room.Tutorial.SandbagGround.AI.Run
    ( runBehaviorInstr
    ) where

import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Axe
import Constants
import Enemy as E
import Level.Room.ArenaWalls.Util
import Level.Room.Tutorial.SandbagGround.Behavior
import Level.Room.Tutorial.SandbagGround.Data
import Msg
import Util
import Window.Graphics
import {-# SOURCE #-} Level.Room.Tutorial.SandbagGround

runBehaviorInstr :: SandbagGroundBehaviorInstr -> Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr cmd enemy = case cmd of
    StartIdleInstr                    -> startIdleBehavior enemy
    UpdateHurtInstr hurtTtl hurtType  -> updateHurtBehavior hurtTtl hurtType enemy
    StartLaunchedInstr hangtimeTtl    -> startLaunchedBehavior hangtimeTtl enemy
    LaunchedHangtimeInstr hangtimeTtl -> launchedHangtimeBehavior hangtimeTtl enemy
    StartFallenInstr fallenTtl        -> startFallenBehavior fallenTtl enemy
    UpdateFallenInstr fallenTtl       -> updateFallenBehavior fallenTtl enemy
    StartDematerializeInstr           -> startDematerializeBehavior enemy
    StartRematerializeInstr           -> startRematerializeBehavior enemy
    StartWallSplatInstr               -> startWallSplatBehavior enemy
    UpdateWallSplatInstr wallSplatTtl -> updateWallSplatBehavior wallSplatTtl enemy
    UpdateSpawnInstr                  -> updateSpawnBehavior enemy
    StartDeathInstr                   -> startDeathBehavior enemy
    SetDeadInstr                      -> setDeadAndRespawnMsgs enemy

mkEnemyUpdateBehaviorMsg :: Enemy SandbagGroundData -> SandbagGroundBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = behavior}
    }

updateBehaviorIfMatching :: Enemy SandbagGroundData -> SandbagGroundBehavior -> SandbagGroundBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (HurtBehavior _ _, HurtBehavior _ _)     -> behavior
    (LaunchedBehavior _, LaunchedBehavior _) -> behavior
    _                                        -> existingBehavior
    where existingBehavior = _behavior $ _data enemy

startLaunchedBehavior :: Secs -> Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeTtl enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeTtl

launchedHangtimeBehavior :: Secs -> Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
launchedHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e behavior}
    , _vel  = zeroVel2
    }
    where behavior = LaunchedBehavior $ hangtimeTtl - timeStep

startWallSplatBehavior :: Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = wallImpactMsgs ++ updateEnemyMsg
    where
        enemyCfg        = _config $ _data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _axe enemyCfg

        -- need to send arena wall impact msgs directly since sandbag is in tutorial room and not arena walls
        pos            = E._pos enemy
        wallImpactMsgs = roomArenaWallsWallSplatMsgs pos ++ enemyWallImpactMessages effectDrawScale enemy

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (_data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = WallSplatBehavior $ wallSplatTtl - timeStep}
    , _vel  = zeroVel2
    }

updateHurtBehavior :: Secs -> HurtType -> Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)}
    }
    where hurtTtl' = hurtTtl - timeStep

startIdleBehavior :: Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy updateBehavior
    where
        updateBehavior = \e -> e
            { _data   = (_data e) {_behavior = IdleBehavior}
            , _vel    = Vel2 0.0 (vecY $ _vel e)
            }

startFallenBehavior :: Secs -> Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = FallenBehavior $ fallenTtl - timeStep}
    , _vel  = zeroVel2
    }

startDematerializeBehavior :: Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
startDematerializeBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = DematerializeBehavior}
    , _vel  = zeroVel2
    }

startRematerializeBehavior :: Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
startRematerializeBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = _data e
    in e
        { _data = eData {_behavior = RematerializeBehavior}
        , _pos  = _spawnPos eData
        , _dir  = _spawnDir eData
        , _vel  = zeroVel2
        }

updateSpawnBehavior :: Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
    _                                               -> []

startDeathBehavior :: Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
startDeathBehavior enemy = deathSoundMsg:updateMsg
    where
        x             = vecX $ E._pos enemy
        centerY       = vecY $ hitboxCenter (enemyHitbox enemy)
        pos           = Pos2 x centerY
        deathSoundMsg = mkMsg $ AudioMsgPlaySound enemyDeathSoundPath pos

        updateMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data   = (_data e) {_behavior = DeathBehavior}
            , _vel    = zeroVel2
            , _attack = Nothing
            }

setDeadAndRespawnMsgs :: Enemy SandbagGroundData -> [Msg ThinkEnemyMsgsPhase]
setDeadAndRespawnMsgs enemy = respawnMsg:enemySetDeadMessages enemy
    where
        enemyData = _data enemy
        respawnMsg = mkMsg $ EnemyMsgAddM (mkSandbagGround (_spawnPos enemyData) (_spawnDir enemyData))
