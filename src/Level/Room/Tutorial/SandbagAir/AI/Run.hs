module Level.Room.Tutorial.SandbagAir.AI.Run
    ( runBehaviorInstr
    ) where

import Collision.Hitbox
import Configs.All.Enemy
import Configs.All.Enemy.Flying
import Constants
import Enemy as E
import Level.Room.ArenaWalls.Util
import Level.Room.Tutorial.SandbagAir.Behavior
import Level.Room.Tutorial.SandbagAir.Data
import Msg
import Util
import Window.Graphics
import {-# SOURCE #-} Level.Room.Tutorial.SandbagAir

runBehaviorInstr :: SandbagAirBehaviorInstr -> Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
runBehaviorInstr cmd enemy = case cmd of
    StartIdleInstr                         -> startIdleBehavior enemy
    UpdateHurtInstr hurtTtl hurtType       -> updateHurtBehavior hurtTtl hurtType enemy
    StartLaunchedInstr hangtimeTtl         -> startLaunchedBehavior hangtimeTtl enemy
    LaunchedInHangtimeInstr hangtimeTtl    -> launchedInHangtimeBehavior hangtimeTtl enemy
    LaunchedNotInHangtimeInstr hangtimeTtl -> launchedNotInHangtimeBehavior hangtimeTtl enemy
    StartFallenInstr fallenTtl             -> startFallenBehavior fallenTtl enemy
    UpdateFallenInstr fallenTtl            -> updateFallenBehavior fallenTtl enemy
    StartWallSplatInstr                    -> startWallSplatBehavior enemy
    UpdateWallSplatInstr wallSplatTtl      -> updateWallSplatBehavior wallSplatTtl enemy
    StartDematerializeInstr                -> startDematerializeBehavior enemy
    StartRematerializeInstr                -> startRematerializeBehavior enemy
    UpdateSpawnInstr                       -> updateSpawnBehavior enemy
    StartDeathInstr                        -> startDeathBehavior enemy
    SetDeadInstr                           -> setDeadAndRespawnMsgs enemy

mkEnemyUpdateBehaviorMsg :: Enemy SandbagAirData -> SandbagAirBehavior -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateBehaviorMsg enemy behavior = mkEnemyUpdateMsg enemy $ \e ->
    e {_data = (E._data e) {_behavior = behavior}}

updateBehaviorIfMatching :: Enemy SandbagAirData -> SandbagAirBehavior -> SandbagAirBehavior
updateBehaviorIfMatching enemy behavior = case (behavior, existingBehavior) of
    (IdleBehavior, IdleBehavior)                 -> behavior
    (HurtBehavior _ _, HurtBehavior _ _)         -> behavior
    (LaunchedBehavior _ _, LaunchedBehavior _ _) -> behavior
    (FallenBehavior _, FallenBehavior _)         -> behavior
    (WallSplatBehavior _, WallSplatBehavior _)   -> behavior
    _                                            -> existingBehavior
    where existingBehavior = _behavior $ E._data enemy

startIdleBehavior :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
startIdleBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (E._data e) {_behavior = IdleBehavior}
    , _vel  = zeroVel2
    }

updateHurtBehavior :: Secs -> HurtType -> Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
updateHurtBehavior hurtTtl hurtType enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        hurtTtl' = hurtTtl - timeStep
        behavior = updateBehaviorIfMatching e (HurtBehavior hurtTtl' hurtType)
    in e {_data = (E._data e) {_behavior = behavior}}

startLaunchedBehavior :: Secs -> Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
startLaunchedBehavior hangtimeSecs enemy = mkEnemyUpdateBehaviorMsg enemy behavior
    where behavior = LaunchedBehavior hangtimeSecs NotInHangtime

launchedInHangtimeBehavior :: Secs -> Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
launchedInHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        hangtimeTtl' = hangtimeTtl - timeStep
        behavior     = updateBehaviorIfMatching e (LaunchedBehavior hangtimeTtl' InHangtime)
    in e
        { _data = (E._data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

launchedNotInHangtimeBehavior :: Secs -> Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
launchedNotInHangtimeBehavior hangtimeTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let behavior = updateBehaviorIfMatching e (LaunchedBehavior hangtimeTtl NotInHangtime)
    in e {_data = (E._data e) {_behavior = behavior}}

startFallenBehavior :: Secs -> Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
startFallenBehavior fallenTtl enemy = mkEnemyUpdateBehaviorMsg enemy (FallenBehavior fallenTtl)

updateFallenBehavior :: Secs -> Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
updateFallenBehavior fallenTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        fallenTtl' = fallenTtl - timeStep
        behavior   = updateBehaviorIfMatching e (FallenBehavior fallenTtl')
    in e
        { _data = (E._data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

startWallSplatBehavior :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
startWallSplatBehavior enemy = wallImpactMsgs ++ updateEnemyMsg
    where
        enemyCfg        = _config $ E._data enemy
        effectDrawScale = _wallImpactEffectDrawScale $ _flying enemyCfg

        -- need to send arena wall impact msgs directly since sandbag is in tutorial room and not arena walls
        pos            = E._pos enemy
        wallImpactMsgs = roomArenaWallsWallSplatMsgs pos ++ enemyWallImpactMessages effectDrawScale enemy

        updateEnemyMsg = mkEnemyUpdateMsg enemy $ \e -> e
            { _data = (E._data e) {_behavior = WallSplatBehavior $ _minWallSplatSecs enemyCfg}
            , _vel  = zeroVel2
            }

updateWallSplatBehavior :: Secs -> Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
updateWallSplatBehavior wallSplatTtl enemy = mkEnemyUpdateMsg enemy $ \e ->
    let
        wallSplatTtl' = wallSplatTtl - timeStep
        behavior      = updateBehaviorIfMatching e (WallSplatBehavior wallSplatTtl')
    in e
        { _data = (E._data e) {_behavior = behavior}
        , _vel  = zeroVel2
        }

startDematerializeBehavior :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
startDematerializeBehavior enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_behavior = DematerializeBehavior}
    , _vel  = zeroVel2
    }

startRematerializeBehavior :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
startRematerializeBehavior enemy = mkEnemyUpdateMsg enemy $ \e ->
    let eData = E._data e
    in e
        { _data = eData {_behavior = RematerializeBehavior}
        , _pos  = _spawnPos eData
        , _dir  = _spawnDir eData
        , _vel  = zeroVel2
        }

updateSpawnBehavior :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
updateSpawnBehavior enemy = case E._sprite enemy of
    Just spr
        | _frameIndex spr == 0 && _frameChanged spr -> enemySpawnEffectMessages enemy
        | spriteFinished spr                        -> startIdleBehavior enemy
    _                                               -> []

startDeathBehavior :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
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

setDeadAndRespawnMsgs :: Enemy SandbagAirData -> [Msg ThinkEnemyMsgsPhase]
setDeadAndRespawnMsgs enemy = respawnMsg:enemySetDeadMessages enemy
    where
        enemyData = _data enemy
        respawnMsg = mkMsg $ EnemyMsgAddM (mkSandbagAir (_spawnPos enemyData) (_spawnDir enemyData))
