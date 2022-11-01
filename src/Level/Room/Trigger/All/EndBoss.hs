module Level.Room.Trigger.All.EndBoss
    ( mkEndBossTrigger
    ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)

import Audio.Fmod
import Configs
import Configs.All.Level
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy.All.Axe
import FileCache
import Id
import Level.Room.Trigger
import Level.Room.Trigger.Util
import Msg
import Particle.All.Simple
import Util
import World.GoldDrop
import World.ZIndex

bossFightMusicPath          = "data/music/boss-fight.ogg"                      :: FilePath
bossSpawnParticlesSoundPath = "event:/SFX Events/Enemy/Boss/spawn-particles-c" :: FilePath

spawnEffectPath = PackResourceFilePath "data/enemies/boss-enemy-spawn.pack" "spawn-effect.spr" :: PackResourceFilePath

spawnPos    = Pos2 1711.0 3372.0 :: Pos2
spawnDir    = LeftDir            :: Direction
goldDropPos = Pos2 1182.0 3372.0 :: Pos2

mkEndBossTrigger :: (ConfigsRead m, MonadIO m) => m RoomTrigger
mkEndBossTrigger = do
    trigger                <- mkRoomTrigger
    disableRoomBossTrigger <- readSettingsConfig _debug _disableRoomBossTrigger

    return $ (trigger :: RoomTrigger)
        { _think = if
            | disableRoomBossTrigger -> \_ t -> return [removeTriggerMessage t]
            | otherwise              -> thinkMusic
        }

spawnParticlesAudioMsg :: RoomTrigger -> Msg ThinkLevelMsgsPhase
spawnParticlesAudioMsg trigger = mkMsg $ AudioMsgPlaySoundContinuous bossSpawnParticlesSoundPath hashedId spawnPos
    where hashedId = hashId $ _msgId (trigger :: RoomTrigger)

thinkMusic :: (ConfigsRead m, MonadIO m) => RoomTriggerThink m
thinkMusic _ trigger = do
    musicIndex <- getFmodMusic bossFightMusicPath
    void $ playFmodMusicWorld musicIndex

    spawnWaitSecs <- readConfig _level _endBossSpawnWaitSecs
    return
        [ updateTriggerThinkMessage (thinkSpawn spawnWaitSecs) trigger
        , mkMsg $ ParticleMsgAddM (loadSimpleParticle spawnPos spawnDir bossUnderBodyZIndex spawnEffectPath)
        , spawnParticlesAudioMsg trigger
        ]

-- NOTE: this is modified from the full source since only the axe enemy is included in this repo
thinkSpawn :: Monad m => Secs -> RoomTriggerThink m
thinkSpawn waitTtl _ trigger = return $ if
    | waitTtl <= 0.0 ->
        [ mkMsg $ EnemyMsgAddM (mkAxeEnemy spawnPos spawnDir)
        , updateTriggerThinkMessage thinkAlive trigger
        ]
    | otherwise      ->
        let waitTtl' = waitTtl - timeStep
        in
            [ updateTriggerThinkMessage (thinkSpawn waitTtl') trigger
            , spawnParticlesAudioMsg trigger
            ]

thinkAlive :: (ConfigsRead m, MsgsRead ThinkLevelMsgsPhase m) => RoomTriggerThink m
thinkAlive _ trigger =
    let
        checkBossEnemyAlive :: [InfoMsgPayload] -> Bool
        checkBossEnemyAlive []     = False
        checkBossEnemyAlive (p:ps) = case p of
            InfoMsgEnemyPos _ _ -> True
            _                   -> checkBossEnemyAlive ps
    in checkBossEnemyAlive <$> readMsgs >>= \case
        True  -> return []
        False -> do
            warpOutWaitSecs <- readConfig _level _endWarpOutWaitSecs
            return
                [ mkMsg $ NewThinkProjectileMsgAddsM (mkEndBossGoldDrops goldDropPos)
                , mkMsg AudioMsgPlayPostBattleExplorationMusic
                , updateTriggerThinkMessage (thinkWarpOut warpOutWaitSecs) trigger
                ]

thinkWarpOut :: Monad m => Secs -> RoomTriggerThink m
thinkWarpOut waitTtl _ trigger = return $ if
    | waitTtl <= 0.0 -> [mkMsg PlayerMsgWarpOut]
    | otherwise      ->
        let waitTtl' = waitTtl - timeStep
        in [updateTriggerThinkMessage (thinkWarpOut waitTtl') trigger]
