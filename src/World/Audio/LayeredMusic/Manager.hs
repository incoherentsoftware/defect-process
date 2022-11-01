module World.Audio.LayeredMusic.Manager
    ( module World.Audio.LayeredMusic.Manager.Types
    , mkLayeredMusicManager
    , updateLayeredMusicManager
    , playPostBattleExplorationMusic
    , setLayeredMusicManagerRoomInfo
    , progressLayeredMusicManagerMusic
    , updateLayeredMusicManagerFromJukebox
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execStateT, lift, modify)

import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Audio
import Level.Room.Item.Jukebox.Types
import Level.Room.Types
import Util
import World.Audio.LayeredMusic
import World.Audio.LayeredMusic.Manager.Types

numMusicLayers = 5 :: Int

mkLayeredMusicManager :: (ConfigsRead m, MonadIO m) => m LayeredMusicManager
mkLayeredMusicManager = do
    cfg              <- readConfig _settings _audio
    battleMusic      <- mkLayeredMusic $ _battleMusic (cfg :: AudioConfig)
    explorationMusic <- mkLayeredMusic $ _explorationMusic (cfg :: AudioConfig)

    return $ LayeredMusicManager
        { _nowPlaying       = NoMusicPlaying
        , _battleMusic      = battleMusic
        , _explorationMusic = explorationMusic
        , _currentRoomType  = EmptyRoomType
        , _numVisitedArenas = 0
        , _numArenas        = 1
        }

playMusicArena :: (ConfigsRead m, MonadIO m) => LayeredMusicManager -> m LayeredMusicManager
playMusicArena layeredMusicManager = case _nowPlaying layeredMusicManager of
    NoMusicPlaying                    -> playBattleMusic'
    BattleMusicPlaying                -> playBattleMusic'
    ExplorationMusicPlaying           -> playBattleMusic'
    JukeboxMusicPlaying               -> playBattleMusic'
    PostBattleExplorationMusicPlaying -> playExplorationMusic layeredMusicManager
    where playBattleMusic' = playBattleMusic layeredMusicManager

playBattleMusic :: (ConfigsRead m, MonadIO m) => LayeredMusicManager -> m LayeredMusicManager
playBattleMusic layeredMusicManager = case _nowPlaying layeredMusicManager of
    BattleMusicPlaying -> return layeredMusicManager
    _                  ->
        let
            shouldProgressMusic =
                let
                    numVisitedArenas = max 0 (_numVisitedArenas layeredMusicManager - 1)
                    divisor          = _numArenas layeredMusicManager `div` numMusicLayers
                in numVisitedArenas > 0 && numVisitedArenas `mod` divisor == 0

            battleMusic               = _battleMusic (layeredMusicManager :: LayeredMusicManager)
            explorationMusic          = _explorationMusic (layeredMusicManager :: LayeredMusicManager)
            (battleMusic', explorationMusic')
                | shouldProgressMusic = (progressLayeredMusic battleMusic, progressLayeredMusic explorationMusic)
                | otherwise           = (battleMusic, explorationMusic)
        in do
            battleMusicInitialVolumePercent <- readSettingsConfig _audio _battleMusicInitialVolumePercent
            fadeInLayeredMusicEx battleMusicInitialVolumePercent battleMusic'
            return $ layeredMusicManager
                { _nowPlaying       = BattleMusicPlaying
                , _battleMusic      = battleMusic'
                , _explorationMusic = explorationMusic'
                }

playExplorationMusic :: MonadIO m => LayeredMusicManager -> m LayeredMusicManager
playExplorationMusic layeredMusicManager = case _nowPlaying layeredMusicManager of
    ExplorationMusicPlaying           -> return layeredMusicManager
    PostBattleExplorationMusicPlaying -> return layeredMusicManager
    NoMusicPlaying                    -> do
        playLayeredMusic explorationMusic
        return $ layeredMusicManager {_nowPlaying = ExplorationMusicPlaying}
    BattleMusicPlaying                -> fadeInExploration
    JukeboxMusicPlaying               -> fadeInExploration
    where
        explorationMusic = _explorationMusic (layeredMusicManager :: LayeredMusicManager)

        fadeInExploration = do
            fadeInLayeredMusic explorationMusic
            return $ layeredMusicManager {_nowPlaying = ExplorationMusicPlaying}

playPostBattleExplorationMusic :: MonadIO m => LayeredMusicManager -> m LayeredMusicManager
playPostBattleExplorationMusic layeredMusicManager = do
    fadeInLayeredMusic $ _explorationMusic (layeredMusicManager :: LayeredMusicManager)
    return $ layeredMusicManager {_nowPlaying = PostBattleExplorationMusicPlaying}

syncLayeredMusicManagerWithConfigs :: (ConfigsRead m, MonadIO m) => LayeredMusicManager -> m LayeredMusicManager
syncLayeredMusicManagerWithConfigs layeredMusicManager
    | _nowPlaying layeredMusicManager == JukeboxMusicPlaying = return layeredMusicManager
    | otherwise                                              = do
        cfg <- readConfig _settings _audio
        let
            cfgBattleMusicType  = _battleMusic (cfg :: AudioConfig)
            cfgExploreMusicType = _explorationMusic (cfg :: AudioConfig)

            battleMusicType  = _type (_battleMusic (layeredMusicManager :: LayeredMusicManager) :: LayeredMusic)
            exploreMusicType = _type (_explorationMusic (layeredMusicManager :: LayeredMusicManager) :: LayeredMusic)

        flip execStateT layeredMusicManager $ do
            when (battleMusicType /= cfgBattleMusicType || exploreMusicType /= cfgExploreMusicType) $
                lift stopFmodMusicWorld

            when (battleMusicType /= cfgBattleMusicType) $ do
                battleMusic <- lift $ mkLayeredMusic cfgBattleMusicType
                modify $ \lmm -> lmm
                    { _nowPlaying  = NoMusicPlaying
                    , _battleMusic = battleMusic
                    }

            when (exploreMusicType /= cfgExploreMusicType) $ do
                exploreMusic <- lift $ mkLayeredMusic cfgExploreMusicType
                modify $ \lmm -> lmm
                    { _nowPlaying       = NoMusicPlaying
                    , _explorationMusic = exploreMusic
                    }

updateLayeredMusicManager :: (ConfigsRead m, MonadIO m) => LayeredMusicManager -> m LayeredMusicManager
updateLayeredMusicManager layeredMusicManager = do
    layeredMusicManager' <- syncLayeredMusicManagerWithConfigs layeredMusicManager

    readSettingsConfig _audio _musicEnabled >>= \case
        False -> return layeredMusicManager'
        True  -> do
            whenM isFmodMusicMenuPlaying $
                pauseFmodMusicMenu True

            let playExplorationMusic' = playExplorationMusic layeredMusicManager'
            case _currentRoomType layeredMusicManager' of
                ArenaRoomType _          -> playMusicArena layeredMusicManager'
                ChallengeRoomType _      -> playExplorationMusic'
                EmptyRoomType            -> playExplorationMusic'
                NextRoomType             -> playExplorationMusic'
                FromTransitionRoomType _ -> playExplorationMusic'
                ToTransitionRoomType _   -> playExplorationMusic'
                SpecialRoomType _        -> case _nowPlaying layeredMusicManager' of
                    JukeboxMusicPlaying -> return layeredMusicManager'
                    _                   -> playExplorationMusic'

setLayeredMusicManagerRoomInfo :: RoomType -> Int -> Int -> LayeredMusicManager -> LayeredMusicManager
setLayeredMusicManagerRoomInfo roomType numVisitedArenas numArenas layeredMusicManager = layeredMusicManager
    { _nowPlaying       = nowPlaying
    , _currentRoomType  = roomType
    , _numVisitedArenas = numVisitedArenas
    , _numArenas        = numArenas
    }
    where
        nowPlaying = case _nowPlaying layeredMusicManager of
            PostBattleExplorationMusicPlaying -> ExplorationMusicPlaying
            np                                -> np

progressLayeredMusicManagerMusic :: MonadIO m => LayeredMusicManager -> m LayeredMusicManager
progressLayeredMusicManagerMusic layeredMusicManager = case _nowPlaying layeredMusicManager of
    NoMusicPlaying                    -> return layeredMusicManager
    JukeboxMusicPlaying               -> return layeredMusicManager
    BattleMusicPlaying                -> progressBattleMusic
    PostBattleExplorationMusicPlaying -> progressExplorationMusic
    ExplorationMusicPlaying           -> progressExplorationMusic
    where
        progressBattleMusic = do
            let battleMusic = progressLayeredMusic $ _battleMusic (layeredMusicManager :: LayeredMusicManager)
            fadeInLayeredMusic battleMusic
            return $ (layeredMusicManager :: LayeredMusicManager) {_battleMusic = battleMusic}

        progressExplorationMusic = do
            let exploreMusic = progressLayeredMusic $ _explorationMusic (layeredMusicManager :: LayeredMusicManager)
            fadeInLayeredMusic exploreMusic
            return $ (layeredMusicManager :: LayeredMusicManager) {_explorationMusic = exploreMusic}

updateLayeredMusicManagerFromJukebox
    :: MonadIO m
    => JukeboxType
    -> LayeredMusicType
    -> LayeredMusicManager
    -> m LayeredMusicManager
updateLayeredMusicManagerFromJukebox jukeboxType layeredMusicType layeredMusicManager = do
    stopFmodMusicWorld

    jukeboxMusic <- mkLayeredMusic layeredMusicType
    fadeInLayeredMusicJukebox jukeboxMusic

    return $ case jukeboxType of
        BattleJukeboxType -> layeredMusicManager
            { _nowPlaying  = JukeboxMusicPlaying
            , _battleMusic = jukeboxMusic
            }

        ExplorationJukeboxType -> layeredMusicManager
            { _nowPlaying       = JukeboxMusicPlaying
            , _explorationMusic = jukeboxMusic
            }
