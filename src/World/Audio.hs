module World.Audio
    ( module World.Audio.LayeredMusic
    , module World.Audio.Types
    , mkWorldAudio
    , resetWorldAudio
    , playWorldAudioSound
    , updateWorldAudio
    , setWorldAudioRoomInfo
    ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execStateT, get, lift, put)
import Data.Foldable          (foldlM, traverse_)
import qualified Data.Map as M
import qualified Data.Set as S

import Audio.Fmod
import Configs
import Id
import Level.Room.Item.Jukebox.Types
import Level.Room.Types
import Msg
import Util
import Window.Graphics
import World.Audio.LayeredMusic
import World.Audio.LayeredMusic.Manager
import World.Audio.Types

mkWorldAudio :: (ConfigsRead m, MonadIO m) => m WorldAudio
mkWorldAudio = do
    soundsMap           <- M.fromList <$> getFmodSounds
    layeredMusicManager <- mkLayeredMusicManager

    return $ WorldAudio
        { _soundsMap                = soundsMap
        , _soundUniqueIds           = S.empty
        , _soundContinuousHashedIds = S.empty
        , _layeredMusicManager      = layeredMusicManager
        }

resetWorldAudio :: (ConfigsRead m, MonadIO m) => WorldAudio -> m WorldAudio
resetWorldAudio worldAudio = do
    stopFmodAudioWorld
    pauseFmodAudioWorld False
    layeredMusicManager <- mkLayeredMusicManager

    return $ worldAudio
        { _soundUniqueIds           = S.empty
        , _soundContinuousHashedIds = S.empty
        , _layeredMusicManager      = layeredMusicManager
        }

processMessages
    :: forall m. (ConfigsRead m, MonadIO m, MsgsReadWrite UpdateAudioMsgsPhase m)
    => WorldAudio
    -> m WorldAudio
processMessages worldAudio = foldlM processMsg worldAudio =<< readMsgs
    where
        processMsg :: WorldAudio -> AudioMsgPayload -> m WorldAudio
        processMsg !audio p = case p of
            AudioMsgPlaySound filePath pos ->
                let hashedId = hashId NullId
                in playWorldAudioSound filePath hashedId (Just pos) audio *> pure audio

            AudioMsgPlaySoundCentered filePath ->
                let hashedId = hashId NullId
                in playWorldAudioSound filePath hashedId Nothing audio *> pure audio

            AudioMsgPlaySoundUnique filePath hashedId pos ->
                let uniqueIds = _soundUniqueIds audio
                in if
                    | hashedId `S.notMember` uniqueIds -> do
                        void $ playWorldAudioSound filePath hashedId (Just pos) audio
                        return $ audio {_soundUniqueIds = hashedId `S.insert` uniqueIds}
                    | otherwise                        -> return audio

            AudioMsgPlaySoundContinuous filePath hashedId pos ->
                playWorldAudioSoundContinuous filePath hashedId pos audio *> pure audio

            AudioMsgMuteSound hashedId mute -> muteFmodSound hashedId mute *> pure audio

            AudioMsgRampMusicToNormalVolume -> rampFmodMusicWorldToNormalVolume *> pure audio

            AudioMsgPlayPostBattleExplorationMusic -> do
                layeredMusicMgr <- playPostBattleExplorationMusic $ _layeredMusicManager audio
                return $ audio {_layeredMusicManager = layeredMusicMgr}

            AudioMsgCycleJukeboxMusic jukeboxType -> do
                let layeredMusicMgr = _layeredMusicManager audio
                layeredMusicMgr'   <- case jukeboxType of
                    BattleJukeboxType -> do
                        let battleMusicType = _type (_battleMusic layeredMusicMgr :: LayeredMusic)
                        battleMusicType'   <- cycleLayeredMusicType battleMusicType

                        writeMsgs
                            [ mkMsg $ ConsoleMsgSetBattleMusic battleMusicType'
                            , mkMsg ConsoleMsgSaveSettings
                            ]

                        updateLayeredMusicManagerFromJukebox jukeboxType battleMusicType' layeredMusicMgr

                    ExplorationJukeboxType -> do
                        let exploreMusicType = _type (_explorationMusic layeredMusicMgr :: LayeredMusic)
                        exploreMusicType'   <- cycleLayeredMusicType exploreMusicType

                        writeMsgs
                            [ mkMsg $ ConsoleMsgSetExplorationMusic exploreMusicType'
                            , mkMsg ConsoleMsgSaveSettings
                            ]

                        updateLayeredMusicManagerFromJukebox jukeboxType exploreMusicType' layeredMusicMgr

                return $ audio {_layeredMusicManager = layeredMusicMgr'}

playWorldAudioSound :: MonadIO m => FilePath -> HashedId -> Maybe Pos2 -> WorldAudio -> m ()
playWorldAudioSound filePath hashedId pos worldAudio = case M.lookup filePath (_soundsMap worldAudio) of
    Nothing         -> return ()
    Just soundIndex -> case pos of
        Nothing         -> playFmodSoundWorld soundIndex hashedId
        Just (Pos2 x _) -> playFmodSoundWorldPositional soundIndex hashedId x

playWorldAudioSoundContinuous :: MonadIO m => FilePath -> HashedId -> Pos2 -> WorldAudio -> m ()
playWorldAudioSoundContinuous filePath hashedId pos worldAudio
    | hashedId `S.member` _soundContinuousHashedIds worldAudio = updateFmodSoundWorldPosition hashedId (vecX pos)
    | otherwise                                                =
        playWorldAudioSound filePath hashedId (Just pos) worldAudio

updateSoundContinuousHashedIds :: (MonadIO m, MsgsRead UpdateAudioMsgsPhase m) => WorldAudio -> m WorldAudio
updateSoundContinuousHashedIds worldAudio =
    let
        continuousMsgHashedIds :: AudioMsgPayload -> S.Set HashedId -> S.Set HashedId
        continuousMsgHashedIds t hashedIds = case t of
            AudioMsgPlaySoundContinuous _ hashedId _ -> hashedId `S.insert` hashedIds
            _                                        -> hashedIds
    in do
        let continuousHashedIds = _soundContinuousHashedIds worldAudio
        continuousHashedIds'   <- foldr continuousMsgHashedIds S.empty <$> readMsgs
        let removedHashedIds = continuousHashedIds `S.difference` continuousHashedIds'

        traverse_ fadeOutFmodSound removedHashedIds
        return $ worldAudio {_soundContinuousHashedIds = continuousHashedIds'}

updateWorldAudio
    :: (ConfigsRead m, GraphicsRead m, MonadIO m, MsgsReadWrite UpdateAudioMsgsPhase m)
    => WorldAudio
    -> m WorldAudio
updateWorldAudio worldAudio = do
    setFmodCameraWorldPos =<< getCameraPos

    flip execStateT worldAudio $ do
        get >>= lift . processMessages >>= put
        get >>= lift . updateSoundContinuousHashedIds >>= put
        get >>= \audio -> do
            layeredMusicManager <- lift $ updateLayeredMusicManager (_layeredMusicManager audio)
            put $ audio {_layeredMusicManager = layeredMusicManager}

setWorldAudioRoomInfo :: RoomType -> Int -> Int -> WorldAudio -> WorldAudio
setWorldAudioRoomInfo roomType numVisitedArenas numArenas worldAudio = worldAudio
    { _layeredMusicManager =
        setLayeredMusicManagerRoomInfo roomType numVisitedArenas numArenas (_layeredMusicManager worldAudio)
    }
