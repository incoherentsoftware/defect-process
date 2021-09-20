{-# LANGUAGE ForeignFunctionInterface #-}
module Audio.Fmod
    ( module Audio.Fmod.Types
    , initFmod
    , freeFmod
    , updateFmod
    , reloadFmodSounds
    , getFmodSoundIndex
    , getFmodSounds
    , getFmodMusic
    , getFmodMusics
    , playFmodSound
    , playFmodSoundWorld
    , playFmodSoundWorldPositional
    , fadeOutFmodSound
    , muteFmodSound
    , updateFmodSoundWorldPosition
    , playOrResumeFmodMusicMenu
    , playFmodMusicWorld
    , fadeInFmodMusicWorld
    , fadeInFmodMusicWorldJukebox
    , muteFmodMusic
    , setFmodSoundVolume
    , setFmodMusicVolume
    , setFmodCameraWorldPos
    , rampFmodMusicWorldToNormalVolume
    , pauseFmodMusicMenu
    , pauseFmodAudioWorld
    , stopFmodAudioWorld
    , stopFmodMusicWorld
    , isFmodMusicMenuPlaying
    , isFmodMusicWorldPlaying
    ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable          (traverse_)
import Data.Functor           ((<&>))
import Data.Traversable       (for)
import Foreign.C.String       (CString, peekCString, withCString)
import Foreign.C.Types        (CFloat(CFloat), CInt(CInt))
import System.FilePath        (normalise)

import Audio.Fmod.Types
import Audio.Util
import Audio.Volume
import Configs
import Configs.All.Settings
import Configs.All.Settings.Audio
import Id
import Util

foreign import ccall "initFmod" c_initFmod :: CString -> IO CInt
foreign import ccall "freeFmod" c_freeFmod :: IO CInt
foreign import ccall "updateFmod" c_updateFmod :: IO CInt
foreign import ccall "loadSounds" c_loadSounds :: IO CInt
foreign import ccall "reloadLoadedSounds" c_reloadLoadedSounds :: IO CInt
foreign import ccall "getLoadedSoundsCount" c_getLoadedSoundsCount :: IO CInt
foreign import ccall "getLoadedSoundIndex" c_getLoadedSoundIndex :: CString -> IO CInt
foreign import ccall "getLoadedSoundFilePath" c_getLoadedSoundFilePath :: CInt -> IO CString
foreign import ccall "loadMusicFilePath" c_loadMusicFilePath :: CString -> IO CInt
foreign import ccall "getMusicFilePathsCount" c_getMusicFilePathsCount :: IO CInt
foreign import ccall "getMusic" c_getMusic :: CString -> IO CInt
foreign import ccall "getMusicFilePath" c_getMusicFilePath :: CInt -> IO CString
foreign import ccall "playSound" c_playSound :: CInt -> IO CInt
foreign import ccall "playSoundWorld" c_playSoundWorld :: CInt -> CInt -> IO CInt
foreign import ccall "playSoundWorldPositional" c_playSoundWorldPositional :: CInt -> CInt -> CFloat -> IO CInt
foreign import ccall "fadeOutSound" c_fadeOutSound :: CInt -> IO CInt
foreign import ccall "muteSound" c_muteSound :: CInt -> CInt -> IO CInt
foreign import ccall "updateSoundWorldPosition" c_updateSoundWorldPosition :: CInt -> CFloat -> IO CInt
foreign import ccall "playOrResumeMusicMenu" c_playOrResumeMusicMenu :: CInt -> IO CInt
foreign import ccall "playMusicWorld" c_playMusicWorld :: CInt -> IO CInt
foreign import ccall "fadeInMusicWorld" c_fadeInMusicWorld :: CInt -> CFloat -> IO CInt
foreign import ccall "fadeInMusicWorldJukebox" c_fadeInMusicWorldJukebox :: CInt -> CFloat -> IO CInt
foreign import ccall "muteMusic" c_muteMusic :: CInt -> IO CInt
foreign import ccall "setSoundVolume" c_setSoundVolume :: CFloat -> IO CInt
foreign import ccall "setMusicVolume" c_setMusicVolume :: CFloat -> IO CInt
foreign import ccall "setCameraWorldPosX" c_setCameraWorldPosX :: CFloat -> IO ()
foreign import ccall "rampMusicWorldToNormalVolume" c_rampMusicWorldToNormalVolume :: IO CInt
foreign import ccall "pauseMusicMenu" c_pauseMusicMenu :: CInt -> IO CInt
foreign import ccall "pauseAudioWorld" c_pauseAudioWorld :: CInt -> IO CInt
foreign import ccall "stopAudioWorld" c_stopAudioWorld :: IO CInt
foreign import ccall "stopMusicWorld" c_stopMusicWorld :: IO CInt
foreign import ccall "isMusicMenuPlaying" c_isMusicMenuPlaying :: IO CInt
foreign import ccall "isMusicWorldPlaying" c_isMusicWorldPlaying :: IO CInt

initFmod :: (ConfigsRead m, MonadIO m) => m ()
initFmod = do
    let loadMusicFilePath = \f -> void $ withCString f c_loadMusicFilePath

    liftIO $ do
        resourceDir <- getResourceDirectory
        void $ withCString resourceDir c_initFmod
        void c_loadSounds
        traverse_ loadMusicFilePath =<< loadMusicDirectoryFilePaths

    cfg <- readConfig _settings _audio
    setFmodSoundVolume $ _soundVolume cfg
    setFmodMusicVolume $ _musicVolume cfg

freeFmod :: MonadIO m => m ()
freeFmod = void $ liftIO c_freeFmod

updateFmod :: MonadIO m => m ()
updateFmod = void $ liftIO c_updateFmod

reloadFmodSounds :: MonadIO m => m ()
reloadFmodSounds = void $ liftIO c_reloadLoadedSounds

getFmodSoundIndex :: MonadIO m => FilePath -> m FmodSoundIndex
getFmodSoundIndex filePath = do
    filePath' <- translateFmodStudioPath filePath
    liftIO $ mkFmodSoundIndex . fromIntegral <$> withCString filePath' c_getLoadedSoundIndex

getFmodSounds :: MonadIO m => m [(FilePath, FmodSoundIndex)]
getFmodSounds = liftIO $ do
    n <- c_getLoadedSoundsCount
    for [0..n - 1] $ \i -> do
        filePath      <- peekCString =<< c_getLoadedSoundFilePath i
        let soundIndex = mkFmodSoundIndex (fromIntegral i)
        filePath'     <- translateFmodStudioPath filePath
        return (filePath', soundIndex)

getFmodMusic :: MonadIO m => FilePath -> m FmodMusicIndex
getFmodMusic filePath = liftIO $ mkFmodMusicIndex . fromIntegral <$> withCString filePath' c_getMusic
    where filePath' = normalise filePath

getFmodMusics :: MonadIO m => m [(FilePath, FmodMusicIndex)]
getFmodMusics = liftIO $ do
    n <- c_getMusicFilePathsCount
    for [0..n - 1] $ \i -> do
        filePath      <- peekCString =<< c_getMusicFilePath i
        let musicIndex = mkFmodMusicIndex (fromIntegral i)
        return (filePath, musicIndex)

playFmodSound :: MonadIO m => FmodSoundIndex -> m ()
playFmodSound FmodSoundIndexInvalid       = return ()
playFmodSound (FmodSoundIndex soundIndex) = void . liftIO . c_playSound $ fromIntegral soundIndex

playFmodSoundWorld :: MonadIO m => FmodSoundIndex -> HashedId -> m ()
playFmodSoundWorld FmodSoundIndexInvalid _              = return ()
playFmodSoundWorld (FmodSoundIndex soundIndex) hashedId = void $ liftIO (c_playSoundWorld soundIndex' hashedId')
    where
        soundIndex' = fromIntegral soundIndex
        hashedId'   = fromIntegral $ _int hashedId

playFmodSoundWorldPositional :: MonadIO m => FmodSoundIndex -> HashedId -> VecX -> m ()
playFmodSoundWorldPositional FmodSoundIndexInvalid _ _                    = return ()
playFmodSoundWorldPositional (FmodSoundIndex soundIndex) hashedId posVecX =
    let
        soundIndex' = fromIntegral soundIndex
        hashedId'   = fromIntegral $ _int hashedId
        posVecX'    = realToFrac posVecX
    in void $ liftIO (c_playSoundWorldPositional soundIndex' hashedId' posVecX')

fadeOutFmodSound :: MonadIO m => HashedId -> m ()
fadeOutFmodSound hashedId = void $ liftIO (c_fadeOutSound hashedId')
    where hashedId' = fromIntegral $ _int hashedId

muteFmodSound :: MonadIO m => HashedId -> Bool -> m ()
muteFmodSound hashedId mute = void $ liftIO (c_muteSound hashedId' mute')
    where
        hashedId' = fromIntegral $ _int hashedId
        mute'     = fromIntegral $ if mute then 1 else 0

updateFmodSoundWorldPosition :: MonadIO m => HashedId -> VecX -> m ()
updateFmodSoundWorldPosition hashedId posVecX = void $ liftIO (c_updateSoundWorldPosition hashedId' posVecX')
    where
        hashedId' = fromIntegral $ _int hashedId
        posVecX'  = realToFrac posVecX

playOrResumeFmodMusicMenu :: MonadIO m => FmodMusicIndex -> m ()
playOrResumeFmodMusicMenu FmodMusicIndexInvalid       = return ()
playOrResumeFmodMusicMenu (FmodMusicIndex musicIndex) =
    void . liftIO $ c_playOrResumeMusicMenu (fromIntegral musicIndex)

playFmodMusicWorld :: MonadIO m => FmodMusicIndex -> m ()
playFmodMusicWorld FmodMusicIndexInvalid       = return ()
playFmodMusicWorld (FmodMusicIndex musicIndex) = void . liftIO $ c_playMusicWorld (fromIntegral musicIndex)

fadeInFmodMusicWorld :: MonadIO m => FmodMusicIndex -> Float -> m ()
fadeInFmodMusicWorld FmodMusicIndexInvalid _                      = return ()
fadeInFmodMusicWorld (FmodMusicIndex musicIndex) volumeMultiplier =
    void . liftIO $ c_fadeInMusicWorld (fromIntegral musicIndex) (realToFrac volumeMultiplier)

fadeInFmodMusicWorldJukebox :: MonadIO m => FmodMusicIndex -> Pos2 -> m ()
fadeInFmodMusicWorldJukebox FmodMusicIndexInvalid _                = return ()
fadeInFmodMusicWorldJukebox (FmodMusicIndex musicIndex) jukeboxPos =
    void . liftIO $ c_fadeInMusicWorldJukebox (fromIntegral musicIndex) jukeboxX
    where jukeboxX = realToFrac $ vecX jukeboxPos

muteFmodMusic :: MonadIO m => Bool -> m ()
muteFmodMusic mute = void $ liftIO (c_muteMusic mute')
    where mute' = fromIntegral $ if mute then 1 else 0

setFmodSoundVolume :: MonadIO m => Volume -> m ()
setFmodSoundVolume volume = void $ liftIO (c_setSoundVolume vol)
    where vol = realToFrac $ volumeToFloat volume

setFmodMusicVolume :: MonadIO m => Volume -> m ()
setFmodMusicVolume volume = void $ liftIO (c_setMusicVolume vol)
    where vol = realToFrac $ volumeToFloat volume

setFmodCameraWorldPos :: MonadIO m => Pos2 -> m ()
setFmodCameraWorldPos cameraPos = liftIO (c_setCameraWorldPosX cameraX)
    where cameraX = realToFrac $ vecX cameraPos

rampFmodMusicWorldToNormalVolume :: MonadIO m => m ()
rampFmodMusicWorldToNormalVolume = void $ liftIO c_rampMusicWorldToNormalVolume

pauseFmodMusicMenu :: MonadIO m => Bool -> m ()
pauseFmodMusicMenu paused = void $ liftIO (c_pauseMusicMenu paused')
    where paused' = fromIntegral $ if paused then 1 else 0

pauseFmodAudioWorld :: MonadIO m => Bool -> m ()
pauseFmodAudioWorld paused = void $ liftIO (c_pauseAudioWorld paused')
    where paused' = fromIntegral $ if paused then 1 else 0

stopFmodAudioWorld :: MonadIO m => m ()
stopFmodAudioWorld = void $ liftIO c_stopAudioWorld

stopFmodMusicWorld :: MonadIO m => m ()
stopFmodMusicWorld = void $ liftIO c_stopMusicWorld

isFmodMusicMenuPlaying :: MonadIO m => m Bool
isFmodMusicMenuPlaying = liftIO c_isMusicMenuPlaying <&> \case
    0 -> False
    _ -> True

isFmodMusicWorldPlaying :: MonadIO m => m Bool
isFmodMusicWorldPlaying = liftIO c_isMusicWorldPlaying <&> \case
    0 -> False
    _ -> True
