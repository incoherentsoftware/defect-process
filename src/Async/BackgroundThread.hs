module Async.BackgroundThread
    ( forkBackgroundThread
    ) where

import Control.Concurrent            (forkIO, threadDelay)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar   (TVar, readTVarIO)
import Control.Monad                 (forever, void)
import Data.Foldable                 (for_, traverse_)
import Data.IORef                    (IORef, modifyIORef', newIORef, readIORef)
import Data.Text.Encoding            (decodeUtf8)
import System.Directory              (doesFileExist)
import System.FilePath               ((</>), takeDirectory)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified SDL.Image

import AppEnv
import Async.Data
import Async.Request
import FileCache
import Level.Room.Types
import Level.Room.Util
import Util
import Window.Graphics.Texture.Manager
import Window.Graphics.Types

idleSleepMicroseconds = 50000                  :: Int
textureKeyName        = T.pack "texture:"      :: T.Text
packFilePathKeyName   = T.pack "packFilePath:" :: T.Text

data BackgroundThreadData = BackgroundThreadData
    { _textureMgrVar           :: TVar TextureManager
    , _fileCacheData           :: FileCacheData
    , _asyncRequestQueue       :: TQueue AsyncRequest
    , _asyncDataQueue          :: TQueue AsyncData
    , _packFileTexturePathsRef :: IORef (M.Map FilePath (S.Set FilePath))
    }

mkBackgroundThreadData :: AppEnvData -> IO BackgroundThreadData
mkBackgroundThreadData appEnvData = do
    packFileTexturePathsRef <- newIORef M.empty
    return $ BackgroundThreadData
        { _textureMgrVar           = _textureManagerVar $ appEnvDataGraphics appEnvData
        , _fileCacheData           = appEnvDataFileCacheData appEnvData
        , _asyncRequestQueue       = appEnvDataAsyncRequestQueue appEnvData
        , _asyncDataQueue          = appEnvDataAsyncDataQueue appEnvData
        , _packFileTexturePathsRef = packFileTexturePathsRef
        }

preloadPackFileTextures :: BackgroundThreadData -> S.Set FilePath -> IO ()
preloadPackFileTextures bgThreadData texturePaths = do
    textureMgr <- readTVarIO $ _textureMgrVar bgThreadData

    for_ texturePaths $ \texturePath ->
        case getTextureManagerTexture texturePath textureMgr of
            Just _  -> return ()
            Nothing -> do
                asyncData <-
                    SdlSurfaceData <$>
                    pure texturePath <*>
                    (SDL.Image.load =<< translateResourcePath texturePath)
                writeAsyncData (_asyncDataQueue bgThreadData) asyncData

preloadPackFile :: BackgroundThreadData -> FilePath -> IO ()
preloadPackFile bgThreadData packFilePath =
    let packFileTexturePathsRef = _packFileTexturePathsRef bgThreadData
    in M.lookup packFilePath <$> readIORef packFileTexturePathsRef >>= \case
        Just texturePaths -> preloadPackFileTextures bgThreadData texturePaths

        Nothing -> do
            packFileByteStr <- readFileCacheDataPack packFilePath (_fileCacheData bgThreadData)
            let
                packFileLines = T.lines $ decodeUtf8 packFileByteStr
                texturePaths  = S.fromList
                    [T.unpack v | [k, v] <- map T.words packFileLines, k == textureKeyName]

            preloadPackFileTextures bgThreadData texturePaths
            modifyIORef' packFileTexturePathsRef (M.insert packFilePath texturePaths)

preloadRoomFgBgPackFiles :: BackgroundThreadData -> RoomType -> IO ()
preloadRoomFgBgPackFiles bgThreadData roomType =
    let roomFilePath = roomTypeToFilePath roomType
    in whenM (doesFileExist roomFilePath) $ do
        roomFileByteStr <- readFileCacheData roomFilePath (_fileCacheData bgThreadData)

        let
            roomFileLines     = T.lines $ decodeUtf8 roomFileByteStr
            packFileBasePaths = S.fromList
                [T.unpack v | [k, v] <- map T.words roomFileLines, k == packFilePathKeyName]
            roomDirPath       = takeDirectory roomFilePath
            packFilePaths     = S.map (roomDirPath </>) packFileBasePaths

        traverse_ (preloadPackFile bgThreadData) packFilePaths

forkBackgroundThread :: AppEnvData -> IO ()
forkBackgroundThread appEnvData = void . forkIO $ do
    bgThreadData         <- mkBackgroundThreadData appEnvData
    let asyncRequestQueue = _asyncRequestQueue bgThreadData

    forever $
        peekAsyncRequests asyncRequestQueue >>= \case
            Nothing  -> threadDelay idleSleepMicroseconds
            Just req -> do
                case req of
                    PreloadPackFileRequest packFilePath      -> preloadPackFile bgThreadData packFilePath
                    PreloadRoomFgBgPackFilesRequest roomType -> preloadRoomFgBgPackFiles bgThreadData roomType
                popAsyncRequests asyncRequestQueue
