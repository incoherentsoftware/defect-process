module FileCache
    ( module FileCache.Types
    , mkFileCacheData
    , readFileCacheDataPack
    , readFileCacheDataPackResource
    , readFileCacheData
    ) where

import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVarIO)
import Control.Monad               (unless, void)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.STM           (atomically)
import Data.Aeson.Types            (FromJSON, Value)
import Data.Yaml                   (decodeEither', encode)
import System.FilePath             ((</>))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Data.Text as T

import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import FileCache.Types
import Util

newtype PackJSON = PackJSON
    { _files :: HM.HashMap T.Text Value
    }
    deriving newtype FromJSON

data CacheMissBehavior
    = OnCacheMissLoad
    | OnCacheMissError

mkFileCacheData :: MonadIO m => Configs -> m FileCacheData
mkFileCacheData cfgs = do
    byteStringsVar <- liftIO (newTVarIO M.empty)
    return $ FileCacheData
        { _disabled       = _disableFileCache $ _debug (_settings cfgs)
        , _byteStringsVar = byteStringsVar
        }

readFileCacheDataPack :: MonadIO m => FilePath -> FileCacheData -> m (BS.ByteString)
readFileCacheDataPack packFilePath fileCacheData = liftIO $ do
    packByteStr <- BS.readFile =<< translateResourcePath packFilePath
    let
        packJSON     = case decodeEither' packByteStr of
            Left e     -> error $ packFilePath ++ ": " ++ show e
            Right json -> json
        packByteStrs = M.fromList
            [ (packFilePath </> T.unpack k, encode v)
            | (k, v) <- HM.toList (_files packJSON)
            ]

    atomically $ modifyTVar' (_byteStringsVar fileCacheData) (packByteStrs `M.union`)
    return packByteStr

readFileCacheDataPackResource :: forall m. MonadIO m => PackResourceFilePath -> FileCacheData -> m BS.ByteString
readFileCacheDataPackResource packResourceFilePath fileCacheData = readPackResource OnCacheMissLoad
    where
        readPackResource :: CacheMissBehavior -> m BS.ByteString
        readPackResource cacheMissBehavior =
            let
                packFilePath     = _filePath packResourceFilePath
                resourceFileName = _fileName packResourceFilePath
            in do
                byteStrings <- liftIO . readTVarIO $ _byteStringsVar fileCacheData
                case (packFilePath </> resourceFileName) `M.lookup` byteStrings of
                    Just cachedResource -> return cachedResource
                    Nothing             -> case cacheMissBehavior of
                        OnCacheMissLoad  -> do
                            void $ readFileCacheDataPack packFilePath fileCacheData
                            readPackResource OnCacheMissError
                        OnCacheMissError -> error $ packFilePath ++ ": missing " ++ resourceFileName

readFileCacheData :: MonadIO m => FilePath -> FileCacheData -> m BS.ByteString
readFileCacheData filePath fileCacheData = liftIO $
    let byteStringsVar = _byteStringsVar fileCacheData
    in M.lookup filePath <$> readTVarIO byteStringsVar >>= \case
        Just cachedData -> return cachedData
        Nothing         -> do
            fileByteStr <- BS.readFile =<< translateResourcePath filePath
            unless (_disabled fileCacheData) $
                atomically $ modifyTVar' byteStringsVar (M.insert filePath fileByteStr)

            return fileByteStr
