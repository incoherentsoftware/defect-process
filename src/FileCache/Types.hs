module FileCache.Types
    ( PackResourceFilePath(..)
    , FileCacheData(..)
    , FileCache(..)
    ) where

import Control.Concurrent.STM.TVar (TVar)

import Util
import qualified Data.ByteString as BS
import qualified Data.Map as M

data PackResourceFilePath = PackResourceFilePath
    { _filePath :: FilePath
    , _fileName :: FileName
    }
    deriving Show

data FileCacheData = FileCacheData
    { _disabled       :: Bool
    , _byteStringsVar :: TVar (M.Map FilePath BS.ByteString)
    }

class Monad m => FileCache m where
    readFileCachePackResource :: PackResourceFilePath -> m BS.ByteString
    readFileCache             :: FilePath -> m BS.ByteString
