module Async.Data
    ( AsyncData(..)
    , AsyncDataRead(..)
    , writeAsyncData
    ) where

import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import qualified SDL

data AsyncData
    = SdlSurfaceData FilePath SDL.Surface

class Monad m => AsyncDataRead m where
    readAsyncData    :: m (Maybe AsyncData)
    readAsyncDataAll :: m [AsyncData]

writeAsyncData :: TQueue AsyncData -> AsyncData -> IO ()
writeAsyncData asyncDataQueue asyncData = atomically $ writeTQueue asyncDataQueue asyncData
