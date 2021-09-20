module Async.Request
    ( AsyncRequest(..)
    , AsyncRequestRead(..)
    , AsyncRequestWrite(..)
    , peekAsyncRequests
    , popAsyncRequests
    ) where

import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TQueue (TQueue, tryPeekTQueue, tryReadTQueue)
import Control.Monad                 (void)

import {-# SOURCE #-} Level.Room.Types

data AsyncRequest
    = PreloadPackFileRequest FilePath
    | PreloadRoomFgBgPackFilesRequest RoomType

class Monad m => AsyncRequestRead m where
    isAsyncRequestsEmpty :: m Bool

class Monad m => AsyncRequestWrite m where
    writeAsyncRequest :: AsyncRequest -> m ()

peekAsyncRequests :: TQueue AsyncRequest -> IO (Maybe AsyncRequest)
peekAsyncRequests asyncRequestQueue = atomically $ tryPeekTQueue asyncRequestQueue

popAsyncRequests :: TQueue AsyncRequest -> IO ()
popAsyncRequests asyncRequestQueue = void . atomically $ tryReadTQueue asyncRequestQueue
