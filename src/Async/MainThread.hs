module Async.MainThread
    ( MainThreadAsyncStatus(..)
    , loadMainThreadAsyncData
    ) where

import Control.Concurrent     (threadDelay)
import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable          (traverse_)

import Async.Data
import Async.Request
import Async.Signal
import Window.Graphics.Texture
import Window.Graphics.Types

pendingAsyncReqsSleepMicroseconds = 10000 :: Int

data MainThreadAsyncStatus
    = MainThreadDidNothing
    | MainThreadLoadedAsyncData

loadMainThreadAsyncData
    :: (AsyncDataRead m, AsyncRequestRead m, AsyncSignalRead m, GraphicsRead m, MonadIO m)
    => m MainThreadAsyncStatus
loadMainThreadAsyncData = readAsyncSignal >>= \case
    Nothing -> return MainThreadDidNothing

    Just DoMainThreadLoadsSignal ->
        let
            waitOnPendingAsyncRequests :: (AsyncRequestRead m1, MonadIO m1) => m1 ()
            waitOnPendingAsyncRequests = do
                isPendingReqs <- not <$> isAsyncRequestsEmpty
                if
                    | isPendingReqs -> do
                        liftIO $ threadDelay pendingAsyncReqsSleepMicroseconds
                        waitOnPendingAsyncRequests
                    | otherwise     -> return ()

            processAsyncData :: (GraphicsRead m1, MonadIO m1) => AsyncData -> m1 ()
            processAsyncData = \case
                SdlSurfaceData filePath sdlSurface -> void $ loadTextureEx filePath sdlSurface
        in do
            waitOnPendingAsyncRequests
            traverse_ processAsyncData =<< readAsyncDataAll

            return MainThreadLoadedAsyncData
