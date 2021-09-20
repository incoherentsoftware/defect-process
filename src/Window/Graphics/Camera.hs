module Window.Graphics.Camera
    ( cameraTransformPos
    , getCameraPos
    , setCameraPos
    , modifyCameraPos
    , getCameraOffset
    , setCameraOffset
    , getCameraSpace
    , setCameraSpace
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (modifyIORef', readIORef, writeIORef)

import Util
import Window.Graphics.Types
import Window.Graphics.Util

cameraTransformPos :: (GraphicsRead m, MonadIO m) => Pos2 -> m Pos2
cameraTransformPos pos = do
    cameraSpaceRef <- _cameraSpace <$> getGraphics
    liftIO (readIORef cameraSpaceRef) >>= \case
        CameraScreenSpace -> return pos
        CameraWorldSpace  -> do
            cameraPos    <- getCameraPos
            cameraOffset <- getCameraOffset
            return $ pos `vecSub` cameraPos `vecSub` cameraOffset

getCameraPos :: (GraphicsRead m, MonadIO m) => m Pos2
getCameraPos = do
    gfx <- getGraphics
    liftIO $ readIORef (_cameraPos gfx)

setCameraPos :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> m ()
setCameraPos pos = do
    gfx <- getGraphics
    liftIO $ writeIORef (_cameraPos gfx) pos

modifyCameraPos :: (GraphicsReadWrite m, MonadIO m) => (Pos2 -> Pos2) -> m ()
modifyCameraPos f = do
    gfx <- getGraphics
    liftIO $ modifyIORef' (_cameraPos gfx) f

getCameraOffset :: (GraphicsRead m, MonadIO m) => m Pos2
getCameraOffset = do
    gfx <- getGraphics
    liftIO $ readIORef (_cameraOffset gfx)

setCameraOffset :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> m ()
setCameraOffset offset = do
    gfx <- getGraphics
    liftIO $ writeIORef (_cameraOffset gfx) offset

getCameraSpace :: (GraphicsRead m, MonadIO m) => m CameraSpace
getCameraSpace = do
    gfx <- getGraphics
    liftIO $ readIORef (_cameraSpace gfx)

setCameraSpace :: (GraphicsReadWrite m, MonadIO m) => CameraSpace -> m ()
setCameraSpace screenSpace = do
    gfx <- getGraphics
    liftIO $ writeIORef (_cameraSpace gfx) screenSpace
