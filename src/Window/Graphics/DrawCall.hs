module Window.Graphics.DrawCall
    ( module Window.Graphics.DrawCall.Types
    , addGraphicsDrawCall
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (readIORef, writeIORef)
import qualified Data.IntMap as IM

import Window.Graphics.Camera
import Window.Graphics.DrawCall.Types
import Window.Graphics.Types
import Window.Graphics.Util

addGraphicsDrawCall :: (GraphicsRead m, MonadIO m) => ZIndex -> DrawCall -> m ()
addGraphicsDrawCall (ZIndex zValue) drawCall = do
    cameraPos    <- getCameraPos
    cameraOffset <- getCameraOffset
    cameraSpace  <- getCameraSpace
    clipRect     <- liftIO . readIORef =<< _clipRectRef <$> getGraphics

    drawCallsRef <- _drawCallsRef <$> getGraphics
    drawCalls    <- liftIO $ readIORef drawCallsRef

    let
        drawCallInternal = DrawCallInternal
            { _draw         = drawCall
            , _cameraPos    = cameraPos
            , _cameraOffset = cameraOffset
            , _cameraSpace  = cameraSpace
            , _clipRect     = clipRect
            }

        drawCalls' = IM.insertWith (++) zValue [drawCallInternal] drawCalls

    liftIO $ writeIORef drawCallsRef drawCalls'
