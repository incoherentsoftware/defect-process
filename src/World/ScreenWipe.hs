module World.ScreenWipe
    ( WorldScreenWipeType(..)
    , WorldScreenWipe(..)
    , mkWorldScreenWipeOut
    , mkWorldScreenWipeIn
    , updateWorldScreenWipe
    , drawWorldScreenWipe
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (IORef, newIORef, readIORef, writeIORef)

import Constants
import Util
import Window.Graphics
import World.ZIndex

wipeMultiplier = 8000.0 :: Float

data WorldScreenWipeType
    = WorldScreenWipeOut
    | WorldScreenWipeIn
    deriving Eq

data WorldScreenWipe = WorldScreenWipe
    { _type            :: WorldScreenWipeType
    , _posX            :: PosX
    , _active          :: Bool
    , _prevDrawPosXRef :: IORef PosX
    }

mkWorldScreenWipeInternal :: MonadIO m => WorldScreenWipeType -> m WorldScreenWipe
mkWorldScreenWipeInternal typ = do
    let posX         = virtualRenderWidth
    prevDrawPosXRef <- liftIO $ newIORef posX
    return $ WorldScreenWipe
        { _type            = typ
        , _posX            = posX
        , _active          = True
        , _prevDrawPosXRef = prevDrawPosXRef
        }

mkWorldScreenWipeOut :: MonadIO m => m WorldScreenWipe
mkWorldScreenWipeOut = mkWorldScreenWipeInternal WorldScreenWipeOut

mkWorldScreenWipeIn :: MonadIO m => m WorldScreenWipe
mkWorldScreenWipeIn = mkWorldScreenWipeInternal WorldScreenWipeIn

updateWorldScreenWipe :: MonadIO m => WorldScreenWipe -> m WorldScreenWipe
updateWorldScreenWipe worldScreenWipe = do
    let posX      = _posX worldScreenWipe - timeStep * wipeMultiplier
    prevDrawPosX <- liftIO $ readIORef (_prevDrawPosXRef worldScreenWipe)
    return $ worldScreenWipe
        { _posX   = posX
        , _active = prevDrawPosX > 0.0
        }

drawWorldScreenWipe :: (GraphicsReadWrite m, MonadIO m) => WorldScreenWipe -> m ()
drawWorldScreenWipe worldScreenWipe =
    let
        prevDrawPosXRef = _prevDrawPosXRef worldScreenWipe
        posX            = _posX worldScreenWipe
    in do
        setCameraSpace CameraScreenSpace

        case _type worldScreenWipe of
            WorldScreenWipeOut ->
                let
                    pos   = Pos2 posX 0.0
                    width = virtualRenderWidth - posX
                in drawRect pos width virtualRenderHeight blackColor screenWipeZIndex
            WorldScreenWipeIn  -> when (posX > 0.0) $
                drawRect zeroPos2 posX virtualRenderHeight blackColor screenWipeZIndex

        liftIO $ writeIORef prevDrawPosXRef posX
