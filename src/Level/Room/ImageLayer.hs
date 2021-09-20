module Level.Room.ImageLayer
    ( module Level.Room.ImageLayer.Types
    , loadRoomImageLayer
    , drawRoomImageLayer
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import System.FilePath        ((</>))
import Text.Printf            (printf)

import Constants
import FileCache
import Level.Room.Bounds
import Level.Room.ImageLayer.JSON
import Level.Room.ImageLayer.Types
import Level.Room.Types
import Util
import Window.Graphics
import World.ZIndex

maxImageLayerZIndexOffset = 20              :: Int
defaultParallax           = Vec2 1.0 1.0    :: Vec2
maxParallaxX              = 1.0             :: VecX
maxParallaxY              = 1.0             :: VecY
rotatedAngle              = toRadians 90    :: Radians
rotatedFlippedAngle       = toRadians (-90) :: Radians

loadRoomImageLayer :: (FileCache m, GraphicsRead m, MonadIO m) => FilePath -> RoomImageLayerJSON -> m RoomImageLayer
loadRoomImageLayer filePathDir json =
    let
        packFilePath    = filePathDir </> _packFilePath json
        imgFileName     = _imageFileName json
        imgResourcePath = PackResourceFilePath packFilePath imgFileName
        pos             = _pos (json :: RoomImageLayerJSON)

        error' :: String -> a
        error' s = error $ imgFileName ++ ": " ++ s

        parallax = case _parallax (json :: RoomImageLayerJSON) of
            Nothing                                      -> defaultParallax
            Just p@(Vec2 pX pY)
                | pX > maxParallaxX || pY > maxParallaxY ->
                    error' $ printf "parallax must be <= [%f, %f]" maxParallaxX maxParallaxY
                | otherwise                              -> p

        zIndex = case _zIndexOffset json of
            Nothing                                       -> levelImageLayerZIndex
            Just zOffset
                | abs zOffset > maxImageLayerZIndexOffset ->
                    let range = (-maxImageLayerZIndexOffset, maxImageLayerZIndexOffset)
                    in error' $ "imageLayers zIndex must be between " ++ show range
                | otherwise                               -> levelImageLayerZIndex - ZIndex zOffset

        rotated = fromMaybe False (_rotated (json :: RoomImageLayerJSON))
        dir     = case _flipped json of
            Just True -> LeftDir
            _         -> RightDir
    in do
        img <- loadPackImage imgResourcePath
        return $ RoomImageLayer
            { _image    = img
            , _pos      = pos
            , _zIndex   = zIndex
            , _parallax = parallax
            , _rotated  = rotated
            , _dir      = dir
            }

drawRoomImageLayer :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Room -> RoomImageLayer -> m ()
drawRoomImageLayer (Pos2 cameraX cameraY) room imgLayer = drawImageRotated pos dir zIndex angle img
    where
        Pos2 x y                 = _pos (imgLayer :: RoomImageLayer)
        Vec2 parallaxX parallaxY = _parallax (imgLayer :: RoomImageLayer)
        bounds                   = _bounds room
        leftBounds               = _leftBounds bounds
        rightBounds              = _rightBounds bounds
        bottomBounds             = _bottomBounds bounds
        centerX                  = leftBounds + (rightBounds - virtualRenderWidth - leftBounds) / 2.0
        xOffset                  = (cameraX - centerX) * (1.0 - parallaxX)
        yOffset                  = (cameraY - bottomBounds + virtualRenderHeight) * (1.0 - parallaxY)

        rotated        = _rotated (imgLayer :: RoomImageLayer)
        flipped        = dir == LeftDir
        rotatedFlipped = rotated && flipped
        img            = _image (imgLayer :: RoomImageLayer)

        xRotateFlipOffset
            | rotatedFlipped = 0.0
            | rotated        = imageHeight img
            | flipped        = imageWidth img
            | otherwise      = 0.0
        yRotateFlipOffset
            | rotatedFlipped = -1.0
            | otherwise      = 0.0

        x'     = x + xOffset + xRotateFlipOffset
        y'     = y + yOffset + yRotateFlipOffset
        pos    = Pos2 x' y'
        dir    = _dir imgLayer
        zIndex = _zIndex imgLayer

        angle
            | dir == LeftDir && rotated = rotatedFlippedAngle
            | rotated                   = rotatedAngle
            | otherwise                 = 0.0
