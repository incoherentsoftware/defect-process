module Window.Graphics.Image
    ( Image(..)
    , mkImage
    , imageWidth
    , imageHeight
    , drawImage
    , drawImageWithOpacity
    , drawImageScaled
    , drawImageRotated
    , drawImageEx
    , drawImageCropped
    , drawImageRect
    , drawImageRectRotated
    , drawImageRectEx
    ) where

import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types        (CInt)

import Util
import Window.Graphics.Image.Types
import Window.Graphics.Opacity
import Window.Graphics.SubRect
import Window.Graphics.Texture
import Window.Graphics.Types
import Window.Graphics.Util

mkImage :: Texture -> SubRect -> Pos2 -> Pos2 -> Image
mkImage texture subRect originPos topLeftOffset = Image
    { _texture       = texture
    , _subRect       = subRect
    , _originPos     = originPos `vecSub` topLeftOffset
    , _topLeftOffset = topLeftOffset
    }

imageWidth :: Image -> Float
imageWidth = fromIntegral . (_width :: SubRect -> CInt) . _subRect

imageHeight :: Image -> Float
imageHeight = fromIntegral . (_height :: SubRect -> CInt) . _subRect

drawImage :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Direction -> ZIndex -> Image -> m ()
drawImage pos dir zIndex img = drawImageEx pos dir zIndex 0.0 FullOpacity NonScaled img

drawImageWithOpacity :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Direction -> ZIndex -> Opacity -> Image -> m ()
drawImageWithOpacity pos dir zIndex opacity img = drawImageEx pos dir zIndex 0.0 opacity NonScaled img

drawImageScaled :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Direction -> ZIndex -> DrawScale -> Image -> m ()
drawImageScaled pos dir zIndex drawScale img = drawImageEx pos dir zIndex 0.0 FullOpacity drawScale img

drawImageRotated :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Direction -> ZIndex -> Radians -> Image -> m ()
drawImageRotated pos dir zIndex angle img = drawImageEx pos dir zIndex angle FullOpacity NonScaled img

drawImageEx
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> Direction
    -> ZIndex
    -> Radians
    -> Opacity
    -> DrawScale
    -> Image
    -> m ()
drawImageEx pos dir zIndex angle opacity drawScale img =
    drawTextureSubRectEx subRect originPos pos width height dir zIndex angle opacity drawScale texture
        where
            texture   = _texture img
            subRect   = _subRect img
            originPos = _originPos img
            width     = imageWidth img
            height    = imageHeight img

drawImageCropped :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Direction -> ZIndex -> Float -> Float -> Image -> m ()
drawImageCropped pos dir zIndex width height img = drawImage pos dir zIndex img'
    where
        subRect  = _subRect img
        width'   = min (round width) (_width (subRect :: SubRect))
        height'  = min (round height) (_height (subRect :: SubRect))
        subRect' = (subRect :: SubRect)
            { _width  = width'
            , _height = height'
            }
        img'     = img {_subRect = subRect'}

drawImageRect :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Float -> Float -> ZIndex -> Image -> m ()
drawImageRect pos width height zIndex img = drawImageRectEx pos width height zIndex 0.0 FullOpacity NonScaled img

drawImageRectRotated
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> Float
    -> Float
    -> ZIndex
    -> Radians
    -> Image
    -> m ()
drawImageRectRotated pos width height zIndex angle img =
    drawImageRectEx pos width height zIndex angle FullOpacity NonScaled img

drawImageRectEx
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> Float
    -> Float
    -> ZIndex
    -> Radians
    -> Opacity
    -> DrawScale
    -> Image
    -> m ()
drawImageRectEx pos width height zIndex angle opacity drawScale img =
    drawTextureSubRectEx subRect originPos pos width height RightDir zIndex angle opacity drawScale texture
        where
            texture   = _texture img
            subRect   = _subRect img
            originPos = _originPos img
