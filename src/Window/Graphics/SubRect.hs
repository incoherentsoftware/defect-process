module Window.Graphics.SubRect
    ( module Window.Graphics.SubRect.Types
    , mkSubRect
    ) where

import Window.Graphics.SubRect.Types

mkSubRect :: Int -> Int -> Int -> Int -> SubRect
mkSubRect x y width height = SubRect
    { _x      = fromIntegral x
    , _y      = fromIntegral y
    , _width  = fromIntegral width
    , _height = fromIntegral height
    }
