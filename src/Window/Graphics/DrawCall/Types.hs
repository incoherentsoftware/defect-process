module Window.Graphics.DrawCall.Types
    ( DrawCall
    , DrawCallInternal(..)
    ) where

import qualified SDL.Raw

import Util
import Window.Graphics.Util

type DrawCall = IO ()

data DrawCallInternal = DrawCallInternal
    { _draw         :: DrawCall
    , _cameraPos    :: Pos2
    , _cameraOffset :: Pos2
    , _cameraSpace  :: CameraSpace
    , _clipRect     :: Maybe SDL.Raw.Rect
    }
