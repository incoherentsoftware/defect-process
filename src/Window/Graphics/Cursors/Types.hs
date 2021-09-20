module Window.Graphics.Cursors.Types
    ( GraphicsCursors(..)
    ) where

import qualified SDL

data GraphicsCursors = GraphicsCursors
    { _crosshair :: SDL.Cursor
    , _menu      :: SDL.Cursor
    }
