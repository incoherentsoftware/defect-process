module Window.Graphics.Image.Types
    ( Image(..)
    ) where

import Util
import Window.Graphics.SubRect.Types
import Window.Graphics.Texture.Types

data Image = Image
    { _texture       :: Texture
    , _subRect       :: SubRect
    , _originPos     :: Pos2
    , _topLeftOffset :: Pos2
    }
