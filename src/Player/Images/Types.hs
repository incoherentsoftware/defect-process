module Player.Images.Types
    ( PlayerImages(..)
    ) where

import Window.Graphics

data PlayerImages = PlayerImages
    { _aimCrosshair   :: Image
    , _gamepadAimLine :: Image
    }
