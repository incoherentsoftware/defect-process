module Player.Overlay.All.GrindSparks.Types
    ( GrindSparksOverlayData(..)
    ) where

import Window.Graphics

data GrindSparksOverlayData = GrindSparksOverlayData
    { _sprite     :: Sprite
    , _slowSprite :: Sprite
    }
