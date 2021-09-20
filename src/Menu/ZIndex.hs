module Menu.ZIndex
    ( menuOverExpandedZIndex
    , menuOverZIndex
    , menuZIndex
    ) where

import Window.Graphics

menuOverExpandedZIndex = ZIndex 1 :: ZIndex
menuOverZIndex         = ZIndex 2 :: ZIndex
menuZIndex             = ZIndex 3 :: ZIndex
