module Level.Room.Item.InfoSign.Types
    ( InfoSignData(..)
    ) where

import Window.Graphics

data InfoSignData = InfoSignData
    { _touchingPlayer          :: Bool
    , _image                   :: Image
    , _overlayInputDisplayText :: InputDisplayText
    }
