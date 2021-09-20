module Level.Room.Item.RefreshStation.Types
    ( RefreshStationSprites(..)
    , RefreshStationData(..)
    ) where

import Window.Graphics

data RefreshStationSprites = RefreshStationSprites
    { _meterFill  :: Sprite
    , _meterDrain :: Sprite
    }

data RefreshStationData = RefreshStationData
    { _touchingPlayer          :: Bool
    , _image                   :: Image
    , _overlaySprite           :: Sprite
    , _overlayInputDisplayText :: InputDisplayText
    , _sprites                 :: RefreshStationSprites
    }
