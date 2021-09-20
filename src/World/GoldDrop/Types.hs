module World.GoldDrop.Types
    ( GoldDropSprites(..)
    , GoldDropType(..)
    , GoldDropData(..)
    ) where

import Window.Graphics
import World.Util

data GoldDropType
    = GoldChunkGoldDrop
    | ArenaGoldDrop

data GoldDropSprites = GoldDropSprites
    { _idleA   :: Sprite
    , _idleB   :: Sprite
    , _idleC   :: Sprite
    , _idleD   :: Sprite
    , _idleE   :: Sprite
    , _appearA :: Sprite
    , _appearB :: Sprite
    , _appearC :: Sprite
    , _appearD :: Sprite
    , _appearE :: Sprite
    }

data GoldDropData = GoldDropData
    { _type    :: GoldDropType
    , _value   :: GoldValue
    , _sprite  :: Sprite
    , _sprites :: GoldDropSprites
    }
