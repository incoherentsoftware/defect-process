module Player.Gun.FireDrawState.LegsState.Types
    ( LegsStatus(..)
    , LegsSprites(..)
    , LegsState(..)
    ) where

import Data.Aeson.Types (FromJSON, FromJSONKey(fromJSONKey), defaultJSONKeyOptions, genericFromJSONKey)
import GHC.Generics     (Generic)

import Window.Graphics.Sprite.Types

data LegsStatus
    = LegsStandingGround
    | LegsWalkingGround
    | LegsBackWalkingGround
    | LegsInAir
    deriving (Eq, FromJSON, Generic, Ord)

instance FromJSONKey LegsStatus where
    fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

data LegsSprites = LegsSprites
    { _stand    :: Sprite
    , _air      :: Sprite
    , _walk     :: Sprite
    , _backWalk :: Sprite
    }

data LegsState = LegsState
    { _status  :: LegsStatus
    , _sprite  :: Sprite
    , _sprites :: LegsSprites
    }
