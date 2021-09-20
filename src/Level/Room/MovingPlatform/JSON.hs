module Level.Room.MovingPlatform.JSON
    ( MovingPlatformJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data MovingPlatformJSON = MovingPlatformJSON
    { _width       :: Float
    , _height      :: Float
    , _leftBounds  :: PosX
    , _rightBounds :: PosX
    , _posY        :: PosY
    , _speed       :: Speed
    }
    deriving Generic

instance FromJSON MovingPlatformJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
