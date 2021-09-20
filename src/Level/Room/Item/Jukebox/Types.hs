module Level.Room.Item.Jukebox.Types
    ( JukeboxType(..)
    , JukeboxSprites(..)
    , JukeboxData(..)
    ) where

import Data.Aeson.Types (FromJSON)
import GHC.Generics     (Generic)

import Window.Graphics

data JukeboxType
    = BattleJukeboxType
    | ExplorationJukeboxType
    deriving (Eq, FromJSON, Generic)

data JukeboxSprites = JukeboxSprites
    { _idle   :: Sprite
    , _active :: Sprite
    }

data JukeboxData = JukeboxData
    { _type                        :: JukeboxType
    , _touchingPlayer              :: Bool
    , _sprite                      :: Sprite
    , _switchTrackInputDisplayText :: InputDisplayText
    , _currentTrackDisplayText     :: DisplayText
    , _sprites                     :: JukeboxSprites
    }
