module Level.Room.Item.EventActivator.Types
    ( EventActivatorData(..)
    ) where

import Level.Room.Event.Types
import Window.Graphics

data EventActivatorData = EventActivatorData
    { _eventType        :: RoomEventType
    , _touchingPlayer   :: Bool
    , _image            :: Image
    , _displayText      :: DisplayText
    , _inputDisplayText :: InputDisplayText
    }
