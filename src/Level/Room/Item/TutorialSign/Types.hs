module Level.Room.Item.TutorialSign.Types
    ( TutorialSignData(..)
    ) where

import Window.Graphics

data TutorialSignData = TutorialSignData
    { _touchingPlayer               :: Bool
    , _image                        :: Image
    , _overlayLine0DisplayText      :: DisplayText
    , _overlayLine1InputDisplayText :: InputDisplayText
    }
