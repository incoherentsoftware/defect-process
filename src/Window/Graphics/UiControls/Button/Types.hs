module Window.Graphics.UiControls.Button.Types
    ( TextButtonArea(..)
    , ButtonType(..)
    , Button(..)
    , ButtonStatus(..)
    ) where

import Util
import Window.Graphics.DisplayText
import Window.Graphics.Image

data TextButtonArea
    = TextButtonArea Float Float
    | TextButtonAltArea Pos2 Float Float

data ButtonType
    = TextButtonType TextButtonArea DisplayText DisplayText
    | ImageButtonType Image Image

data Button = Button
    { _type       :: ButtonType
    , _pos        :: Pos2
    , _isSelected :: Bool
    , _isPressed  :: Bool
    , _height     :: Float
    }

data ButtonStatus
    = ButtonSelectedActiveStatus
    | ButtonActiveStatus
    | ButtonInactiveStatus
    deriving Eq
