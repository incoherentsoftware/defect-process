module Window.Types
    ( Window(..)
    ) where

import Window.Graphics.Types
import Window.InputState.Types

data Window = Window
    { _graphics   :: Graphics
    , _inputState :: InputState
    , _closed     :: Bool
    }
