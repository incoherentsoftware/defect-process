module Window.Graphics.DisplayText.Types
    ( DisplayText(..)
    ) where

import Data.IORef (IORef)
import qualified Data.Text as T

import Window.Graphics.Color
import Window.Graphics.Fonts
import Window.Graphics.Texture.Types

data DisplayText = DisplayText
    { _text           :: T.Text
    , _font           :: Font
    , _color          :: Color
    , _textureRef     :: IORef Texture
    , _textureTextRef :: IORef T.Text
    }
