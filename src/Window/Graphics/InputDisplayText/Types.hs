module Window.Graphics.InputDisplayText.Types
    ( InputDisplayText(..)
    ) where

import qualified Data.Text as T

import Util
import Window.Graphics.Color
import Window.Graphics.Fonts
import Window.Graphics.Image.Types
import Window.Graphics.Texture.Types
import Window.InputState.Alias
import Window.InputState.GamepadManager.Types
import Window.InputState.RawData

data InputDisplayText = InputDisplayText
    { _inputAlias        :: Maybe InputAlias
    , _inputRawDatas     :: [InputRawData]
    , _text              :: T.Text
    , _font              :: Font
    , _color             :: Color
    , _mouseKbTexture    :: Texture
    , _gamepadTexture    :: Texture
    , _gamepadImages     :: [(Pos2, Image)]
    , _gamepadType       :: GamepadType
    , _prefixSymbolImage :: Maybe Image
    }
