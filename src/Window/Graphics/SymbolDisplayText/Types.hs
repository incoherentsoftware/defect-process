module Window.Graphics.SymbolDisplayText.Types
    ( SymbolDisplayText(..)
    ) where

import qualified Data.Text as T

import Window.Graphics.DisplayText.Types
import Window.Graphics.Image.Types

data SymbolDisplayText = SymbolDisplayText
    { _displayText       :: DisplayText
    , _text              :: T.Text
    , _prefixSymbolImage :: Maybe Image
    }
