module Window.Graphics.UiControls.ComboBox.Types
    ( ComboBox(..)
    , ComboBoxStatus(..)
    ) where

import qualified Data.Text as T

import Util
import Window.Graphics.Image
import Window.Graphics.UiControls.Button.Types

data ComboBox = ComboBox
    { _pos           :: Pos2
    , _valueButton   :: Button
    , _valueButtons  :: [Button]
    , _image         :: Image
    , _selectedImage :: Image
    , _selectionText :: T.Text
    , _isSelected    :: Bool
    , _isPressed     :: Bool
    , _isExpanded    :: Bool
    }

data ComboBoxStatus
    = ComboBoxSelectedActiveStatus
    | ComboBoxActiveStatus
    | ComboBoxInactiveStatus
    deriving Eq
