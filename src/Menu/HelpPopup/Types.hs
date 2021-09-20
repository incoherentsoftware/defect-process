module Menu.HelpPopup.Types
    ( HelpPopupTextOverlayDraw
    , HelpPopupTextOverlay(..)
    , HelpPopupTextOverlayDescription(..)
    , HelpPopupImageAltOverlay(..)
    , HelpPopupScreen(..)
    , HelpPopupTab(..)
    , HelpPopupSelection(..)
    , HelpPopup(..)
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified Data.Vector as V

import Menu.SoundIndices.Types
import Util
import Window.Graphics
import Window.Graphics.UiControls.Button
import Window.InputState

type HelpPopupTextOverlayDraw m = HelpPopupTextOverlay -> m ()

data HelpPopupTextOverlay = HelpPopupTextOverlay
    { _inputDisplayText :: InputDisplayText
    , _draw             :: forall m. (GraphicsReadWrite m, MonadIO m, InputRead m) => HelpPopupTextOverlayDraw m
    }

data HelpPopupTextOverlayDescription = HelpPopupTextOverlayDescription
    { _text :: T.Text
    , _draw :: forall m. (GraphicsReadWrite m, MonadIO m, InputRead m) => HelpPopupTextOverlayDraw m
    }

data HelpPopupImageAltOverlay = HelpPopupImageAltOverlay
    { _overlayActive     :: Bool
    , _popupImageOverlay :: Image
    , _showOverlayButton :: Button
    , _hideOverlayButton :: Button
    }

data HelpPopupScreen = HelpPopupScreen
    { _backgroundImage :: Image
    , _image           :: Image
    , _textOverlays    :: [HelpPopupTextOverlay]
    }

data HelpPopupTab = HelpPopupTab
    { _index           :: Int
    , _image           :: Image
    , _tabButtons      :: V.Vector Button
    , _imageAltOverlay :: Maybe HelpPopupImageAltOverlay
    , _textOverlays    :: [HelpPopupTextOverlay]
    }

data HelpPopupSelection
    = HelpPopupCloseSelection
    | HelpPopupAltControlsSelection
    | HelpPopupTabSelection PosX

data HelpPopup = HelpPopup
    { _screenTabs   :: Either HelpPopupScreen [HelpPopupTab]
    , _closeButton  :: Button
    , _selection    :: HelpPopupSelection
    , _soundIndices :: MenuSoundIndices
    }
