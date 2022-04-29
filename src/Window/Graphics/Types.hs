module Window.Graphics.Types
    ( DrawCall
    , Graphics(..)
    , GraphicsRead(..)
    , GraphicsReadWrite
    ) where

import Control.Concurrent.STM.TVar (TVar)
import Data.IORef                  (IORef)
import qualified Data.IntMap as IM
import qualified SDL
import qualified SDL.Raw

import Util
import Window.Graphics.BlendMode
import Window.Graphics.Cursors.Types
import Window.Graphics.DrawCall.Types
import Window.Graphics.Fonts.Types
import Window.Graphics.Renderer.Types
import Window.Graphics.Texture.Manager.Types
import Window.Graphics.Util

data Graphics = Graphics
    { _window            :: SDL.Window
    , _renderer          :: Renderer
    , _fonts             :: GraphicsFonts
    , _cursors           :: GraphicsCursors
    , _drawCallsRef      :: IORef (IM.IntMap [DrawCallInternal])
    , _cameraPos         :: IORef Pos2
    , _cameraOffset      :: IORef Pos2
    , _cameraSpace       :: IORef CameraSpace
    , _lerpRef           :: IORef Lerp
    , _clipRectRef       :: IORef (Maybe SDL.Raw.Rect)
    , _blendModeRef      :: IORef BlendMode
    , _textureManagerVar :: TVar TextureManager
    }

class Monad m => GraphicsRead m where
    getGraphics :: m Graphics

class GraphicsRead m => GraphicsReadWrite m
