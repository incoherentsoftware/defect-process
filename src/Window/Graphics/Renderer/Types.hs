module Window.Graphics.Renderer.Types
    ( RenderMode(..)
    , Renderer(..)
    ) where

import Data.IORef (IORef)
import qualified SDL

import Window.Graphics.Texture.Types

data RenderMode
    = RenderSD
    | RenderHD

data Renderer = Renderer
    { _sdlRenderer      :: SDL.Renderer
    , _targetTextureRef :: IORef Texture
    , _renderMode       :: RenderMode
    }
