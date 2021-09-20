module Window.Graphics.Texture.Manager.Types
    ( TextureManager(..)
    ) where

import qualified Data.Map as M

import Window.Graphics.Texture.Types

data TextureManager = TextureManager
    { _textures :: M.Map FilePath Texture
    }
