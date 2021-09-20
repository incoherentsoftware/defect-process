module Window.Graphics.Texture.Types
    ( Texture(..)
    ) where

import qualified SDL

import Id

data Texture = Texture
    { _id         :: Id Texture
    , _sdlTexture :: SDL.Texture
    , _width      :: Int
    , _height     :: Int
    }

instance Eq Texture where
    (==) :: Texture -> Texture -> Bool
    (==) t1 t2 = _id t1 == _id t2

instance Ord Texture where
    (<=) :: Texture -> Texture -> Bool
    (<=) t1 t2 = _id t1 <= _id t2
