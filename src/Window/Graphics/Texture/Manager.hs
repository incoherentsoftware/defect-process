module Window.Graphics.Texture.Manager
    ( TextureManager
    , mkTextureManager
    , getTextureManagerTexture
    , putTextureManagerTexture
    , freeTextureManagerTextures
    , freeTextureManagerAllTextures
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (traverse_)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified SDL

import Window.Graphics.Texture.Manager.Types
import Window.Graphics.Texture.Types

mkTextureManager :: TextureManager
mkTextureManager = TextureManager {_textures = M.empty}

getTextureManagerTexture :: FilePath -> TextureManager -> Maybe Texture
getTextureManagerTexture filePath textureMgr = filePath `M.lookup` textures
    where textures = _textures textureMgr

putTextureManagerTexture :: FilePath -> Texture -> TextureManager -> TextureManager
putTextureManagerTexture filePath texture textureMgr = textureMgr {_textures = textures'}
    where
        textures  = _textures textureMgr
        textures' = M.insert filePath texture textures

freeTextureManagerTextures :: MonadIO m => S.Set Texture -> TextureManager -> m TextureManager
freeTextureManagerTextures textures textureMgr = do
    traverse_ (SDL.destroyTexture . _sdlTexture) textures
    return $ textureMgr
        {_textures = M.filter (`S.notMember` textures) (_textures textureMgr)
        }

freeTextureManagerAllTextures :: MonadIO m => TextureManager -> m TextureManager
freeTextureManagerAllTextures textureMgr = freeTextureManagerTextures allTextures textureMgr
    where allTextures = S.fromList $ M.elems (_textures textureMgr)
