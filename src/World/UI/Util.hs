module World.UI.Util
    ( loadUiPackImage
    , loadUiPackSprite
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Util
import Window.Graphics

uiPackPath :: FileName -> PackResourceFilePath
uiPackPath f = PackResourceFilePath "data/ui/ui.pack" f

loadUiPackImage :: (FileCache m, GraphicsRead m, MonadIO m) => FileName -> m Image
loadUiPackImage f = loadPackImage $ uiPackPath f

loadUiPackSprite :: (FileCache m, GraphicsRead m, MonadIO m) => FileName -> m Sprite
loadUiPackSprite f = loadPackSprite $ uiPackPath f
