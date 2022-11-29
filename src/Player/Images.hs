module Player.Images
    ( module Player.Images.Types
    , mkPlayerImages
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Player.Images.Types
import Window.Graphics

mkPlayerImages :: (FileCache m, GraphicsRead m, MonadIO m) => m PlayerImages
mkPlayerImages =
    PlayerImages <$>
    loadPackImg "crosshair.image" <*>
    loadPackImg "gamepad-aim-line.image"
    where loadPackImg = \f -> loadPackImage $ PackResourceFilePath "data/ui/ui.pack" f
