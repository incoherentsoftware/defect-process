module Level.Room.Tutorial.SandbagGround
    ( mkSandbagGround
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Enemy
import FileCache
import Util
import Window.Graphics

mkSandbagGround :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
