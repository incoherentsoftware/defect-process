module Level.Room.SpringLauncher.Types
    ( SpringLauncherSprites(..)
    , SpringLauncher(..)
    ) where

import Configs.All.Level
import Msg.Types
import Util
import Window.Graphics

data SpringLauncherSprites = SpringLauncherSprites
    { _idle     :: Sprite
    , _activate :: Sprite
    }

data SpringLauncher = SpringLauncher
    { _msgId    :: MsgId
    , _pos      :: Pos2
    , _cooldown :: Secs
    , _sprite   :: Sprite
    , _sprites  :: SpringLauncherSprites
    , _config   :: LevelConfig
    }
