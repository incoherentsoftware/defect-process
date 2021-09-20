module Game.Types
    ( module Game.Mode
    , Game(..)
    ) where

import Console.Types
import Game.Mode
import Menu.Types
import Util.Time
import World.Types

data Game = Game
    { _mode     :: GameMode
    , _prevMode :: GameMode
    , _world    :: World
    , _menu     :: Menu
    , _time     :: Time
    , _console  :: Console
    , _quit     :: Bool
    }
