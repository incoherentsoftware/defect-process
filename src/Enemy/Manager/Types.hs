module Enemy.Manager.Types
    ( EnemyManager(..)
    ) where

import Enemy.Types
import Util

data EnemyManager = EnemyManager
    { _enemies :: [Some Enemy]
    }
