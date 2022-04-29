module Enemy.Timers
    ( EnemyTimers(..)
    , mkEnemyTimers
    , updateEnemyTimers
    ) where

import Constants
import Util

data EnemyTimers = EnemyTimers
    { _stasisTtl :: Secs
    }

mkEnemyTimers :: EnemyTimers
mkEnemyTimers = EnemyTimers
    { _stasisTtl = 0.0
    }

updateEnemyTimers :: EnemyTimers -> EnemyTimers
updateEnemyTimers enemyTimers = enemyTimers {_stasisTtl = stasisTtl}
    where stasisTtl = max 0.0 (_stasisTtl enemyTimers - timeStep)
