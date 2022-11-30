module Enemy.All.Flail.Data.Types
    ( FlailEnemyData(..)
    ) where

import Configs.All.Enemy
import Enemy.All.Flail.AttackDescriptions.Types
import Enemy.All.Flail.Behavior
import Enemy.All.Flail.Sprites
import Util

data FlailEnemyData = FlailEnemyData
    { _attackCooldownTtl :: Secs
    , _sprites           :: EnemySprites
    , _attackDescs       :: EnemyAttackDescriptions
    , _behavior          :: FlailEnemyBehavior
    , _prevBehavior      :: FlailEnemyBehavior
    , _config            :: EnemyConfig
    }
