module Enemy.All.Zombie.Data.Types
    ( ZombieEnemyData(..)
    ) where

import Configs.All.Enemy
import Enemy.All.Zombie.AttackDescriptions.Types
import Enemy.All.Zombie.Behavior
import Enemy.All.Zombie.Sprites
import Util

data ZombieEnemyData = ZombieEnemyData
    { _attackCooldown            :: Secs
    , _postSpawnNoAttackCooldown :: Secs
    , _sprites                   :: EnemySprites
    , _attackDescs               :: EnemyAttackDescriptions
    , _behavior                  :: ZombieEnemyBehavior
    , _prevBehavior              :: ZombieEnemyBehavior
    , _config                    :: EnemyConfig
    }
