module Enemy.All.Zombie.Data
    ( module Enemy.All.Zombie.Data.Types
    , mkZombieEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Zombie
import Enemy.All.Zombie.AttackDescriptions
import Enemy.All.Zombie.Behavior
import Enemy.All.Zombie.Data.Types
import Enemy.All.Zombie.Sprites
import FileCache
import Window.Graphics

mkZombieEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m ZombieEnemyData
mkZombieEnemyData = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    enemyCfg    <- _enemy <$> readConfigs
    let cfg      = _zombie enemyCfg

    return $ ZombieEnemyData
        { _attackCooldown            = _initialAtkCooldown cfg
        , _postSpawnNoAttackCooldown = _postSpawnNoAttackSecs cfg
        , _sprites                   = sprs
        , _attackDescs               = attackDescs
        , _behavior                  = SpawnBehavior
        , _prevBehavior              = SpawnBehavior
        , _config                    = enemyCfg
        }
