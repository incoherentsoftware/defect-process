module Enemy.All.Hop.Data
    ( HopEnemyData(..)
    , mkHopEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Enemy.All.Hop.AttackDescriptions
import Enemy.All.Hop.Behavior
import Enemy.All.Hop.Sprites
import FileCache
import Window.Graphics

data HopEnemyData = HopEnemyData
    { _sprites      :: EnemySprites
    , _attackDescs  :: EnemyAttackDescriptions
    , _behavior     :: HopEnemyBehavior
    , _prevBehavior :: HopEnemyBehavior
    , _config       :: EnemyConfig
    }

mkHopEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m HopEnemyData
mkHopEnemyData = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    cfg         <- _enemy <$> readConfigs

    return $ HopEnemyData
        { _sprites      = sprs
        , _attackDescs  = attackDescs
        , _behavior     = SpawnBehavior
        , _prevBehavior = SpawnBehavior
        , _config       = cfg
        }
