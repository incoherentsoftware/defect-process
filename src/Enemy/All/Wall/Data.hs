module Enemy.All.Wall.Data
    ( WallEnemyData(..)
    , mkWallEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Wall
import Enemy.All.Wall.AttackDescriptions
import Enemy.All.Wall.Behavior
import Enemy.All.Wall.Sprites
import FileCache
import Util
import Window.Graphics

data WallEnemyData = WallEnemyData
    { _attackCooldown :: Secs
    , _sprites        :: EnemySprites
    , _attackDescs    :: EnemyAttackDescriptions
    , _behavior       :: WallEnemyBehavior
    , _prevBehavior   :: WallEnemyBehavior
    , _config         :: EnemyConfig
    }

mkWallEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m WallEnemyData
mkWallEnemyData = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    cfg         <- _enemy <$> readConfigs

    return $ WallEnemyData
        { _attackCooldown = _initialAttackCooldown $ _wall cfg
        , _sprites        = sprs
        , _attackDescs    = attackDescs
        , _behavior       = SpawnBehavior
        , _prevBehavior   = SpawnBehavior
        , _config         = cfg
        }
