module Enemy.All.Blob.Data
    ( BlobEnemyData(..)
    , mkBlobEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Enemy.All.Blob.AttackDescriptions
import Enemy.All.Blob.Behavior
import Enemy.All.Blob.Sprites
import FileCache
import Window.Graphics

data BlobEnemyData = BlobEnemyData
    { _sprites      :: EnemySprites
    , _attackDescs  :: EnemyAttackDescriptions
    , _behavior     :: BlobEnemyBehavior
    , _prevBehavior :: BlobEnemyBehavior
    , _config       :: EnemyConfig
    }

mkBlobEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m BlobEnemyData
mkBlobEnemyData = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    cfg         <- _enemy <$> readConfigs

    return $ BlobEnemyData
        { _sprites      = sprs
        , _attackDescs  = attackDescs
        , _behavior     = SpawnBehavior
        , _prevBehavior = SpawnBehavior
        , _config       = cfg
        }
