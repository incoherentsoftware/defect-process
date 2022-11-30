module Enemy.All.Bat.Data
    ( BatEnemyData(..)
    , mkBatEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Enemy.All.Bat.AttackDescriptions
import Enemy.All.Bat.Behavior
import Enemy.All.Bat.Sprites
import FileCache
import Util
import Window.Graphics

data BatEnemyData = BatEnemyData
    { _startPosY    :: PosY
    , _sprites      :: EnemySprites
    , _attackDescs  :: EnemyAttackDescriptions
    , _behavior     :: BatEnemyBehavior
    , _prevBehavior :: BatEnemyBehavior
    , _config       :: EnemyConfig
    }

mkBatEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m BatEnemyData
mkBatEnemyData (Pos2 _ posY) = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    cfg         <- _enemy <$> readConfigs

    return $ BatEnemyData
        { _startPosY    = posY
        , _sprites      = sprs
        , _attackDescs  = attackDescs
        , _behavior     = SpawnBehavior
        , _prevBehavior = SpawnBehavior
        , _config       = cfg
        }
