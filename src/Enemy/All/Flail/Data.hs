module Enemy.All.Flail.Data
    ( module Enemy.All.Flail.Data.Types
    , mkFlailEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Flail
import Enemy.All.Flail.AttackDescriptions
import Enemy.All.Flail.Behavior
import Enemy.All.Flail.Data.Types
import Enemy.All.Flail.Sprites
import FileCache
import Window.Graphics

mkFlailEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m FlailEnemyData
mkFlailEnemyData = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    enemyCfg    <- _enemy <$> readConfigs

    return $ FlailEnemyData
        { _attackCooldownTtl = _initialAtkCooldownSecs $ _flail enemyCfg
        , _sprites           = sprs
        , _attackDescs       = attackDescs
        , _behavior          = SpawnBehavior
        , _prevBehavior      = SpawnBehavior
        , _config            = enemyCfg
        }
