module Enemy.All.BubbleTurret.Data
    ( BubbleTurretEnemyData(..)
    , mkBubbleTurretEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.BubbleTurret
import Enemy.All.BubbleTurret.AttackDescriptions
import Enemy.All.BubbleTurret.Behavior
import Enemy.All.BubbleTurret.Sprites
import FileCache
import Util
import Window.Graphics

data BubbleTurretEnemyData = BubbleTurretEnemyData
    { _attackCooldown     :: Secs
    , _turnAroundTimerTtl :: Maybe Secs
    , _sprites            :: EnemySprites
    , _attackDescs        :: EnemyAttackDescriptions
    , _behavior           :: BubbleTurretEnemyBehavior
    , _prevBehavior       :: BubbleTurretEnemyBehavior
    , _config             :: EnemyConfig
    }

mkBubbleTurretEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m BubbleTurretEnemyData
mkBubbleTurretEnemyData = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    cfg         <- _enemy <$> readConfigs

    return $ BubbleTurretEnemyData
        { _attackCooldown     = _initialAttackCooldown $ _bubbleTurret cfg
        , _turnAroundTimerTtl = Nothing
        , _sprites            = sprs
        , _attackDescs        = attackDescs
        , _behavior           = SpawnBehavior
        , _prevBehavior       = SpawnBehavior
        , _config             = cfg
        }
