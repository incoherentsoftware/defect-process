module Enemy.All.Giant.Data
    ( GiantEnemyData(..)
    , mkGiantEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Giant
import Enemy.All.Giant.AttackDescriptions
import Enemy.All.Giant.Behavior
import Enemy.All.Giant.Sprites
import FileCache
import Util
import Window.Graphics

data GiantEnemyData = GiantEnemyData
    { _attackCooldown :: Secs
    , _sprites        :: EnemySprites
    , _attackDescs    :: EnemyAttackDescriptions
    , _behavior       :: GiantEnemyBehavior
    , _prevBehavior   :: GiantEnemyBehavior
    , _config         :: EnemyConfig
    }

mkGiantEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GiantEnemyData
mkGiantEnemyData = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    enemyCfg    <- _enemy <$> readConfigs

    return $ GiantEnemyData
        { _attackCooldown = _initialAttackCooldown $ _giant enemyCfg
        , _sprites        = sprs
        , _attackDescs    = attackDescs
        , _behavior       = SpawnBehavior
        , _prevBehavior   = SpawnBehavior
        , _config         = enemyCfg
        }
