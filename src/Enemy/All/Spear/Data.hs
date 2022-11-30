module Enemy.All.Spear.Data
    ( SpearEnemyData(..)
    , mkSpearEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Spear
import Enemy.All.Spear.AttackDescriptions
import Enemy.All.Spear.Behavior
import Enemy.All.Spear.Sprites
import FileCache
import Util
import Window.Graphics

data SpearEnemyData = SpearEnemyData
    { _throwAtkCooldownTtl :: Secs
    , _shoveAtkCooldownTtl :: Secs
    , _sprites             :: EnemySprites
    , _attackDescs         :: EnemyAttackDescriptions
    , _behavior            :: SpearEnemyBehavior
    , _prevBehavior        :: SpearEnemyBehavior
    , _config              :: EnemyConfig
    }

mkSpearEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SpearEnemyData
mkSpearEnemyData = do
    sprs        <- mkEnemySprites
    enemyCfg    <- _enemy <$> readConfigs
    attackDescs <- mkEnemyAttackDescs
    let cfg      = _spear enemyCfg

    return $ SpearEnemyData
        { _throwAtkCooldownTtl = _initialAtkCooldownSecs cfg
        , _shoveAtkCooldownTtl = _initialAtkCooldownSecs cfg
        , _sprites             = sprs
        , _attackDescs         = attackDescs
        , _behavior            = SpawnBehavior
        , _prevBehavior        = SpawnBehavior
        , _config              = enemyCfg
        }
