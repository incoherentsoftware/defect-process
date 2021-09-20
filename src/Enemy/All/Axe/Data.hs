module Enemy.All.Axe.Data
    ( AxeEnemyData(..)
    , mkAxeEnemyData
    , rollWillUseAttackLunge
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random          (randomRIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Axe
import Enemy.All.Axe.AttackDescriptions
import Enemy.All.Axe.Behavior
import Enemy.All.Axe.Sprites
import FileCache
import Util
import Window.Graphics

data AxeEnemyData = AxeEnemyData
    { _attackCooldown     :: Secs
    , _sprites            :: EnemySprites
    , _attackDescs        :: EnemyAttackDescriptions
    , _behavior           :: AxeEnemyBehavior
    , _prevBehavior       :: AxeEnemyBehavior
    , _willUseAttackLunge :: Bool
    , _config             :: EnemyConfig
    }

mkAxeEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m AxeEnemyData
mkAxeEnemyData = do
    sprs               <- mkEnemySprites
    attackDescs        <- mkEnemyAttackDescs
    cfg                <- _enemy <$> readConfigs
    willUseAttackLunge <- rollWillUseAttackLunge cfg

    return $ AxeEnemyData
        { _attackCooldown     = _initialAttackCooldown $ _axe cfg
        , _sprites            = sprs
        , _attackDescs        = attackDescs
        , _behavior           = SpawnBehavior
        , _prevBehavior       = SpawnBehavior
        , _willUseAttackLunge = willUseAttackLunge
        , _config             = cfg
        }

rollWillUseAttackLunge :: MonadIO m => EnemyConfig -> m Bool
rollWillUseAttackLunge cfg = do
    roll <- liftIO $ randomRIO (0.0, 1.0)
    return $ roll <= _willUseLungeChance (_axe cfg)
