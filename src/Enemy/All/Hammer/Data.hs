module Enemy.All.Hammer.Data
    ( HammerEnemyData(..)
    , mkHammerEnemyData
    , rollWillUseTeleport
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random          (randomRIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Hammer
import Enemy.All.Hammer.AttackDescriptions
import Enemy.All.Hammer.Behavior
import Enemy.All.Hammer.Sprites
import FileCache
import Util
import Window.Graphics

data HammerEnemyData = HammerEnemyData
    { _startPosY       :: PosY
    , _sprites         :: EnemySprites
    , _attackDescs     :: EnemyAttackDescriptions
    , _behavior        :: HammerEnemyBehavior
    , _prevBehavior    :: HammerEnemyBehavior
    , _willUseTeleport :: Bool
    , _config          :: EnemyConfig
    }

mkHammerEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m HammerEnemyData
mkHammerEnemyData (Pos2 _ posY) = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    cfg         <- _enemy <$> readConfigs

    return $ HammerEnemyData
        { _startPosY       = posY
        , _sprites         = sprs
        , _attackDescs     = attackDescs
        , _behavior        = SpawnBehavior
        , _prevBehavior    = SpawnBehavior
        , _willUseTeleport = False
        , _config          = cfg
        }

rollWillUseTeleport :: MonadIO m => EnemyConfig -> m Bool
rollWillUseTeleport cfg = do
    roll <- liftIO $ randomRIO (0.0, 1.0)
    return $ roll <= _willUseTeleportChance (_hammer cfg)
