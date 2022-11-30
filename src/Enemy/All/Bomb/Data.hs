module Enemy.All.Bomb.Data
    ( BombEnemyData(..)
    , mkBombEnemyData
    , isExplodeTimerActive
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (isJust)

import Configs
import Configs.All.Enemy
import Enemy.All.Bomb.AttackDescriptions
import Enemy.All.Bomb.Behavior
import Enemy.All.Bomb.Sprites
import FileCache
import Util
import Window.Graphics

data BombEnemyData = BombEnemyData
    { _behavior        :: BombEnemyBehavior
    , _prevBehavior    :: BombEnemyBehavior
    , _explodeTimerTtl :: Maybe Secs
    , _sprites         :: EnemySprites
    , _attackDescs     :: EnemyAttackDescriptions
    , _config          :: EnemyConfig
    }

mkBombEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m BombEnemyData
mkBombEnemyData = do
    sprs     <- mkEnemySprites
    atkDescs <- mkEnemyAttackDescs
    cfg      <- _enemy <$> readConfigs

    return $ BombEnemyData
        { _behavior        = SpawnBehavior
        , _prevBehavior    = SpawnBehavior
        , _explodeTimerTtl = Nothing
        , _sprites         = sprs
        , _attackDescs     = atkDescs
        , _config          = cfg
        }

isExplodeTimerActive :: BombEnemyData -> Bool
isExplodeTimerActive = isJust . _explodeTimerTtl
