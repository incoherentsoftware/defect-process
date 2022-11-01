module Level.Room.Tutorial.SandbagAir.Data
    ( SandbagAirData(..)
    , mkSandbagAirData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Level.Room.Tutorial.SandbagAir.Behavior
import Level.Room.Tutorial.SandbagAir.Sprites
import FileCache
import Util
import Window.Graphics

data SandbagAirData = SandbagAirData
    { _spawnPos     :: Pos2
    , _spawnDir     :: Direction
    , _sprites      :: SandbagAirSprites
    , _behavior     :: SandbagAirBehavior
    , _prevBehavior :: SandbagAirBehavior
    , _config       :: EnemyConfig
    }

mkSandbagAirData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m SandbagAirData
mkSandbagAirData spawnPos spawnDir = do
    sprs <- mkSandbagAirSprites
    cfg  <- _enemy <$> readConfigs

    return $ SandbagAirData
        { _spawnPos     = spawnPos
        , _spawnDir     = spawnDir
        , _sprites      = sprs
        , _behavior     = SpawnBehavior
        , _prevBehavior = SpawnBehavior
        , _config       = cfg
        }
