module Level.Room.Tutorial.SandbagGround.Data
    ( SandbagGroundData(..)
    , mkSandbagGroundData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import FileCache
import Level.Room.Tutorial.SandbagGround.Behavior
import Level.Room.Tutorial.SandbagGround.Sprites
import Util
import Window.Graphics

data SandbagGroundData = SandbagGroundData
    { _spawnPos     :: Pos2
    , _spawnDir     :: Direction
    , _sprites      :: SandbagGroundSprites
    , _behavior     :: SandbagGroundBehavior
    , _prevBehavior :: SandbagGroundBehavior
    , _config       :: EnemyConfig
    }

mkSandbagGroundData
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> m SandbagGroundData
mkSandbagGroundData spawnPos spawnDir = do
    sprs <- mkSandbagGroundSprites
    cfg  <- _enemy <$> readConfigs

    return $ SandbagGroundData
        { _spawnPos     = spawnPos
        , _spawnDir     = spawnDir
        , _sprites      = sprs
        , _behavior     = SpawnBehavior
        , _prevBehavior = SpawnBehavior
        , _config       = cfg
        }
