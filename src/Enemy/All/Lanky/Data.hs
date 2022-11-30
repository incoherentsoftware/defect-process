module Enemy.All.Lanky.Data
    ( LankyEnemyData(..)
    , mkLankyEnemyData
    , hasLankyEnemyDataAura
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack.Util
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Lanky
import Enemy.All.Lanky.AttackDescriptions
import Enemy.All.Lanky.Behavior
import Enemy.All.Lanky.Sprites
import FileCache
import Util
import Window.Graphics

data LankyEnemyData = LankyEnemyData
    { _auraHealth               :: Health
    , _lastKnownPlayerGroundPos :: Pos2
    , _summonAtkCooldownTtl     :: Secs
    , _beamAtkCooldownTtl       :: Secs
    , _sprites                  :: EnemySprites
    , _attackDescs              :: EnemyAttackDescriptions
    , _behavior                 :: LankyEnemyBehavior
    , _prevBehavior             :: LankyEnemyBehavior
    , _config                   :: EnemyConfig
    }

mkLankyEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m LankyEnemyData
mkLankyEnemyData pos = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    enemyCfg    <- _enemy <$> readConfigs
    let cfg      = _lanky enemyCfg

    return $ LankyEnemyData
        { _auraHealth               = _maxAuraHealth cfg
        , _lastKnownPlayerGroundPos = pos
        , _summonAtkCooldownTtl     = _initialAtkCooldownSecs cfg
        , _beamAtkCooldownTtl       = _initialAtkCooldownSecs cfg
        , _sprites                  = sprs
        , _attackDescs              = attackDescs
        , _behavior                 = SpawnBehavior
        , _prevBehavior             = SpawnBehavior
        , _config                   = enemyCfg
        }

hasLankyEnemyDataAura :: LankyEnemyData -> Bool
hasLankyEnemyDataAura = not . isHealthZero . _auraHealth
