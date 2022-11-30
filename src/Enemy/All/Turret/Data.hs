module Enemy.All.Turret.Data
    ( TurretEnemyData(..)
    , mkTurretEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Turret
import Enemy.All.Turret.AttackDescriptions
import Enemy.All.Turret.Behavior
import Enemy.All.Turret.Sprites
import FileCache
import Id
import Msg
import Util
import Window.Graphics

data TurretEnemyData = TurretEnemyData
    { _attackCooldown     :: Secs
    , _attackProjMsgId    :: MsgId
    , _turnAroundTimerTtl :: Maybe Secs
    , _sprites            :: EnemySprites
    , _attackDescs        :: EnemyAttackDescriptions
    , _behavior           :: TurretEnemyBehavior
    , _prevBehavior       :: TurretEnemyBehavior
    , _config             :: EnemyConfig
    }

mkTurretEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m TurretEnemyData
mkTurretEnemyData = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    cfg         <- _enemy <$> readConfigs

    return $ TurretEnemyData
        { _attackCooldown     = _initialAttackCooldown $ _turret cfg
        , _attackProjMsgId    = NullId
        , _turnAroundTimerTtl = Nothing
        , _sprites            = sprs
        , _attackDescs        = attackDescs
        , _behavior           = SpawnBehavior
        , _prevBehavior       = SpawnBehavior
        , _config             = cfg
        }
