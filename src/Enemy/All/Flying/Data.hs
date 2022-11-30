module Enemy.All.Flying.Data
    ( FlyingEnemyData(..)
    , mkFlyingEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Flying
import Enemy.All.Flying.AttackDescriptions
import Enemy.All.Flying.Behavior
import Enemy.All.Flying.Sprites
import FileCache
import Util
import Window.Graphics

data FlyingEnemyData = FlyingEnemyData
    { _startPosY      :: PosY
    , _attackCooldown :: Secs
    , _sprites        :: EnemySprites
    , _attackDescs    :: EnemyAttackDescriptions
    , _behavior       :: FlyingEnemyBehavior
    , _prevBehavior   :: FlyingEnemyBehavior
    , _prevAttackType :: AttackType
    , _config         :: EnemyConfig
    }

mkFlyingEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m FlyingEnemyData
mkFlyingEnemyData (Pos2 _ posY) = do
    sprs        <- mkEnemySprites
    attackDescs <- mkEnemyAttackDescs
    cfg         <- _enemy <$> readConfigs

    return $ FlyingEnemyData
        { _startPosY      = posY
        , _attackCooldown = _initialAttackCooldown $ _flying cfg
        , _sprites        = sprs
        , _attackDescs    = attackDescs
        , _behavior       = SpawnBehavior
        , _prevBehavior   = SpawnBehavior
        , _prevAttackType = ShootAttackType
        , _config         = cfg
        }
