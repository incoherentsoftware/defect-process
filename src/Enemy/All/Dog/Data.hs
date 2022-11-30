module Enemy.All.Dog.Data
    ( DogEnemyData(..)
    , mkDogEnemyData
    , rollWillUseAttackProjectile
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random          (randomRIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Dog
import Enemy.All.Dog.AttackDescriptions
import Enemy.All.Dog.Behavior
import Enemy.All.Dog.Sprites
import FileCache
import Util
import Window.Graphics

data DogEnemyData = DogEnemyData
    { _attackCooldown          :: Secs
    , _sprites                 :: EnemySprites
    , _attackDescs             :: EnemyAttackDescriptions
    , _behavior                :: DogEnemyBehavior
    , _prevBehavior            :: DogEnemyBehavior
    , _willUseAttackProjectile :: Bool
    , _config                  :: EnemyConfig
    }

mkDogEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m DogEnemyData
mkDogEnemyData = do
    sprs           <- mkEnemySprites
    attackDescs    <- mkEnemyAttackDescs
    cfg            <- _enemy <$> readConfigs
    willUseAtkProj <- rollWillUseAttackProjectile cfg

    return $ DogEnemyData
        { _attackCooldown          = _initialAttackCooldown $ _dog cfg
        , _sprites                 = sprs
        , _attackDescs             = attackDescs
        , _behavior                = SpawnBehavior
        , _prevBehavior            = SpawnBehavior
        , _willUseAttackProjectile = willUseAtkProj
        , _config                  = cfg
        }

rollWillUseAttackProjectile :: MonadIO m => EnemyConfig -> m Bool
rollWillUseAttackProjectile cfg = do
    roll <- liftIO $ randomRIO (0.0, 1.0)
    return $ roll <= _willUseProjectileChance (_dog cfg)
