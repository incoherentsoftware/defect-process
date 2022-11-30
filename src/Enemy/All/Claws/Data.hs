module Enemy.All.Claws.Data
    ( ClawsEnemyData(..)
    , mkClawsEnemyData
    , rollWillUseAttackProjectile
    , rollWillUseDash
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random          (randomRIO)

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Claws
import Enemy.All.Claws.AttackDescriptions
import Enemy.All.Claws.Behavior
import Enemy.All.Claws.Sprites
import FileCache
import Util
import Window.Graphics

data ClawsEnemyData = ClawsEnemyData
    { _attackCooldown          :: Secs
    , _sprites                 :: EnemySprites
    , _attackDescs             :: EnemyAttackDescriptions
    , _behavior                :: ClawsEnemyBehavior
    , _prevBehavior            :: ClawsEnemyBehavior
    , _willUseAttackProjectile :: Bool
    , _willUseDash             :: Bool
    , _config                  :: EnemyConfig
    }

mkClawsEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m ClawsEnemyData
mkClawsEnemyData = do
    sprs           <- mkEnemySprites
    attackDescs    <- mkEnemyAttackDescs
    cfg            <- _enemy <$> readConfigs
    willUseAtkProj <- rollWillUseAttackProjectile cfg
    willUseDash    <- rollWillUseDash cfg

    return $ ClawsEnemyData
        { _attackCooldown          = _initialAttackCooldown $ _claws cfg
        , _sprites                 = sprs
        , _attackDescs             = attackDescs
        , _behavior                = SpawnBehavior
        , _prevBehavior            = SpawnBehavior
        , _willUseAttackProjectile = willUseAtkProj
        , _willUseDash             = willUseDash
        , _config                  = cfg
        }

roll :: MonadIO m => Float -> m Bool
roll percent = (<= percent) <$> liftIO (randomRIO (0.0, 1.0))

rollWillUseAttackProjectile :: MonadIO m => EnemyConfig -> m Bool
rollWillUseAttackProjectile cfg = roll $ _willUseProjectileChance (_claws cfg)

rollWillUseDash :: MonadIO m => EnemyConfig -> m Bool
rollWillUseDash cfg = roll $ _willUseDashChance (_claws cfg)
