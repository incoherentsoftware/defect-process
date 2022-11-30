module Enemy.All.Claws.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _slash             :: AttackDescription
    , _releaseProjectile :: AttackDescription
    , _projectile        :: AttackDescription
    , _dash              :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    let loadAtkDesc = \f -> loadPackAttackDescription (PackResourceFilePath "data/enemies/claws-enemy.pack" f)
    in
        EnemyAttackDescriptions <$>
        loadAtkDesc "attack-slash.atk" <*>
        loadAtkDesc "attack-release-projectile.atk" <*>
        loadAtkDesc "attack-projectile.atk" <*>
        loadAtkDesc "dash.atk"
