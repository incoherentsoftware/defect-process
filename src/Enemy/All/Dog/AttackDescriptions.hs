module Enemy.All.Dog.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _shootCharge     :: AttackDescription
    , _shootRelease    :: AttackDescription
    , _shootProjectile :: AttackDescription
    , _headbutt        :: AttackDescription
    , _headbuttLand    :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    let loadAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/dog-enemy.pack" f
    in
        EnemyAttackDescriptions <$>
        loadAtkDesc "attack-charge.atk" <*>
        loadAtkDesc "attack-release.atk" <*>
        loadAtkDesc "attack-projectile.atk" <*>
        loadAtkDesc "attack-headbutt.atk" <*>
        loadAtkDesc "attack-headbutt-land.atk"
