module Enemy.All.Spear.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _shove     :: AttackDescription
    , _throw     :: AttackDescription
    , _spearProj :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadAtk "attack-shove.atk" <*>
    loadAtk "attack-throw.atk" <*>
    loadAtk "attack-projectile.atk"
    where loadAtk = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/spear-enemy.pack" f
