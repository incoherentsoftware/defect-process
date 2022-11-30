module Enemy.All.Wall.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _releaseWall :: AttackDescription
    , _wallProj    :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadAtk "attack.atk" <*>
    loadAtk "attack-projectile.atk"
    where loadAtk = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/wall-enemy.pack" f
