module Enemy.All.Flying.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _shoot    :: AttackDescription
    , _fireball :: AttackDescription
    , _shock    :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadPackAtkDesc "attack-projectile-release.atk" <*>
    loadPackAtkDesc "attack-projectile.atk" <*>
    loadPackAtkDesc "attack-shock.atk"
    where loadPackAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/flying-enemy.pack" f
