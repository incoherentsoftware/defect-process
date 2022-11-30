module Enemy.All.Zombie.AttackDescriptions
    ( module Enemy.All.Zombie.AttackDescriptions.Types
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Enemy.All.Zombie.AttackDescriptions.Types
import FileCache
import Window.Graphics

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadAtk "attack-spit.atk" <*>
    loadAtk "attack-fall.atk" <*>
    loadAtk "attack-projectile-spit.atk" <*>
    loadAtk "attack-projectile-puddle-ignite.atk" <*>
    loadAtk "attack-projectile-flames.atk"
    where loadAtk = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/zombie-enemy.pack" f
