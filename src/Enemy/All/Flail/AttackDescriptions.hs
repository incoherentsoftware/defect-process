module Enemy.All.Flail.AttackDescriptions
    ( module Enemy.All.Flail.AttackDescriptions.Types
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Enemy.All.Flail.AttackDescriptions.Types
import FileCache
import Window.Graphics

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadAtk "attack-forwards.atk" <*>
    loadAtk "attack-diag-upwards.atk" <*>
    loadAtk "attack-upwards.atk"
    where loadAtk = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/flail-enemy.pack" f
