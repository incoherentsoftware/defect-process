module Enemy.All.Bat.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _attack1 :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    let loadPackAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/bat-enemy.pack" f
    in
        EnemyAttackDescriptions <$>
        loadPackAtkDesc "attack.atk"
