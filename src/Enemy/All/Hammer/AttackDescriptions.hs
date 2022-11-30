module Enemy.All.Hammer.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _meteor           :: AttackDescription
    , _meteorHammerLand :: AttackDescription
    , _swing            :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadPackAtkDesc "attack-meteor.atk" <*>
    loadPackAtkDesc "attack-meteor-hammer-land.atk" <*>
    loadPackAtkDesc "attack-swing.atk"
    where loadPackAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/hammer-enemy.pack" f
