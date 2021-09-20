module Enemy.All.Axe.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _attackSlash     :: AttackDescription
    , _attackLunge     :: AttackDescription
    , _attackLungeLand :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    let loadAtk = \s -> loadPackAttackDescription (PackResourceFilePath "data/enemies/axe-enemy.pack" s)
    in
        EnemyAttackDescriptions <$>
        loadAtk "attack-slash.atk" <*>
        loadAtk "attack-lunge.atk" <*>
        loadAtk "attack-lunge-land.atk"
