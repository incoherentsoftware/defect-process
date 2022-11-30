module Enemy.All.Hop.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _attackPreHopLong   :: AttackDescription
    , _attackHopLong      :: AttackDescription
    , _attackPreHopShort  :: AttackDescription
    , _attackHopShort     :: AttackDescription
    , _attackHopShortLand :: AttackDescription
    , _attackProj         :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadAtk "attack-pre-hop-long.atk" <*>
    loadAtk "attack-hop-long.atk" <*>
    loadAtk "attack-pre-hop-short.atk" <*>
    loadAtk "attack-hop-short.atk" <*>
    loadAtk "attack-hop-short-land.atk" <*>
    loadAtk "attack-projectile.atk"
    where loadAtk = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/hop-enemy.pack" f
