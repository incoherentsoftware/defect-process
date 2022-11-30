module Enemy.All.Lanky.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _summon     :: AttackDescription
    , _summonAura :: AttackDescription
    , _pillar     :: AttackDescription
    , _beam       :: AttackDescription
    , _beamAura   :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadPackAtkDesc "attack-summon.atk" <*>
    loadPackAtkDesc "attack-summon-aura.atk" <*>
    loadPackAtkDesc "attack-pillar.atk" <*>
    loadPackAtkDesc "attack-beam.atk" <*>
    loadPackAtkDesc "attack-beam-aura.atk"
    where loadPackAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath "data/enemies/lanky-enemy.pack" f
