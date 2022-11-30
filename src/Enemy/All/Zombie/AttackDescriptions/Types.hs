module Enemy.All.Zombie.AttackDescriptions.Types
    ( EnemyAttackDescriptions(..)
    ) where

import Attack.Description.Types

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _spit             :: AttackDescription
    , _fall             :: AttackDescription
    , _projSpit         :: AttackDescription
    , _projPuddleIgnite :: AttackDescription
    , _projFlames       :: AttackDescription
    }
