module Enemy.All.Flail.AttackDescriptions.Types
    ( EnemyAttackDescriptions(..)
    ) where

import Attack.Description.Types

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _forwards    :: AttackDescription
    , _diagUpwards :: AttackDescription
    , _upwards     :: AttackDescription
    }
