module Enemy.LockOnData
    ( EnemyLockOnData(..)
    ) where

import Attack.Util
import Collision.Hitbox.Types
import Msg.Types
import Util

data EnemyLockOnData = EnemyLockOnData
    { _enemyId       :: MsgId
    , _enemyHitbox   :: Hitbox
    , _enemyHealth   :: Health
    , _enemyVel      :: Vel2
    , _reticleScale  :: Float
    , _reticleOffset :: Pos2
    }
