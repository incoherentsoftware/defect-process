module Enemy.StasisData.Types
    ( EnemyStasisData(..)
    ) where

import Attack.Hit.Types
import Id
import Util

data EnemyStasisData = EnemyStasisData
    { _stasisTtl           :: Secs
    , _deferredAttackHits  :: [AttackHit]
    , _stasisSoundHashedId :: HashedId
    }
