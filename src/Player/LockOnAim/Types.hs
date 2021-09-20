module Player.LockOnAim.Types
    ( PlayerEnemyLockOn(..)
    , PlayerLockOnAim(..)
    ) where

import qualified Data.Set as S

import Attack.Util
import Msg.Types
import Util
import Window.Graphics

data PlayerEnemyLockOn = PlayerEnemyLockOn
    { _enemyId                  :: MsgId
    , _enemyHealth              :: Health
    , _enemyVel                 :: Vel2
    , _lockOnPos                :: Pos2
    , _reticleScale             :: Float
    , _prevSwitchTargetEnemyIds :: S.Set MsgId
    }

data PlayerLockOnAim = PlayerLockOnAim
    { _enemyLockOn    :: Maybe PlayerEnemyLockOn
    , _reticleSprites :: [Sprite]
    , _manualOverride :: Bool
    }
