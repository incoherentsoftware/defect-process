module Player.LockOnAim.Types
    ( PlayerEnemyLockOnSource(..)
    , PlayerEnemyLockOn(..)
    , PlayerLockOnAim(..)
    ) where

import qualified Data.Set as S

import Attack.Util
import Msg.Types
import Util
import Window.Graphics

data PlayerEnemyLockOnSource
    = CycleLockOnSource
    | CursorLockOnSource
    | GamepadAxisLockOnSource
    deriving Eq

data PlayerEnemyLockOn = PlayerEnemyLockOn
    { _enemyId                  :: MsgId
    , _enemyHealth              :: Health
    , _enemyVel                 :: Vel2
    , _lockOnPos                :: Pos2
    , _reticleScale             :: Float
    , _source                   :: PlayerEnemyLockOnSource
    , _prevSwitchTargetEnemyIds :: S.Set MsgId
    }

data PlayerLockOnAim = PlayerLockOnAim
    { _enemyLockOn    :: Maybe PlayerEnemyLockOn
    , _reticleSprites :: [Sprite]
    , _manualOverride :: Bool
    }
