module Player.LockOnAim.Util
    ( playerLockOnAimPos
    ) where

import Player.LockOnAim.Types
import Util

playerLockOnAimPos :: PlayerLockOnAim -> Maybe Pos2
playerLockOnAimPos lockOnAim = _lockOnPos <$> _enemyLockOn lockOnAim
