module Player.LockOnAim.Util
    ( playerLockOnAimPos
    ) where

import Player.LockOnAim.Types
import Util

playerLockOnAimPos :: PlayerLockOnAim -> Maybe Pos2
playerLockOnAimPos lockOnAim
    | _manualOverride lockOnAim = Nothing
    | otherwise                 = _lockOnPos <$> _enemyLockOn lockOnAim
