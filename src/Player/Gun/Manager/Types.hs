module Player.Gun.Manager.Types
    ( GunManager(..)
    ) where

import Player.Gun.Types
import Player.Gun.FireDrawState.Types
import Util

data GunManager = GunManager
    { _guns          :: [Some Gun]
    , _fireDrawState :: GunFireDrawState
    }
