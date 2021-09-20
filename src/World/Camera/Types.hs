module World.Camera.Types
    ( WorldCamera(..)
    ) where

import Util

data WorldCamera = WorldCamera
    { _locked              :: Bool
    , _leftBoundsOverride  :: Maybe PosX
    , _rightBoundsOverride :: Maybe PosX
    }
