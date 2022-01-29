module Player.Gun
    ( module Player.Gun.Types
    , module Player.Gun.Status
    , mkGun
    ) where

import Data.Dynamic  (fromDynamic)
import Data.Typeable (Typeable)

import Player.Gun.Status
import Player.Gun.Types

mkGun :: Typeable d => d -> GunType -> Gun d
mkGun gunData gunType = Gun
    { _data          = gunData
    , _type          = gunType
    , _fireDrawData  = Nothing
    , _drawOverlay   = \_ _ -> return ()
    , _think         = \_ _ _ -> return []
    , _update        = return . id
    , _updateDynamic = updateDynamic
    }

updateDynamic :: Typeable d => GunUpdateDynamic d
updateDynamic dyn gun = case fromDynamic dyn of
    Just update -> update gun
    Nothing     -> gun
