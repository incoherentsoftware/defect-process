module Player.Weapon
    ( module Player.Weapon.Types
    , mkWeapon
    ) where

import Data.Dynamic  (fromDynamic)
import Data.Typeable (Typeable)

import Player.Weapon.Types

mkWeapon :: forall d. Typeable d => d -> WeaponType -> Weapon d
mkWeapon wpnData wpnType = Weapon
    { _data             = wpnData
    , _type             = wpnType
    , _think            = \_ _ _ _ -> return []
    , _update           = \_ _ _ -> return . id
    , _processDynamic   = processDynamic
    , _drawOverlay      = \_ _ _ _ -> return ()
    , _hitSoundFilePath = ""
    }

processDynamic :: Typeable d => WeaponProcessDynamic d
processDynamic dyn wpn = case fromDynamic dyn of
    Just update -> update wpn
    Nothing     -> wpn
