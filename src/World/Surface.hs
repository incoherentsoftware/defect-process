module World.Surface
    ( module World.Surface.Types
    , mkGeneralSurface
    , mkPlatformSurface
    , moveSurface
    , isGeneralSurface
    ) where

import Collision
import Util
import World.Surface.Types

mkSurface :: SurfaceType -> Hitbox -> Surface
mkSurface sType hitbox = Surface
    { _type   = sType
    , _hitbox = hitbox
    }

mkGeneralSurface :: Hitbox -> Surface
mkGeneralSurface hitbox = mkSurface GeneralSurface hitbox

mkPlatformSurface :: Hitbox -> Surface
mkPlatformSurface hitbox = mkSurface PlatformSurface hitbox

moveSurface :: Pos2 -> Surface -> Surface
moveSurface offset surface = surface {_hitbox = moveHitbox offset (_hitbox surface)}

isGeneralSurface :: Surface -> Bool
isGeneralSurface  = (== GeneralSurface) . _type
