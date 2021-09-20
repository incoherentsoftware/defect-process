module Projectile.Manager.Types
    ( ProjectileManager(..)
    ) where

import Projectile
import Util

data ProjectileManager = ProjectileManager
    { _projectiles :: [Some Projectile]
    }
