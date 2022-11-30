module Player.MovementSkill.All.GrappleSkill.Projectile.Data
    ( GrappleProjData(..)
    , mkGrappleProjData
    ) where

import Collision
import Configs.All.PlayerSkill.Grapple
import Player
import Player.MovementSkill.All.GrappleSkill.Data
import Projectile as P
import Util

data GrappleProjData = GrappleProjData
    { _shotType             :: ShotType
    , _startVisualPos       :: Projectile GrappleProjData -> Pos2
    , _playerPos            :: Pos2
    , _playerTouchingGround :: Bool
    , _startPosVel          :: Vel2
    , _images               :: GrappleImages
    , _config               :: GrappleConfig
    }

mkGrappleProjData :: ShotType -> Player -> GrappleData -> GrappleProjData
mkGrappleProjData shotType player grappleData = GrappleProjData
    { _shotType             = shotType
    , _startVisualPos       = startVisualPos
    , _playerPos            = _pos (player :: Player)
    , _playerTouchingGround = _touchingGround $ _flags player
    , _startPosVel          = zeroVel2
    , _images               = _images (grappleData :: GrappleData)
    , _config               = _config (grappleData :: GrappleData)
    }
    where
        startVisualPos = \p ->
            let
                hbx          = projectileHitbox p
                startPos     = hitboxStartVertex hbx
                angle        = case hitboxVertices hbx of
                    [(Pos2 startX startY), (Pos2 endX endY)] -> atan2 (endY - startY) (endX - startX)
                    _                                        -> 0.0
                cfg          = _config (grappleData :: GrappleData)
                visualOffset = Pos2 (cos angle) (sin angle) `vecMul` _projStartPosVisualOffsetDist cfg
            in startPos `vecAdd` visualOffset
