module Player.Gun.All.RicochetGun.Shot.Data
    ( ShotType(..)
    , ShotLastHit(..)
    , ShotPrevBounceData(..)
    , ShotProjectileData(..)
    , mkShotProjectileData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Collision.Hitbox
import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.RicochetGun
import FileCache
import Msg
import Window.Graphics

beamSpritePath =
    PackResourceFilePath "data/player/player-guns.pack" "ricochet-gun-projectile-beam.spr" :: PackResourceFilePath

data ShotType
    = DirectShot
    | BounceShot

data ShotLastHit
    = SurfaceHit
    | EnemyHit
    | NoHit

data ShotPrevBounceData
    = PrevBounceEnemyId MsgId
    | PrevBounceSurfaceHitbox Hitbox
    | NoPrevBounceData

data ShotProjectileData = ShotProjectileData
    { _type           :: ShotType
    , _numBounces     :: Int
    , _lastHit        :: ShotLastHit
    , _prevBounceData :: ShotPrevBounceData
    , _beamSprite     :: Sprite
    , _config         :: RicochetGunConfig
    }

mkShotProjectileData
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => ShotType
    -> Int
    -> ShotPrevBounceData
    -> m ShotProjectileData
mkShotProjectileData shotType numBounces prevBounceData = do
    beamSpr <- loadPackSprite beamSpritePath
    cfg     <- readConfig _playerGun _ricochetGun
    return $ ShotProjectileData
        { _type           = shotType
        , _numBounces     = numBounces
        , _lastHit        = NoHit
        , _prevBounceData = prevBounceData
        , _beamSprite     = beamSpr
        , _config         = cfg
        }
