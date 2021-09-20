module Level.Room.ArenaWalls.Types
    ( module Level.Room.ArenaWalls.EnemySpawn.Types
    , RoomArenaWallsStatus(..)
    , RoomArenaWallsSprites(..)
    , RoomArenaWalls(..)
    ) where

import qualified Data.Set as S

import Collision.Hitbox.Types
import Id
import Level.Room.ArenaWalls.EnemySpawn.Types
import Level.Room.ArenaWalls.Ripple.Types
import Util
import Window.Graphics
import World.Surface.Types
import World.Util

data RoomArenaWallsStatus
    = WallsReadyStatus
    | WallsActiveStatus Secs
    | WallsDoneStatus
    deriving (Eq, Show)

data RoomArenaWallsSprites = RoomArenaWallsSprites
    { _wallAppear   :: Sprite
    , _wallIdle     :: Sprite
    , _wallHit      :: Sprite
    , _markerRipple :: Sprite
    }

data RoomArenaWalls = RoomArenaWalls
    { _hashedId              :: HashedId
    , _status                :: RoomArenaWallsStatus
    , _leftWall              :: Surface
    , _rightWall             :: Surface
    , _triggerHitbox         :: Hitbox
    , _triggerHitByHashedIds :: S.Set HashedId
    , _enemySpawnPositions   :: [Pos2]
    , _enemySpawnWaves       :: [EnemySpawnWave]
    , _goldValue             :: GoldValue
    , _markerRipples         :: [RoomArenaMarkerRipple]
    , _markerSprite          :: Sprite
    , _wallSprite            :: Sprite
    , _sprites               :: RoomArenaWallsSprites
    }
