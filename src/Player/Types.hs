module Player.Types
    ( Player(..)
    ) where

import qualified Data.Set as S

import Attack.Types
import Attack.Util
import Collision
import Configs.All.Player
import Id
import Msg.Types
import Player.BufferedInputState.Types
import Player.Flags
import Player.Gun.Manager.Types
import Player.Images.Types
import Player.LockOnAim.Types
import Player.Meter
import Player.Momentum
import Player.MovementSkill.Types
import Player.Overlay.Types
import Player.SecondarySkill.Manager.Types
import Player.SoundHashedIds
import Player.Sprites
import Player.TimersCounters
import Player.Upgrade.Manager
import Player.Weapon.Manager.Types
import Util
import Window.Graphics
import World.Util
import {-# SOURCE #-} Player

data Player = Player
    { _msgId                 :: MsgId
    , _pos                   :: Pos2
    , _vel                   :: Vel2
    , _dir                   :: Direction
    , _prevHitbox            :: Hitbox
    , _momentum              :: PlayerMomentum
    , _sprite                :: Sprite
    , _attack                :: Maybe Attack
    , _health                :: Health
    , _meter                 :: PlayerMeter
    , _gold                  :: GoldValue
    , _aimPos                :: Pos2
    , _lockOnAim             :: PlayerLockOnAim
    , _bufferedInputState    :: PlayerBufferedInputState
    , _weaponManager         :: WeaponManager
    , _gunManager            :: GunManager
    , _movementSkill         :: Maybe (Some MovementSkill)
    , _secondarySkillManager :: SecondarySkillManager
    , _upgradeManager        :: PlayerUpgradeManager
    , _hitByHashedIds        :: S.Set HashedId
    , _flags                 :: PlayerFlags
    , _timersCounters        :: PlayerTimersCounters
    , _overlay               :: Maybe (Some PlayerOverlay)
    , _images                :: PlayerImages
    , _sprites               :: PlayerSprites
    , _overlays              :: PlayerOverlays
    , _soundHashedIds        :: PlayerSoundHashedIds
    , _config                :: PlayerConfig
    }

instance CollisionEntity Player where
    collisionEntityMsgId :: Player -> MsgId
    collisionEntityMsgId = _msgId

    collisionEntityHitbox :: Player -> Hitbox
    collisionEntityHitbox = playerHitbox

    collisionEntityPrevHitbox :: Player -> Hitbox
    collisionEntityPrevHitbox = _prevHitbox

    collisionEntityVel :: Player -> Vel2
    collisionEntityVel = _vel

    collisionEntityDir :: Player -> Direction
    collisionEntityDir = _dir

    collisionEntityHitByHashedIds :: Player -> S.Set HashedId
    collisionEntityHitByHashedIds = _hitByHashedIds

    collisionEntityWallProximityHitbox :: Player -> Maybe Hitbox
    collisionEntityWallProximityHitbox = Just . playerWallProximityHitbox
