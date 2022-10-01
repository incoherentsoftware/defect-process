module Attack.Description.Types
    ( AttackRefreshHitboxesType(..)
    , AttackHitEffectType(..)
    , AttackOnHit
    , AttackOnHitType(..)
    , AttackOnSurfaceHit
    , AttackOnSurfaceHitType(..)
    , AttackDescription(..)
    ) where

import Attack.Sound
import Attack.Util
import Collision
import FileCache
import Msg.Types
import Particle.All.AttackSpecks.Types
import Player.Meter
import Util
import Window.Graphics
import World.Screenshake.Types
import {-# SOURCE #-} Attack.Types

data AttackRefreshHitboxesType
    = NoRefreshHitboxes
    | RefreshHitboxesOnLoop
    | RefreshHitboxesPerFrameCount Int

type AttackOnHit = Hitbox -> MsgId -> Attack -> [Msg ThinkCollisionMsgsPhase]

data AttackOnHitType
    = NormalOnHitType                -- normal on hit behavior (damage enemy, play attack hit sound, etc)
    | AddedOnHitType AttackOnHit     -- additional on hit behavior in addition to the normal behavior
    | ReplacedOnHitType AttackOnHit  -- replacement on hit behavior

type AttackOnSurfaceHit = Hitbox -> Attack -> [Msg ThinkCollisionMsgsPhase]

data AttackOnSurfaceHitType
    = NoSurfaceHitType
    | WithSurfaceHitType AttackOnSurfaceHit

data AttackDescription = AttackDescription
    { _filePath                :: FilePath
    , _sprite                  :: Sprite
    , _hitboxes                :: [Maybe Hitbox]
    , _vels                    :: [AttackVel]
    , _hitVels                 :: [Maybe Vel2]
    , _cancelFrameIndex        :: FrameIndex
    , _walkCancelFrameIndex    :: FrameIndex
    , _launchTargetOffsetY     :: Maybe Float
    , _noLaunchTargetOvershoot :: Maybe Bool
    , _isWeakHitVels           :: Maybe Bool
    , _hitlag                  :: Secs
    , _damage                  :: Damage
    , _stagger                 :: Stagger
    , _hitstunMultiplier       :: Maybe Float
    , _sound                   :: AttackSound
    , _screenshakeType         :: ScreenshakeType
    , _refreshHitboxesType     :: AttackRefreshHitboxesType
    , _hitEffectPaths          :: [PackResourceFilePath]
    , _hitEffectType           :: AttackHitEffectType
    , _onHitType               :: AttackOnHitType
    , _onSurfaceHitType        :: AttackOnSurfaceHitType
    , _meterGain               :: Maybe MeterValue
    , _nextAttackDescOnDone    :: Maybe AttackDescription
    , _isRanged                :: Bool
    , _specksType              :: Maybe AttackSpecksType
    , _specksPos               :: Maybe AttackSpecksPosition
    , _specksDirection         :: Maybe AttackSpecksDirection
    }

instance Eq AttackDescription where
    (==) :: AttackDescription -> AttackDescription -> Bool
    (==) atkDesc1 atkDesc2 =
        _filePath (atkDesc1 :: AttackDescription) == _filePath (atkDesc2 :: AttackDescription)

-- Need this instance to use as key in Map
instance Ord AttackDescription where
    compare :: AttackDescription -> AttackDescription -> Ordering
    compare atkDesc1 atkDesc2 =
        compare (_filePath (atkDesc1 :: AttackDescription)) (_filePath (atkDesc2 :: AttackDescription))

instance Show AttackDescription where
    show :: AttackDescription -> String
    show atkDesc = _filePath (atkDesc :: AttackDescription)
