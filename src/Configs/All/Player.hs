module Configs.All.Player
    ( PlayerConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Map as M

import Util
import Window.Graphics.Opacity
import World.Util

data PlayerConfig = PlayerConfig
    { _width                      :: Float
    , _height                     :: Float
    , _moveSpeed                  :: Speed
    , _moveAccel                  :: Float
    , _airMomentumDecelerateSpeed :: Speed
    , _airStallThresholdVelY      :: VelY
    , _airStallForcedGravity      :: Float
    , _airStallMaxVelY            :: VelY
    , _hurtInvincibleSecs         :: Secs
    , _hurtInvincibleFadedSecs    :: Secs
    , _hurtInvincibleFadedOpacity :: Opacity
    , _hurtHitlagSecs             :: Secs
    , _deathHitlagSecs            :: Secs
    , _initialGold                :: GoldValue
    , _tauntGainMeterMultiplier   :: Int

    , _jumpVelY               :: VelY
    , _shortJumpReleaseSecs   :: Secs
    , _shortJumpThresholdVelY :: VelY
    , _shortJumpVelY          :: VelY
    , _doubleJumpVelY         :: VelY
    , _gravity                :: Acceleration
    , _platformDropSpeed      :: Speed
    , _moveSpeedShootModifier :: Float
    , _nearWallTopMaxDist     :: Distance

    , _gamepadAimDist            :: Distance
    , _aimAxisThreshold          :: Float
    , _defaultAimXOffset         :: PosX
    , _aimCrosshairAxisThreshold :: Float
    , _isGamepadAimLineEnabled   :: Bool
    , _aimLockOnCursorMaxDist    :: Distance
    , _minSoftLandingVelY        :: VelY
    , _minMediumLandingVelY      :: VelY
    , _minHardLandingVelY        :: VelY
    , _minAttackLandingVelY      :: VelY

    , _torsoNeckOffset               :: Pos2
    , _rearShoulderHipsOffset        :: Pos2
    , _leadShoulderHipsOffset        :: Pos2
    , _legsHipsOffset                :: Pos2
    , _backAimTorsoNeckOffset        :: Pos2
    , _backAimRearShoulderHipsOffset :: Pos2
    , _backAimLeadShoulderHipsOffset :: Pos2
    , _backAimLegsHipsOffset         :: Pos2

    , _chargeOverlaySprOffset    :: Pos2
    , _chargeOverlaySprOffsetMap :: M.Map String [Pos2]
    }
    deriving Generic

instance FromJSON PlayerConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
