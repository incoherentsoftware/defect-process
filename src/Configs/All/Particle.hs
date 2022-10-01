module Configs.All.Particle
    ( ParticleConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data ParticleConfig = ParticleConfig
    { _enemyHurtSpeckInitialOffsetLow  :: Distance
    , _enemyHurtSpeckInitialOffsetHigh :: Distance
    , _enemyHurtSpeckInitialSpeedLow   :: Speed
    , _enemyHurtSpeckInitialSpeedHigh  :: Speed
    , _enemyHurtSpeckAcceleration      :: Acceleration
    , _enemyHurtSpeckGravity           :: Float

    , _attackSpeckInitialSpeedLow  :: Speed
    , _attackSpeckInitialSpeedHigh :: Speed
    , _attackSpeckAcceleration     :: Acceleration
    , _attackSpeckGravity          :: Float

    , _swordSpeckInitialOffsetLow         :: Distance
    , _swordSpeckInitialOffsetHigh        :: Distance
    , _staffSpeckInitialOffsetLow         :: Distance
    , _staffSpeckInitialOffsetHigh        :: Distance
    , _gauntletsSpeckInitialOffsetLow     :: Distance
    , _gauntletsSpeckInitialOffsetHigh    :: Distance
    , _scytheSpeckInitialOffsetLow        :: Distance
    , _scytheSpeckInitialOffsetHigh       :: Distance
    , _spiritBladeSpeckInitialOffsetLow   :: Distance
    , _spiritBladeSpeckInitialOffsetHigh  :: Distance
    , _bulletSpeckInitialOffsetLow        :: Distance
    , _bulletSpeckInitialOffsetHigh       :: Distance
    , _shardSpeckInitialOffsetLow         :: Distance
    , _shardSpeckInitialOffsetHigh        :: Distance
    , _shardExplodeSpeckInitialOffsetLow  :: Distance
    , _shardExplodeSpeckInitialOffsetHigh :: Distance
    , _spikeSpeckInitialOffsetLow         :: Distance
    , _spikeSpeckInitialOffsetHigh        :: Distance
    , _ricochetSpeckInitialOffsetLow      :: Distance
    , _ricochetSpeckInitialOffsetHigh     :: Distance
    , _grenadeSpeckInitialOffsetLow       :: Distance
    , _grenadeSpeckInitialOffsetHigh      :: Distance
    , _mineSpeckInitialOffsetLow          :: Distance
    , _mineSpeckInitialOffsetHigh         :: Distance
    }
    deriving Generic

instance FromJSON ParticleConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
