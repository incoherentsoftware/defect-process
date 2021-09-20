module Configs.All.EnemyLockOn
    ( EnemyLockOnConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util
import {-# SOURCE #-} Enemy.Util

data EnemyLockOnConfig = EnemyLockOnConfig
    { _axe          :: EnemyLockOnReticleData
    , _bat          :: EnemyLockOnReticleData
    , _blob         :: EnemyLockOnReticleData
    , _bomb         :: EnemyLockOnReticleData
    , _boss         :: EnemyLockOnReticleData
    , _bubbleTurret :: EnemyLockOnReticleData
    , _claws        :: EnemyLockOnReticleData
    , _dog          :: EnemyLockOnReticleData
    , _flail        :: EnemyLockOnReticleData
    , _flying       :: EnemyLockOnReticleData
    , _giant        :: EnemyLockOnReticleData
    , _hammer       :: EnemyLockOnReticleData
    , _hop          :: EnemyLockOnReticleData
    , _lanky        :: EnemyLockOnReticleData
    , _spear        :: EnemyLockOnReticleData
    , _turret       :: EnemyLockOnReticleData
    , _wall         :: EnemyLockOnReticleData
    , _zombie       :: EnemyLockOnReticleData
    }
    deriving Generic

instance FromJSON EnemyLockOnConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
