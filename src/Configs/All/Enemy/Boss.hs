module Configs.All.Enemy.Boss
    ( BossEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Util
import Window.Graphics.Util
import {-# SOURCE #-} Enemy.Util

data BossEnemyConfig = BossEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _skipSpawnAnim :: Bool
    , _noSuperArmor  :: Bool

    , _staggerThreshold     :: Stagger
    , _idleSecs             :: Secs
    , _maxIncapacitatedSecs :: Secs
    , _healthbarBackdropPos :: Pos2
    , _attackShortRange     :: Distance
    , _attackMediumRange    :: Distance
    , _attackLongRange      :: Distance

    , _releaseBlobProjOffset :: Pos2
    , _teleportOffset        :: Pos2
    , _turret1ProjOffset     :: Pos2
    , _turret2ProjOffset     :: Pos2
    , _turretProjDamage      :: Damage
    , _turretProjMaxRange    :: Distance

    , _hpThresholdBodyOffset :: Pos2

    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData :: EnemyHurtEffectData
    }
    deriving Generic

instance FromJSON BossEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
