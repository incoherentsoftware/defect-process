module Configs.All.PlayerWeapon.Sword
    ( SwordConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Map as M

import Player.Meter
import Util
import {-# SOURCE #-} Enemy.Util

data SwordConfig = SwordConfig
    { _slash3AoeHitVel            :: Vel2
    , _summonAttackOrbOffset      :: Pos2
    , _summonAttackOrbCost        :: MeterValue
    , _attackOrbMaxActivations    :: Int
    , _attackOrbAliveSecs         :: Secs
    , _attackOrbAppearVelX        :: VelX
    , _attackOrbWidth             :: Float
    , _attackOrbHeight            :: Float
    , _attackOrbLockOnReticleData :: EnemyLockOnReticleData
    , _chargeHeldThresholdSecs    :: Secs
    , _chargeMeterCost            :: MeterValue
    , _chargeOverlaySprOffset     :: Pos2
    , _chargeOverlaySprOffsetMap  :: M.Map String [Pos2]
    }
    deriving Generic

instance FromJSON SwordConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
