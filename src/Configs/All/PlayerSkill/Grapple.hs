module Configs.All.PlayerSkill.Grapple
    ( GrappleConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Map as M
import qualified Data.Vector as V

import Attack.Util
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawState.LegsState.Types
import Util

data GrappleConfig = GrappleConfig
    { _grappleCooldown :: Secs

    , _projRange                    :: Distance
    , _projThrowSecs                :: Secs
    , _projTowardsSecs              :: Secs
    , _projPullSecs                 :: Secs
    , _projSpeed                    :: Speed
    , _projHitlag                   :: Secs
    , _projDamage                   :: Damage
    , _projStagger                  :: Stagger
    , _projHitstunMultiplier        :: Float
    , _projStartPosVisualOffsetDist :: Distance
    , _projThrowTargetDistSq        :: Distance
    , _projProngsLineOffset         :: Pos2

    , _projStartLeadShoulderOffsets :: M.Map GunFireDrawAngle Pos2
    , _projPullStartVisualOffsets   :: M.Map GunFireDrawAngle Pos2

    , _pullHeadAngleMultiplier    :: Float
    , _pullLeadArmAngleMultiplier :: Float
    , _pullRearArmAngleMultiplier :: Float
    , _pullTorsoAngleMultiplier   :: Float
    , _pullRearArmAngleMultiplier :: Float

    , _pullTorsoNeckOffsets        :: M.Map GunFireDrawAngle Pos2
    , _pullRearShoulderHipsOffsets :: M.Map GunFireDrawAngle Pos2
    , _pullLeadShoulderHipsOffsets :: M.Map GunFireDrawAngle Pos2
    , _pullLegsHipsOffsets         :: M.Map LegsStatus (V.Vector Pos2)

    , _playerAfterHangtimeSecs                     :: Secs
    , _playerThrowHangtimeSecs                     :: Secs
    , _playerGrappleSpeed                          :: Speed
    , _playerHangtimeCancelFallVel                 :: Vel2
    , _playerPullOffsetX                           :: PosX
    , _playerPullMinDistance                       :: Distance
    , _playerPullActiveMaxSecs                     :: Secs
    , _playerTowardsXOffset                        :: PosX
    , _playerTowardsHandsOffset                    :: Pos2
    , _playerTowardsPlatformPopUpVel               :: Vel2
    , _playerTowardsPlatformPopUpGroundMinDistance :: Distance
    , _playerTowardsWallTopPopUpVel                :: Vel2
    , _playerTowardsActiveMaxSecs                  :: Secs
    , _playerDownCancelVel                         :: Vel2

    , _enemyTowardsHangtimeSecs :: Secs
    , _enemyPullGrappleSpeed    :: Speed
    , _enemyPullHangtimeSecs    :: Secs
    , _enemyPullMinBreakDist    :: Distance
    , _enemyAfterPullVel        :: Vel2

    , _enemyPullDummyProjSecs :: Secs
    }
    deriving Generic

instance FromJSON GrappleConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
