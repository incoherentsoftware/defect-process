module Configs.All.Level
    ( LevelConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.List.NonEmpty as NE

import Enemy.LockOnReticleData
import Level.Room.ArenaWalls.EnemySpawn.Types
import Level.Room.ArenaWalls.JSON
import Level.Room.Event.SlotMachine.Util
import Util
import World.Util

data LevelConfig = LevelConfig
    { _maxNumArenas          :: Int
    , _runProgressScreenSecs :: Secs
    , _endBossGoldValue      :: GoldValue
    , _endBossSpawnWaitSecs  :: Secs
    , _endWarpOutWaitSecs    :: Secs

    , _itemPickupWeaponGoldValue               :: GoldValue
    , _itemPickupGunGoldValue                  :: GoldValue
    , _itemPickupMovementSkillGoldValue        :: GoldValue
    , _itemPickupStoneFormSkillGoldValue       :: GoldValue
    , _itemPickupFlightSkillGoldValue          :: GoldValue
    , _itemPickupFastFallSkillGoldValue        :: GoldValue
    , _itemPickupStasisBlastSkillGoldValue     :: GoldValue
    , _itemPickupMarkRecallSkillGoldValue      :: GoldValue
    , _itemPickupSummonPlatformSkillGoldValue  :: GoldValue
    , _itemPickupMeterUpgradeGoldValue         :: GoldValue
    , _itemPickupDoubleJumpUpgradeGoldValue    :: GoldValue
    , _itemPickupMovementSkillUpgradeGoldValue :: GoldValue
    , _itemPickupHealthGoldValue               :: GoldValue
    , _itemPickupHealthMultiplicandGoldValue   :: GoldValue

    , _arenaWallsGoldDrops :: NE.NonEmpty RoomArenaWallsGoldDropJSON
    , _arenaWallsMaxWidths :: [RoomArenaWallsMaxWidthJSON]

    , _speedRailAcceleration             :: Acceleration
    , _speedRailMaxSpeed                 :: Speed
    , _speedRailMaxPlayerTurnaroundSpeed :: Speed
    , _speedRailSlowSpeedThreshold       :: Speed

    , _springLauncherVelY          :: VelY
    , _springLauncherWidth         :: Float
    , _springLauncherHeight        :: Float
    , _springLauncherSurfaceWidth  :: Float
    , _springLauncherSurfaceHeight :: Float

    , _eventLightningNumWaves               :: Int
    , _eventLightningGoldValue              :: GoldValue
    , _eventLightningPerHitPenaltyGoldValue :: GoldValue

    , _eventBouncingBallAliveSecs           :: Secs
    , _eventBouncingBallDropCooldownSecs    :: Secs
    , _eventBouncingBallMinSpeed            :: Speed
    , _eventBouncingBallMaxSpeed            :: Speed
    , _eventBouncingBallDropMeleeGoldValue  :: GoldValue
    , _eventBouncingBallDropRangedGoldValue :: GoldValue
    , _eventBouncingBallLockOnReticleData   :: EnemyLockOnReticleData

    , _eventSlotMachineSelectionIntervalSecs               :: Secs
    , _eventSlotMachineSelectionActivateIntervalSecs       :: Secs
    , _eventSlotMachineSelectionActivateIntervalMultiplier :: Float
    , _eventSlotMachineSelectionMinIntervalSecs            :: Secs
    , _eventSlotMachineSelectionTextOffset                 :: Pos2
    , _eventSlotMachineSelectionOffsets                    :: NE.NonEmpty Pos2
    , _eventSlotMachineSlotsChoices                        :: NE.NonEmpty SlotsChoice

    , _enemySpawnWaves :: NE.NonEmpty EnemySpawnWaveJSON
    }
    deriving Generic

instance FromJSON LevelConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
