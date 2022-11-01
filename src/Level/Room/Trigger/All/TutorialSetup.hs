module Level.Room.Trigger.All.TutorialSetup
    ( mkTutorialSetupTrigger
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))

import Enemy.Types
import Level.Room.ArenaWalls.EnemySpawn
import Level.Room.Item.Pickup.All.GunItemPickups
import Level.Room.Item.RefreshStation
import Level.Room.Trigger
import Level.Room.Trigger.All.TutorialInstructions
import Level.Room.Trigger.All.TutorialListener
import Level.Room.Trigger.Util
import Level.Room.Tutorial.SandbagAir
import Level.Room.Tutorial.SandbagGround
import Level.Room.Util
import Msg
import Player.EquipmentInfo
import Util

freeRevolverPickupPos = Pos2 967.0 3370.0                                               :: Pos2
refreshStationPos     = Pos2 563.0 3370.0                                               :: Pos2
sandbagGroundPos      = Pos2 1239.0 3372.0                                              :: Pos2
sandbagAirPos         = roomArenaWallsEnemySpawnOffset (Pos2 1553.0 3370.0) FlyingEnemy :: Pos2

mkTutorialSetupTrigger :: MonadIO m => m RoomTrigger
mkTutorialSetupTrigger = do
    trigger <- mkRoomTrigger
    return $ (trigger :: RoomTrigger) {_think = thinkSpawnNeededEquipment}

readIsPlayerGunTypesEmpty :: MsgsRead ThinkLevelMsgsPhase m => m Bool
readIsPlayerGunTypesEmpty = null . _gunTypes <$> readPlayerEquipmentInfo

thinkSpawnNeededEquipment :: (MonadIO m, MsgsRead ThinkLevelMsgsPhase m) => RoomTriggerThink m
thinkSpawnNeededEquipment _ trigger = readIsPlayerGunTypesEmpty <&> \case
    False -> [updateTriggerThinkMessage (thinkFinishSetup False) trigger]
    True  ->
        [ mkMsg $ RoomMsgAddItemM (mkFreeRevolverItemPickup tutorialRoomType freeRevolverPickupPos)
        , updateTriggerThinkMessage (thinkFinishSetup True) trigger
        ]

thinkFinishSetup :: (MonadIO m, MsgsRead ThinkLevelMsgsPhase m) => Bool -> RoomTriggerThink m
thinkFinishSetup isGivenFreeRevolver _ trigger = readIsPlayerGunTypesEmpty <&> \case
    True  -> []
    False ->
        [ mkMsg $ RoomMsgAddItemM (mkRefreshStation refreshStationPos)
        , mkMsg $ EnemyMsgAddM (mkSandbagGround sandbagGroundPos LeftDir)
        , mkMsg $ EnemyMsgAddM (mkSandbagAir sandbagAirPos LeftDir)
        , mkMsg $ RoomMsgAddTriggerM (mkTutorialInstructionsTrigger isGivenFreeRevolver)
        , mkMsg $ RoomMsgAddTriggerM mkTutorialListenerTrigger
        , removeTriggerMessage trigger
        ]
