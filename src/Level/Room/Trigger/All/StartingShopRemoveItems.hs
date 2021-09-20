module Level.Room.Trigger.All.StartingShopRemoveItems
    ( mkStartingShopRemoveItemsTrigger
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M

import Collision
import FileCache
import Level.Room
import Level.Room.Item.Types as RI
import Level.Room.Trigger
import Msg
import Particle.All.Simple
import Util
import World.ZIndex

itemDisappearPath
    = PackResourceFilePath "data/levels/level-items.pack" "item-pickup-disappear.spr" :: PackResourceFilePath

removableItemTypes =
    [ WeaponPickupItemType
    , GunPickupItemType
    , MovementSkillPickupItemType
    ] :: [RoomItemType]

mkStartingShopRemoveItemsTrigger :: MonadIO m => m RoomTrigger
mkStartingShopRemoveItemsTrigger = do
    trigger <- mkRoomTrigger
    return $ (trigger :: RoomTrigger) {_think = think}

think :: Monad m => RoomTriggerThink m
think room trigger = return $ if
    | _type (room :: Room) /= startingShopRoomType ->
        let triggerId = _msgId (trigger :: RoomTrigger)
        in [mkMsg $ RoomMsgRemoveTrigger triggerId]

    | otherwise ->
        let
            itemTypeCounts     = M.fromListWith (+) [(RI._type ri, 1) | Some ri <- _items room]
            itemTypesToRemove  =
                [ riType
                | riType <- removableItemTypes
                , maybe False (== 1) (riType `M.lookup` itemTypeCounts)
                ]
            removeItemTypesMsgs = map (mkMsg . RoomMsgRemoveItemType) itemTypesToRemove

            removeItemParticlesMsgs =
                [ mkMsg $ ParticleMsgAddM (loadSimpleParticle pos RightDir levelItemZIndex itemDisappearPath)
                | Some ri <- _items room
                , RI._type ri `elem` itemTypesToRemove
                , let pos = hitboxBotCenter $ RI._hitbox ri
                ]

            removeRoomPortalBarrierMsgs
                | WeaponPickupItemType `elem` itemTypesToRemove = [mkMsg RoomMsgRemovePortalBarrier]
                | otherwise                                     = []
        in removeItemTypesMsgs ++ removeItemParticlesMsgs ++ removeRoomPortalBarrierMsgs
