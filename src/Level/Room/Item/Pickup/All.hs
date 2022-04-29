module Level.Room.Item.Pickup.All
    ( chooseLoadRoomItemPickups
    , loadRoomItemHealthPickups
    , loadRoomItemGoldChunk
    , loadRoomItemEvent
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, modify)
import Data.Foldable          (foldlM)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe, listToMaybe)
import System.Random.Shuffle  (shuffleM)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S

import AppEnv
import Attack.Util
import Configs
import Configs.All.Progress
import Level.Room.Event.Types
import Level.Room.Item as RI
import Level.Room.Item.EventActivator
import Level.Room.Item.GoldChunk
import Level.Room.Item.GoldChunk.JSON
import Level.Room.Item.Pickup.All.GunItemPickups
import Level.Room.Item.Pickup.All.HealthItemPickup
import Level.Room.Item.Pickup.All.MovementSkillItemPickups
import Level.Room.Item.Pickup.All.SecondarySkillItemPickups
import Level.Room.Item.Pickup.All.UpgradeItemPickups
import Level.Room.Item.Pickup.All.WeaponItemPickups
import Level.Room.Item.Pickup.JSON
import Level.Room.JSON
import Level.Room.Types
import Player.EquipmentInfo
import Player.Gun.Types
import Player.Info
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Upgrade
import Player.Upgrade.Manager
import Player.Weapon.Types
import Stats.Manager
import Util

maxChooseWeapons        = 2 :: Int
maxChooseGuns           = 2 :: Int
maxChooseMovementSkills = 2 :: Int

transitionGoldChunkCountChoices = NE.fromList (map GoldChunkCount [1, 1, 1, 2, 2, 2, 3]) :: NE.NonEmpty GoldChunkCount

-- NOTE: this is modified from the full source since only sword is included in this repo
allInitWeaponRoomItems =
    [ (SwordWeapon, mkSwordItemPickup)
    , (GauntletsWeapon, mkSwordItemPickup)
    , (ScytheWeapon, mkSwordItemPickup)
    , (StaffWeapon, mkSwordItemPickup)
    , (SpiritBladeWeapon, mkSwordItemPickup)
    ] :: [(WeaponType, RoomType -> RoomItemInit p)]

-- NOTE: this is modified from the full source since only revolver is included in this repo
allInitGunRoomItems =
    [ (RevolverGun, mkRevolverItemPickup)
    , (ShotgunGun, mkRevolverItemPickup)
    , (ShardGun, mkRevolverItemPickup)
    , (GrenadeLauncherGun, mkRevolverItemPickup)
    , (SpikeGun, mkRevolverItemPickup)
    , (RicochetGun, mkRevolverItemPickup)
    ] :: [(GunType, RoomType -> RoomItemInit p)]

-- NOTE: this is modified from the full source since only dash is included in this repo
allInitMovementSkillRoomItems =
    [ (DashSkill, mkDashItemPickup)
    , (TeleportSkill, mkDashItemPickup)
    , (GrappleSkill, mkDashItemPickup)
    ] :: [(MovementSkillType, RoomType -> RoomItemInit p)]

-- NOTE: this is modified from the full source since only stoneForm is included in this repo
allInitSecondarySkillRoomItems =
    [ (StoneFormSkill, mkStoneFormItemPickup)
    , (FlightSkill, mkStoneFormItemPickup)
    , (FastFallSkill, mkStoneFormItemPickup)
    ] :: [(SecondarySkillType, RoomType -> RoomItemInit p)]

allInitUpgradeRoomItems =
    [ (MeterUpgradeType, mkMeterUpgradeItemPickup)
    , (DoubleJumpUpgradeType, mkDoubleJumpUpgradeItemPickup)
    , (MovementSkillUpgradeType, mkMovementSkillUpgradeItemPickup)
    ] :: [(PlayerUpgradeType, RoomType -> RoomItemInit p)]

data RoomItemChoices p = RoomItemChoices
    { _weapons         :: [RoomType -> RoomItemInit p]
    , _guns            :: [RoomType -> RoomItemInit p]
    , _movementSkills  :: [RoomType -> RoomItemInit p]
    , _secondarySkills :: [RoomType -> RoomItemInit p]
    , _upgrades        :: [RoomType -> RoomItemInit p]
    }

data AnyRoomItemChoice
    = AnyWeaponItemChoice
    | AnyGunItemChoice
    | AnyMovementSkillItemChoice
    | AnySecondarySkillItemChoice
    | AnyUpgradeItemChoice
    deriving (Bounded, Enum, Eq, Show)

allAnyRoomItemChoice = [minBound..maxBound] :: [AnyRoomItemChoice]

takeAnyRoomItemChoices
    :: MonadIO m
    => PlayerEquipmentInfo
    -> [Some RoomItem]
    -> RoomItemChoices p
    -> m (Maybe (RoomType -> RoomItemInit p), RoomItemChoices p)
takeAnyRoomItemChoices equipmentInfo chosenItems roomItemChoices =
    let
        isMaxEquipWeapons       = length (_weaponTypes equipmentInfo) >= maxEquipWeapons
        isMaxEquipGuns          = length (_gunTypes equipmentInfo) >= maxEquipGuns
        isMaxEquipMovementSkill = length (_movementSkillTypes equipmentInfo) >= maxEquipMovementSkills

        weaponChoices         = _weapons roomItemChoices
        gunChoices            = _guns roomItemChoices
        movementSkillChoices  = _movementSkills roomItemChoices
        secondarySkillChoices = _secondarySkills roomItemChoices
        upgradeChoices        = _upgrades roomItemChoices

        isWeaponChoices         = not $ null weaponChoices
        isGunChoices            = not $ null gunChoices
        isMovementSkillChoices  = not $ null movementSkillChoices
        isSecondarySkillChoices = not $ null secondarySkillChoices
        isUpgradeChoices        = not $ null upgradeChoices

        candidates = case chosenItems of
            []
                | not (isMaxEquipWeapons && isMaxEquipGuns && isMaxEquipMovementSkill) -> flip execState [] $ do
                    when (not isMaxEquipWeapons) $
                        modify $ (AnyWeaponItemChoice:)
                    when (not isMaxEquipGuns) $
                        modify $ (AnyGunItemChoice:)
                    when (not isMaxEquipMovementSkill) $
                        modify $ (AnyMovementSkillItemChoice:)

            [Some c]
                | not isMaxEquipWeapons && RI._type c == WeaponPickupItemType && isWeaponChoices                     ->
                    [AnyWeaponItemChoice]
                | not isMaxEquipGuns && RI._type c == GunPickupItemType && isGunChoices                              ->
                    [AnyGunItemChoice]
                | not isMaxEquipMovementSkill && RI._type c == MovementSkillPickupItemType && isMovementSkillChoices ->
                    [AnyMovementSkillItemChoice]

            _ -> flip execState allAnyRoomItemChoice $ do
                when (isMaxEquipWeapons || not isWeaponChoices) $
                    modify $ filter (/= AnyWeaponItemChoice)
                when (isMaxEquipGuns || not isGunChoices) $
                    modify $ filter (/= AnyGunItemChoice)
                when (isMaxEquipMovementSkill || not isMovementSkillChoices) $
                    modify $ filter (/= AnyMovementSkillItemChoice)
                when (not isSecondarySkillChoices) $
                    modify $ filter (/= AnySecondarySkillItemChoice)
                when (not isUpgradeChoices) $
                    modify $ filter (/= AnyUpgradeItemChoice)

                let
                    chosenItemTypes          = [RI._type c | Some c <- chosenItems]
                    chosenWeaponCount        = length $ filter (== WeaponPickupItemType) chosenItemTypes
                    chosenGunCount           = length $ filter (== GunPickupItemType) chosenItemTypes
                    chosenMovementSkillCount = length $ filter (== MovementSkillPickupItemType) chosenItemTypes

                when (chosenWeaponCount >= maxChooseWeapons) $
                    modify $ filter (/= AnyWeaponItemChoice)
                when (chosenGunCount >= maxChooseGuns) $
                    modify $ filter (/= AnyGunItemChoice)
                when (chosenMovementSkillCount >= maxChooseMovementSkills) $
                    modify $ filter (/= AnyMovementSkillItemChoice)
    in case candidates of
        [] -> return (Nothing, roomItemChoices)
        _  -> randomChoice (NE.fromList candidates) <&> \case
            AnyWeaponItemChoice         ->
                (listToMaybe weaponChoices, roomItemChoices {_weapons = safeTail weaponChoices})
            AnyGunItemChoice            ->
                (listToMaybe gunChoices, roomItemChoices {_guns = safeTail gunChoices})
            AnyMovementSkillItemChoice  ->
                (listToMaybe movementSkillChoices, roomItemChoices {_movementSkills = safeTail movementSkillChoices})
            AnySecondarySkillItemChoice ->
                ( listToMaybe secondarySkillChoices
                , roomItemChoices {_secondarySkills = safeTail secondarySkillChoices}
                )
            AnyUpgradeItemChoice        ->
                (listToMaybe upgradeChoices, roomItemChoices {_upgrades = safeTail upgradeChoices})

chooseLoadRoomItemPickups :: forall p. RoomJSON -> RoomType -> PlayerInfo -> StatsManager -> AppEnv p [Some RoomItem]
chooseLoadRoomItemPickups roomJSON roomType playerInfo statsManager =
    let
        equipmentInfo            = _equipment playerInfo
        equipWeaponTypes         = _weaponTypes equipmentInfo
        equipGunTypes            = _gunTypes equipmentInfo
        equipMovementSkillTypes  = _movementSkillTypes equipmentInfo
        equipSecondarySkillTypes = playerEquipmentInfoSecondarySkillTypes equipmentInfo
        isPlayerMaxMeterUpgrade  =
            M.findWithDefault 0 MeterUpgradeType (_upgradeCounts equipmentInfo) >= maxMeterUpgradeCount
    in do
        progressCfg <- _progress <$> readConfigs

        availableInitWeaponRoomItems <- shuffleM
            [ mkItem
            | (wpnType, mkItem) <- allInitWeaponRoomItems
            , wpnType `notElem` equipWeaponTypes
            , wpnType `S.member` _unlockedWeapons progressCfg
            ]

        availableInitGunRoomItems <- shuffleM
            [ mkItem
            | (gunType, mkItem) <- allInitGunRoomItems
            , gunType `notElem` equipGunTypes
            , gunType `S.member` _unlockedGuns progressCfg
            ]

        availableInitMovementSkillRoomItems <- shuffleM
            [ mkItem
            | (moveSkillType, mkItem) <- allInitMovementSkillRoomItems
            , moveSkillType `notElem` equipMovementSkillTypes
            , moveSkillType `S.member` _unlockedMovementSkills progressCfg
            ]

        availableInitSecondarySkillRoomItems <- shuffleM
            [ mkItem
            | (secondarySkillType, mkItem) <- allInitSecondarySkillRoomItems
            , secondarySkillType `notElem` equipSecondarySkillTypes
            , secondarySkillType `S.member` _unlockedSecondarySkills progressCfg
            ]

        availableInitUpgradeRoomItems <- shuffleM
            [ mkItem
            | (upgradeType, mkItem) <- allInitUpgradeRoomItems
            , not $ upgradeType == MeterUpgradeType && isPlayerMaxMeterUpgrade
            ]

        let
            chooseRoomItemInits
                :: ([Some RoomItem], RoomItemChoices p)
                -> RoomItemPickupJSON
                -> AppEnv p ([Some RoomItem], RoomItemChoices p)
            chooseRoomItemInits (chosen, choices) json = case _type (json :: RoomItemPickupJSON) of
                RandomWeaponPickup -> case _weapons choices of
                    []         -> return (chosen, choices)
                    (mkW:mkWs) -> do
                        chosen' <- (:chosen) <$> mkW roomType pos
                        return (chosen', choices {_weapons = mkWs})

                RandomGunPickup -> case _guns choices of
                    []         -> return (chosen, choices)
                    (mkG:mkGs) -> do
                        chosen' <- (:chosen) <$> mkG roomType pos
                        return (chosen', choices {_guns = mkGs})

                RandomMovementSkillPickup -> case _movementSkills choices of
                    []         -> return (chosen, choices)
                    (mkM:mkMs) -> do
                        chosen' <- (:chosen) <$> mkM roomType pos
                        return (chosen', choices {_movementSkills = mkMs})

                RandomSecondarySkillPickup -> case _secondarySkills choices of
                    []         -> return (chosen, choices)
                    (mkS:mkSs) -> do
                        chosen' <- (:chosen) <$> mkS roomType pos
                        return (chosen', choices {_secondarySkills = mkSs})

                RandomUpgradePickup -> case _upgrades choices of
                    []         -> return (chosen, choices)
                    (mkU:mkUs) -> do
                        chosen' <- (:chosen) <$> mkU roomType pos
                        return (chosen', choices {_upgrades = mkUs})

                RandomAnyPickup -> do
                    (mkItem, choices') <- takeAnyRoomItemChoices equipmentInfo chosen choices
                    case mkItem of
                        Nothing      -> return (chosen, choices')
                        Just mkItem' -> do
                            chosen' <- (:chosen) <$> mkItem' roomType pos
                            return (chosen', choices')

                HealthPickup
                    | isHealthMax (_health (playerInfo :: PlayerInfo)) ->
                        let json' = json {_type = RandomAnyPickup} :: RoomItemPickupJSON
                        in chooseRoomItemInits (chosen, choices) json'
                    | otherwise                                        -> do
                        chosen' <- (:chosen) <$> mkHealthItemPickup roomType pos statsManager
                        return (chosen', choices)

                where pos = _pos (json :: RoomItemPickupJSON)

        let
            roomItemInitChoices = RoomItemChoices
                { _weapons         = availableInitWeaponRoomItems
                , _guns            = availableInitGunRoomItems
                , _movementSkills  = availableInitMovementSkillRoomItems
                , _secondarySkills = availableInitSecondarySkillRoomItems
                , _upgrades        = availableInitUpgradeRoomItems
                }
            jsons               = fromMaybe [] (_items (roomJSON :: RoomJSON))
        fst <$> foldlM chooseRoomItemInits ([], roomItemInitChoices) jsons

roomItemPickupsCenterPos :: RoomJSON -> Maybe Pos2
roomItemPickupsCenterPos roomJSON = case _items (roomJSON :: RoomJSON) of
    Nothing    -> Nothing
    Just []    -> Nothing
    Just jsons ->
        let
            positions       = [_pos (json :: RoomItemPickupJSON) | json <- jsons]
            Pos2 xSum ySum  = foldr vecAdd zeroPos2 positions
            positionsLength = fromIntegral $ length positions
        in Just $ Pos2 (xSum / positionsLength) (ySum / positionsLength)

loadRoomItemHealthPickups :: RoomJSON -> RoomType -> StatsManager -> AppEnv p [Some RoomItem]
loadRoomItemHealthPickups roomJSON roomType statsManager = case roomItemPickupsCenterPos roomJSON of
    Nothing  -> return []
    Just pos -> pure <$> mkHealthItemPickup roomType pos statsManager

loadRoomItemGoldChunk :: RoomJSON -> AppEnv p [Some RoomItem]
loadRoomItemGoldChunk roomJSON = case roomItemPickupsCenterPos roomJSON of
    Nothing  -> return []
    Just pos -> do
        goldChunkJSON <- GoldChunkJSON pos <$> randomChoice transitionGoldChunkCountChoices
        pure <$> mkGoldChunk goldChunkJSON

loadRoomItemEvent :: RoomJSON -> RoomEventType -> AppEnv p [Some RoomItem]
loadRoomItemEvent roomJSON eventType = case roomItemPickupsCenterPos roomJSON of
    Nothing  -> return []
    Just pos -> pure <$> mkEventActivator eventType pos
