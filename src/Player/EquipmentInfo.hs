module Player.EquipmentInfo
    ( module Player.EquipmentInfo.Types
    , mkPlayerEquipmentInfo
    , mkEmptyPlayerEquipmentInfo
    , playerEquipmentInfoSecondarySkillTypes
    , playerEquipmentInfoUpgradeTypes
    , isPlayerEquipmentInfoWeaponsFull
    , isPlayerEquipmentInfoGunsFull
    , isPlayerEquipmentInfoMovementSkillFull
    , isPlayerEquipmentInfoSecondarySkillsFull
    , readPlayerEquipmentInfo
    ) where

import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)

import InfoMsg.Util
import Msg
import Player
import Player.EquipmentInfo.Types
import Player.SecondarySkill.Manager
import Player.SecondarySkill.Types
import Player.Upgrade
import Player.Upgrade.Manager

maxEquipWeapons        = 2 :: Int
maxEquipGuns           = 2 :: Int
maxEquipMovementSkills = 1 :: Int

mkPlayerEquipmentInfo :: Player -> PlayerEquipmentInfo
mkPlayerEquipmentInfo player = PlayerEquipmentInfo
    { _weaponTypes               = playerWeaponTypes player
    , _gunTypes                  = playerGunTypes player
    , _movementSkillTypes        = maybe [] pure (playerMovementSkillType player)
    , _secondarySkillNeutralType = secondarySkillManagerNeutralSlotType secondarySkillMgr
    , _secondarySkillUpType      = secondarySkillManagerUpSlotType secondarySkillMgr
    , _secondarySkillDownType    = secondarySkillManagerDownSlotType secondarySkillMgr
    , _upgradeCounts             = _counts $ _upgradeManager player
    }
    where secondarySkillMgr = _secondarySkillManager player

mkEmptyPlayerEquipmentInfo :: PlayerEquipmentInfo
mkEmptyPlayerEquipmentInfo   = PlayerEquipmentInfo
    { _weaponTypes               = []
    , _gunTypes                  = []
    , _movementSkillTypes        = []
    , _secondarySkillNeutralType = Nothing
    , _secondarySkillUpType      = Nothing
    , _secondarySkillDownType    = Nothing
    , _upgradeCounts             = M.empty
    }

playerEquipmentInfoSecondarySkillTypes :: PlayerEquipmentInfo -> [SecondarySkillType]
playerEquipmentInfoSecondarySkillTypes equipmentInfo = catMaybes
    [ _secondarySkillNeutralType equipmentInfo
    , _secondarySkillUpType equipmentInfo
    , _secondarySkillDownType equipmentInfo
    ]

playerEquipmentInfoUpgradeTypes :: PlayerEquipmentInfo -> [PlayerUpgradeType]
playerEquipmentInfoUpgradeTypes equipmentInfo = M.keys $ M.filter (> 0) (_upgradeCounts equipmentInfo)

isPlayerEquipmentInfoWeaponsFull :: PlayerEquipmentInfo -> Bool
isPlayerEquipmentInfoWeaponsFull equipmentInfo = length (_weaponTypes equipmentInfo) >= maxEquipWeapons

isPlayerEquipmentInfoGunsFull :: PlayerEquipmentInfo -> Bool
isPlayerEquipmentInfoGunsFull equipmentInfo = length (_gunTypes equipmentInfo) >= maxEquipGuns

isPlayerEquipmentInfoMovementSkillFull :: PlayerEquipmentInfo -> Bool
isPlayerEquipmentInfoMovementSkillFull equipmentInfo =
    length (_movementSkillTypes equipmentInfo) >= maxEquipMovementSkills

isPlayerEquipmentInfoSecondarySkillsFull :: PlayerEquipmentInfo -> Bool
isPlayerEquipmentInfoSecondarySkillsFull equipmentInfo = and
    [ isJust $ _secondarySkillNeutralType equipmentInfo
    , isJust $ _secondarySkillUpType equipmentInfo
    , isJust $ _secondarySkillNeutralType equipmentInfo
    ]

readPlayerEquipmentInfo :: (AllowMsgRead p InfoMsgPayload, MsgsRead p m) => m PlayerEquipmentInfo
readPlayerEquipmentInfo = processMsg <$> readMsgs
    where
        processMsg :: [InfoMsgPayload] -> PlayerEquipmentInfo
        processMsg []     = mkEmptyPlayerEquipmentInfo
        processMsg (d:ds) = case d of
            InfoMsgPlayer playerInfo -> _equipment playerInfo
            _                        -> processMsg ds
