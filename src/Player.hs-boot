module Player
    ( Player
    , mkPlayer
    , playerHitbox
    , playerWallProximityHitbox
    , playerAttackHitbox
    , playerAttackId
    , playerAttackHitlag
    , playerAttackDamage
    , playerAttackStagger
    , playerAttackCancelable
    , playerAttackActive
    , playerMovementSkillActive
    , playerMovementSkillCancelable
    , playerWeaponTypes
    , playerGunTypes
    , playerMovementSkillType
    , playerSightPos
    , givePlayerWeapon
    , givePlayerGun
    ) where

import AppEnv.Types
import Attack.Types
import Attack.Util
import Collision.Hitbox.Types
import Id
import Msg.Phase
import Player.Gun.Types
import Player.MovementSkill.Types
import Player.Weapon.Types
import Util
import {-# SOURCE #-} Player.Types

mkPlayer                      :: AppEnv SetupMsgsPhase Player
playerHitbox                  :: Player -> Hitbox
playerWallProximityHitbox     :: Player -> Hitbox
playerAttackHitbox            :: Player -> Maybe Hitbox
playerAttackId                :: Player -> Maybe (Id Attack)
playerAttackHitlag            :: Player -> Maybe Secs
playerAttackDamage            :: Player -> Maybe Damage
playerAttackStagger           :: Player -> Maybe Stagger
playerAttackCancelable        :: Player -> Bool
playerAttackActive            :: Player -> Bool
playerMovementSkillActive     :: Player -> Bool
playerMovementSkillCancelable :: Player -> Bool
playerWeaponTypes             :: Player -> [WeaponType]
playerGunTypes                :: Player -> [GunType]
playerMovementSkillType       :: Player -> Maybe MovementSkillType
playerSightPos                :: Player -> Pos2
givePlayerWeapon              :: Some Weapon -> Player -> Player
givePlayerGun                 :: Some Gun -> Player -> Player
