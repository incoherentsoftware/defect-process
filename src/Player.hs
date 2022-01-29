module Player
    ( module Player.Types
    , module Player.Flags
    , module Player.Util
    , mkPlayer
    , playerHitbox
    , playerWallProximityHitbox
    , playerAttackHitbox
    , playerAttackVel
    , playerAttackId
    , playerAttackHitlag
    , playerAttackDamage
    , playerAttackStagger
    , playerAttackCancelable
    , playerAttackWalkCancelable
    , playerAttackActive
    , playerAttackSprite
    , playerMovementSkillType
    , playerMovementSkillActive
    , playerMovementSkillCancelable
    , playerMovementSkillWalkCancelable
    , playerMovementSkillNumCharges
    , playerMovementSkillMaxNumCharges
    , playerMovementSkillSprite
    , playerSecondarySkillType
    , playerSecondarySkillOnCooldown
    , playerUpgradeCount
    , playerWeaponTypes
    , playerGunTypes
    , playerGunFireDrawSprites
    , playerSightPos
    , playerGravityVel
    , playerAntiGravityVel
    , givePlayerWeapon
    , givePlayerGun
    , givePlayerUpgrade
    ) where

import Data.Functor ((<&>))
import Data.Maybe   (isJust)
import qualified Data.Set as S

import AppEnv
import Attack
import Collision
import Configs
import Configs.All.Player
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Id
import Msg.Phase
import Player.BufferedInputState
import Player.Flags
import Player.Gun as G
import Player.Gun.Manager
import Player.Images
import Player.LockOnAim
import Player.Meter
import Player.Momentum
import Player.MovementSkill as MS
import Player.Overlay.All
import Player.SecondarySkill as SS
import Player.SecondarySkill.Manager
import Player.SoundHashedIds
import Player.Sprites
import Player.TimersCounters
import Player.Types
import Player.Upgrade
import Player.Upgrade.Manager
import Player.Util
import Player.Weapon as W
import Player.Weapon.Manager
import Util
import Window.Graphics

initialHealth       = mkHealth 100 :: Health
wallProximityOffset = 5.0          :: Distance

mkPlayer :: AppEnv SetupMsgsPhase Player
mkPlayer = do
    playerId           <- newId
    bufferedInputState <- mkPlayerBufferedInputState
    lockOnAim          <- mkPlayerLockOnAim
    gunManager         <- mkGunManager
    images             <- mkPlayerImages
    sprites            <- mkPlayerSprites
    overlays           <- mkPlayerOverlays
    soundHashedIds     <- mkPlayerSoundHashedIds
    cfg                <- _player <$> readConfigs

    startingSpr <- readSettingsConfig _debug _skipPlayerSpawnAnim <&> \case
        False -> _spawn sprites
        True  -> _idle sprites

    return $ Player
        { _msgId                 = playerId
        , _pos                   = zeroPos2
        , _vel                   = Vel2 0.0 (_gravity cfg * timeStep)
        , _dir                   = RightDir
        , _prevHitbox            = dummyHitbox zeroPos2
        , _momentum              = mkPlayerMomentum
        , _sprite                = startingSpr
        , _attack                = Nothing
        , _health                = initialHealth
        , _meter                 = mkPlayerMeter
        , _gold                  = _initialGold cfg
        , _aimPos                = zeroPos2
        , _lockOnAim             = lockOnAim
        , _bufferedInputState    = bufferedInputState
        , _weaponManager         = mkWeaponManager
        , _gunManager            = gunManager
        , _movementSkill         = Nothing
        , _secondarySkillManager = mkSecondarySkillManager
        , _upgradeManager        = mkPlayerUpgradeManager
        , _hitByHashedIds        = S.empty
        , _flags                 = mkPlayerFlags
        , _timersCounters        = mkPlayerTimersCounters
        , _overlay               = Nothing
        , _images                = images
        , _sprites               = sprites
        , _overlays              = overlays
        , _soundHashedIds        = soundHashedIds
        , _config                = cfg
        }

playerHitbox :: Player -> Hitbox
playerHitbox player
    | isPlayerInDeathAnim player = dummyHitbox pos
    | otherwise                  = rectHitbox pos w h
    where
        Pos2 x y  = _pos (player :: Player)
        playerCfg = _config (player :: Player)
        w         = _width (playerCfg :: PlayerConfig)
        h         = _height (playerCfg :: PlayerConfig)
        pos       = Pos2 (x - w / 2.0) (y - h)

playerWallProximityHitbox :: Player -> Hitbox
playerWallProximityHitbox player = case playerHitbox player of
    dummyHbx@(DummyHitbox _) -> dummyHbx
    hbx                      ->
        let
            Pos2 x y = hitboxTopLeft hbx
            pos      = Pos2 (x - wallProximityOffset) (y - wallProximityOffset)
            width    = hitboxWidth hbx + 2.0 * wallProximityOffset
            height   = hitboxHeight hbx - 2.0 * wallProximityOffset
        in rectHitbox pos width height

playerAttackHitbox :: Player -> Maybe Hitbox
playerAttackHitbox = (attackHitbox =<<) . _attack

playerAttackVel :: Player -> Maybe AttackVel
playerAttackVel = (attackVel <$>) . _attack

playerAttackId :: Player -> Maybe (Id Attack)
playerAttackId = fmap (_id :: Attack -> Id Attack) . _attack

playerAttackHitlag :: Player -> Maybe Secs
playerAttackHitlag = (attackHitlag <$>) . _attack

playerAttackDamage :: Player -> Maybe Damage
playerAttackDamage = (attackDamage <$>) . _attack

playerAttackStagger :: Player -> Maybe Stagger
playerAttackStagger = (attackStagger <$>) . _attack

playerAttackCancelable :: Player -> Bool
playerAttackCancelable player = maybe True attackCancelable (_attack player)

playerAttackWalkCancelable :: Player -> Bool
playerAttackWalkCancelable player = maybe True attackWalkCancelable (_attack player)

playerMovementSkillType :: Player -> Maybe MovementSkillType
playerMovementSkillType player = _movementSkill player <&> \(Some ms) -> MS._type ms

playerMovementSkillActive :: Player -> Bool
playerMovementSkillActive player = case _movementSkill player of
    Just (Some ms) -> movementSkillActive ms
    Nothing        -> False

playerMovementSkillCancelable :: Player -> Bool
playerMovementSkillCancelable player = case _movementSkill player of
    Just (Some ms) -> movementSkillCancelable ms
    Nothing        -> True

playerMovementSkillWalkCancelable :: Player -> Bool
playerMovementSkillWalkCancelable player = case _movementSkill player of
    Just (Some ms) -> movementSkillWalkCancelable ms
    Nothing        -> True

playerMovementSkillNumCharges :: Player -> Int
playerMovementSkillNumCharges player = case _movementSkill player of
    Just (Some ms) -> MS._numCharges ms
    Nothing        -> 0

playerMovementSkillMaxNumCharges :: Player -> Int
playerMovementSkillMaxNumCharges player = case _movementSkill player of
    Just _  -> playerUpgradeCount MovementSkillUpgradeType player + 1
    Nothing -> 0

playerMovementSkillSprite :: Player -> Maybe Sprite
playerMovementSkillSprite player = case _movementSkill player of
    Just (Some ms)
        | movementSkillActive ms -> MS._sprite ms
    _                            -> Nothing

playerSecondarySkillType :: SecondarySkillSlot -> Player -> Maybe SecondarySkillType
playerSecondarySkillType slot player = slotF (_secondarySkillManager player) <&> \(Some ss) -> SS._type ss
    where
        slotF = case slot of
            SecondarySkillNeutralSlot -> _neutralSlot
            SecondarySkillUpSlot      -> _upSlot
            SecondarySkillDownSlot    -> _downSlot

playerSecondarySkillOnCooldown :: SecondarySkillSlot -> Player -> Bool
playerSecondarySkillOnCooldown slot player = case slotF (_secondarySkillManager player) of
    Just (Some ss) -> (SS._onCooldown ss) ss
    Nothing        -> False
    where
        slotF = case slot of
            SecondarySkillNeutralSlot -> _neutralSlot
            SecondarySkillUpSlot      -> _upSlot
            SecondarySkillDownSlot    -> _downSlot

playerUpgradeCount :: PlayerUpgradeType -> Player -> Int
playerUpgradeCount upgradeType player = playerUpgradeManagerCount upgradeType (_upgradeManager player)

playerAttackActive :: Player -> Bool
playerAttackActive = isJust . _attack

playerAttackSprite :: Player -> Maybe Sprite
playerAttackSprite = (attackSprite <$>) . _attack

playerWeaponTypes :: Player -> [WeaponType]
playerWeaponTypes player = [W._type w | Some w <- _weapons (_weaponManager player)]

playerGunTypes :: Player -> [GunType]
playerGunTypes player = [G._type g | Some g <- _guns (_gunManager player)]

playerGunFireDrawSprites :: Player -> Maybe GunFireDrawSprites
playerGunFireDrawSprites = _gunFireDrawSprites . _fireDrawState . _gunManager

playerSightPos :: Player -> Pos2
playerSightPos player = Pos2 (x + w / 2.0) y
    where
        hitbox   = playerHitbox player
        Pos2 x y = hitboxTopLeft hitbox
        w        = hitboxWidth hitbox

playerGravityVel :: Player -> Vel2
playerGravityVel player = Vel2 0.0 (gravity * timeStep)
    where gravity = _gravity $ _config (player :: Player)

playerAntiGravityVel :: Player -> Vel2
playerAntiGravityVel player = playerGravityVel player `vecMul` (-1.0)

givePlayerWeapon :: Some Weapon -> Player -> Player
givePlayerWeapon wpn player = player {_weaponManager = weaponMgr}
    where weaponMgr = giveWeaponManagerWeapon wpn (_weaponManager player)

givePlayerGun :: Some Gun -> Player -> Player
givePlayerGun gun player = player {_gunManager = gunMgr}
    where gunMgr = giveGunManagerGun gun (_gunManager player)

givePlayerUpgrade :: PlayerUpgradeType -> Player -> Player
givePlayerUpgrade upgradeType player = player {_upgradeManager = upgradeMgr}
    where upgradeMgr = givePlayerUpgradeManagerUpgrade upgradeType (_upgradeManager player)
