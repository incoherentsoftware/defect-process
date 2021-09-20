{-# LANGUAGE NoStrictData #-}

module Msg.Payload
    ( IsMsgPayload(..)
    , AudioMsgPayload(..)
    , CollisionMsgPayload(..)
    , ConsoleMsgPayload(..)
    , EnemyMsgPayload(..)
    , HurtMsgPayload(..)
    , InfoMsgPayload(..)
    , MenuMsgPayload(..)
    , RoomMsgPayload(..)
    , ParticleMsgPayload(..)
    , PlayerMsgPayload(..)
    , ProjectileMsgPayload(..)
    , NewThinkProjectileMsgPayload(..)
    , NewUpdateProjectileMsgPayload(..)
    , UiMsgPayload(..)
    , WorldMsgPayload(..)
    , MsgPayload
    ) where

import Data.Typeable (Typeable)
import qualified Data.Set as S
import qualified Data.Text as T

import AppEnv.Types
import Attack.Description.Types
import Attack.Hit.Types
import Attack.Types
import Audio.Volume
import Collision.Hitbox.Types
import Enemy.Types
import Id
import InfoMsg.Util
import Level.Room.ArenaWalls.Types
import Level.Room.Item.Jukebox.Types
import Level.Room.Item.Types
import Level.Room.SpringLauncher.Types
import Level.Room.Trigger.Types
import Level.Room.Types
import Particle.Types
import Player.BufferedInputState.Types
import Player.EquipmentInfo.Types
import Player.Gun.Types
import Player.Meter
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Upgrade
import Player.Weapon.Types
import Projectile.Types
import Util
import Window.Graphics
import Window.InputState.Alias
import Window.InputState.RawData
import World.Audio.LayeredMusic.Types
import World.Screenshake.Types
import World.Surface.Types
import World.UI.Voiceover.Types
import World.Util
import {-# SOURCE #-} Enemy.Util
import {-# SOURCE #-} Msg.Types

class IsMsgPayload a where
    toMsgPayload   :: a -> MsgPayload
    fromMsgPayload :: MsgPayload -> Maybe a

data AudioMsgPayload
    = AudioMsgPlaySound FilePath Pos2
    | AudioMsgPlaySoundCentered FilePath
    | AudioMsgPlaySoundUnique FilePath HashedId Pos2
    | AudioMsgPlaySoundContinuous FilePath HashedId Pos2
    | AudioMsgMuteSound HashedId Bool
    | AudioMsgRampMusicToNormalVolume
    | AudioMsgPlayPostBattleExplorationMusic
    | AudioMsgCycleJukeboxMusic Pos2 JukeboxType

data CollisionMsgPayload
    = CollisionMsgTouchingGround PosY SurfaceType
    | CollisionMsgTouchingWall PosX PosY WallSurfaceType
    | CollisionMsgTouchingRoof PosY
    | CollisionMsgWillFallOffGround
    | CollisionMsgMovingPlatform Hitbox Hitbox
    | CollisionMsgWallProximity OffsetX WallSurfaceType

data ConsoleMsgPayload
    = ConsoleMsgPrint T.Text
    | ConsoleMsgPrintString String
    | ConsoleMsgSetControlsInputAlias InputAlias [InputRawData] (Maybe InputRawData)
    | ConsoleMsgSetGraphicsResolution Int Int
    | ConsoleMsgSetGraphicsWindowMode WindowMode
    | ConsoleMsgUpdateRenderConfigWinDisplayIndex
    | ConsoleMsgSetSoundVolume Volume
    | ConsoleMsgSetMusicVolume Volume
    | ConsoleMsgSetBattleMusic LayeredMusicType
    | ConsoleMsgSetExplorationMusic LayeredMusicType
    | ConsoleMsgAddProgressTotalGold GoldValue
    | ConsoleMsgUnlockWeapon GoldValue WeaponType
    | ConsoleMsgUnlockGun GoldValue GunType
    | ConsoleMsgUnlockMovementSkill GoldValue MovementSkillType
    | ConsoleMsgUnlockSecondarySkill GoldValue SecondarySkillType
    | ConsoleMsgUnlockMusic GoldValue LayeredMusicType
    | ConsoleMsgRestoreDefaultSettingsControls
    | ConsoleMsgRestoreDefaultSettingsRender
    | ConsoleMsgRestoreDefaultSettingsAudio
    | ConsoleMsgSaveSettings
    | ConsoleMsgSaveProgress

data EnemyMsgPayload where
    EnemyMsgAdds           :: [Some Enemy] -> EnemyMsgPayload
    EnemyMsgAddM           :: AppEnv UpdateEnemyMsgsPhase (Some Enemy) -> EnemyMsgPayload
    EnemyMsgAddsM          :: AppEnv UpdateEnemyMsgsPhase [Some Enemy] -> EnemyMsgPayload
    EnemyMsgUpdate         :: Typeable d => (Enemy d -> Enemy d) -> EnemyMsgPayload
    EnemyMsgUpdateM        :: Typeable d => (Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)) -> EnemyMsgPayload
    EnemyMsgSetVelocity    :: Vel2 -> EnemyMsgPayload
    EnemyMsgUpdateVelocity :: (Vel2 -> Vel2) -> EnemyMsgPayload
    EnemyMsgSetDirection   :: Direction -> EnemyMsgPayload
    EnemyMsgClearAttack    :: EnemyMsgPayload
    EnemyMsgSetAttackDesc  :: AttackDescription -> EnemyMsgPayload
    EnemyMsgSetAttackDescM :: AppEnv UpdateEnemyMsgsPhase AttackDescription -> EnemyMsgPayload
    EnemyMsgSetDead        :: EnemyMsgPayload
    EnemyMsgSetHangtime    :: Secs -> EnemyMsgPayload

data HurtMsgPayload
    = HurtMsgAttackHit AttackHit

data InfoMsgPayload
    = InfoMsgPlayer PlayerInfo
    | InfoMsgSeenPlayer (Maybe PlayerInfo)
    | InfoMsgProjectilePos Pos2 MsgId MsgId
    | InfoMsgEnemyPos Hitbox MsgId
    | InfoMsgEnemyLockOnReticle EnemyLockOnData
    | InfoMsgRoomArenaWalls RoomArenaWallsInfo
    | InfoMsgRoomTopBounds PosY
    | InfoMsgBattleMusic LayeredMusicType
    | InfoMsgExplorationMusic LayeredMusicType

data MenuMsgPayload
    = MenuMsgControlsShowNotification T.Text
    | MenuMsgControlsToggleView
    | MenuMsgUnlocksInsufficientGold
    | MenuMsgShowUnlockOverlay Pos2

data RoomMsgPayload where
    RoomMsgAddTrigger              :: RoomTrigger -> RoomMsgPayload
    RoomMsgAddTriggerM             :: AppEnv UpdateLevelMsgsPhase RoomTrigger -> RoomMsgPayload
    RoomMsgRemoveTrigger           :: MsgId -> RoomMsgPayload
    RoomMsgRemoveItem              :: MsgId -> RoomMsgPayload
    RoomMsgRemoveItemType          :: RoomItemType -> RoomMsgPayload
    RoomMsgReappearItem            :: MsgId -> RoomMsgPayload
    RoomMsgShowPickupItemIndicator :: RoomMsgPayload
    RoomMsgUpdateTrigger           :: (RoomTrigger -> RoomTrigger) -> RoomMsgPayload
    RoomMsgUpdateItem              :: Typeable d => (RoomItem d -> RoomItem d) -> RoomMsgPayload
    RoomMsgUpdateSpringLauncher    :: (SpringLauncher -> SpringLauncher) -> RoomMsgPayload
    RoomMsgUpdateArenaWalls        :: (RoomArenaWalls -> RoomArenaWalls) -> RoomMsgPayload
    RoomMsgArenaWallsSplat         :: Pos2 -> RoomMsgPayload
    RoomMsgAddPortalBarrier        :: RoomMsgPayload
    RoomMsgRemovePortalBarrier     :: RoomMsgPayload

data ParticleMsgPayload
    = ParticleMsgAdd (Some Particle)
    | ParticleMsgAddM (AppEnv UpdateParticleMsgsPhase (Some Particle))
    | ParticleMsgAddsM (AppEnv UpdateParticleMsgsPhase [Some Particle])

data PlayerMsgPayload where
    PlayerMsgSetVelocity                 :: Vel2 -> PlayerMsgPayload
    PlayerMsgUpdateVelocity              :: (Vel2 -> Vel2) -> PlayerMsgPayload
    PlayerMsgSetDirection                :: Direction -> PlayerMsgPayload
    PlayerMsgSetPosition                 :: Pos2 -> PlayerMsgPayload
    PlayerMsgUpdatePosition              :: (Pos2 -> Pos2) -> PlayerMsgPayload
    PlayerMsgPushbackOffset              :: OffsetX -> PlayerMsgPayload
    PlayerMsgUsedMovementSkill           :: PlayerMsgPayload
    PlayerMsgUpdateMovementSkill         :: Typeable d => (MovementSkill d -> MovementSkill d) -> PlayerMsgPayload
    PlayerMsgCancelMovementSkill         :: PlayerMsgPayload
    PlayerMsgUpdateSecondarySkill
        :: Typeable d
        => SecondarySkillSlot
        -> (SecondarySkill d -> SecondarySkill d)
        -> PlayerMsgPayload
    PlayerMsgFiredGun                    :: PlayerMsgPayload
    PlayerMsgGiveWeapon                  :: Some Weapon -> PlayerMsgPayload
    PlayerMsgUpdateGun                   :: Typeable d => (Gun d -> Gun d) -> PlayerMsgPayload
    PlayerMsgUpdateWeapon                :: Typeable d => (Weapon d -> Weapon d) -> PlayerMsgPayload
    PlayerMsgSetPhased                   :: PlayerMsgPayload
    PlayerMsgTouchingGold                :: GoldValue -> PlayerMsgPayload
    PlayerMsgClearAttack                 :: PlayerMsgPayload
    PlayerMsgSetAttack                   :: Attack -> PlayerMsgPayload
    PlayerMsgSetAttackDesc               :: AttackDescription -> PlayerMsgPayload
    PlayerMsgSetAttackDescEx             :: Pos2 -> Direction -> AttackDescription -> PlayerMsgPayload
    PlayerMsgUpdateAttack                :: (Attack -> Attack) -> PlayerMsgPayload
    PlayerMsgUpdateAttackM               :: (Attack -> AppEnv UpdatePlayerMsgsPhase Attack) -> PlayerMsgPayload
    PlayerMsgInteract                    :: GoldValue -> PlayerMsgPayload
    PlayerMsgBuyWeapon                   :: Some Weapon -> GoldValue -> PlayerMsgPayload
    PlayerMsgBuyGun                      :: Some Gun -> GoldValue -> PlayerMsgPayload
    PlayerMsgBuyMovementSkill            :: Some MovementSkill -> GoldValue -> PlayerMsgPayload
    PlayerMsgBuySecondarySkill           :: Some SecondarySkill -> GoldValue -> PlayerMsgPayload
    PlayerMsgBuyUpgrade                  :: PlayerUpgradeType -> GoldValue -> PlayerMsgPayload
    PlayerMsgBuyHealth                   :: GoldValue -> PlayerMsgPayload
    PlayerMsgClearInputBuffer            :: S.Set PlayerInput -> PlayerMsgPayload
    PlayerMsgGainMeter                   :: MeterValue -> PlayerMsgPayload
    PlayerMsgSpendMeter                  :: MeterValue -> PlayerMsgPayload
    PlayerMsgFillMeterFull               :: PlayerMsgPayload
    PlayerMsgResetDoubleJump             :: PlayerMsgPayload
    PlayerMsgResetAirStallAttacksCounter :: PlayerMsgPayload
    PlayerMsgForceInAir                  :: PlayerMsgPayload
    PlayerMsgWarpOut                     :: PlayerMsgPayload
    PlayerMsgTouchingInfoSign            :: PlayerMsgPayload

data ProjectileMsgPayload where
    ProjectileMsgSetVelocity     :: Vel2 -> ProjectileMsgPayload
    ProjectileMsgSetHitbox       :: Hitbox -> ProjectileMsgPayload
    ProjectileMsgSetTtl          :: Secs -> ProjectileMsgPayload
    ProjectileMsgRemoveCollision :: ProjectileMsgPayload
    ProjectileMsgRemoveThink     :: ProjectileMsgPayload
    ProjectileMsgRemoveUpdate    :: ProjectileMsgPayload
    ProjectileMsgUpdate          :: Typeable d => (Projectile d -> Projectile d) -> ProjectileMsgPayload
    ProjectileMsgUpdateM
        :: Typeable d
        => (Projectile d -> AppEnv UpdateProjectileMsgsPhase (Projectile d))
        -> ProjectileMsgPayload

data NewThinkProjectileMsgPayload where
    NewThinkProjectileMsgAdd   :: Some Projectile -> NewThinkProjectileMsgPayload
    NewThinkProjectileMsgAddM  :: AppEnv ThinkProjectileMsgsPhase (Some Projectile) -> NewThinkProjectileMsgPayload
    NewThinkProjectileMsgAddsM :: AppEnv ThinkProjectileMsgsPhase [Some Projectile] -> NewThinkProjectileMsgPayload

data NewUpdateProjectileMsgPayload where
    NewUpdateProjectileMsgAdd   :: Some Projectile -> NewUpdateProjectileMsgPayload
    NewUpdateProjectileMsgAddM  :: AppEnv UpdateProjectileMsgsPhase (Some Projectile) -> NewUpdateProjectileMsgPayload
    NewUpdateProjectileMsgAddsM :: AppEnv UpdateProjectileMsgsPhase [Some Projectile] -> NewUpdateProjectileMsgPayload

data UiMsgPayload
    = UiMsgFullRefillMeter
    | UiMsgInsufficientMeter MeterValue Bool
    | UiMsgInsufficientGold
    | UiMsgInvalidAction InputAlias
    | UiMsgInvalidActionEx InputAlias InputAlias
    | UiMsgShowVoiceoverText (VoiceoverUI -> DisplayText)
    | UiMsgShowMoveControls
    | UiMsgShowWeaponEquipmentInfo Int
    | UiMsgShowGunEquipmentInfo Int
    | UiMsgShowSecondarySkillEquipmentInfo PlayerEquipmentInfo
    | UiMsgShowGeneralEquipmentInfo

data WorldMsgPayload
    = WorldMsgSwitchRoom RoomType PosY
    | WorldMsgHitlag Secs
    | WorldMsgScreenshake ScreenshakeMagnitude
    | WorldMsgLockCamera
    | WorldMsgPause
    | WorldMsgDeactivate

instance IsMsgPayload AudioMsgPayload where
    toMsgPayload :: AudioMsgPayload -> MsgPayload
    toMsgPayload = AudioMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe AudioMsgPayload
    fromMsgPayload = \case
        AudioMsgPayload' d -> Just d
        _                  -> Nothing

instance IsMsgPayload CollisionMsgPayload where
    toMsgPayload :: CollisionMsgPayload -> MsgPayload
    toMsgPayload = CollisionMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe CollisionMsgPayload
    fromMsgPayload = \case
        CollisionMsgPayload' d -> Just d
        _                      -> Nothing

instance IsMsgPayload ConsoleMsgPayload where
    toMsgPayload :: ConsoleMsgPayload -> MsgPayload
    toMsgPayload = ConsoleMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe ConsoleMsgPayload
    fromMsgPayload = \case
        ConsoleMsgPayload' d -> Just d
        _                    -> Nothing

instance IsMsgPayload EnemyMsgPayload where
    toMsgPayload :: EnemyMsgPayload -> MsgPayload
    toMsgPayload = EnemyMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe EnemyMsgPayload
    fromMsgPayload = \case
        EnemyMsgPayload' d -> Just d
        _                  -> Nothing

instance IsMsgPayload HurtMsgPayload where
    toMsgPayload :: HurtMsgPayload -> MsgPayload
    toMsgPayload = HurtMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe HurtMsgPayload
    fromMsgPayload = \case
        HurtMsgPayload' d -> Just d
        _                 -> Nothing

instance IsMsgPayload InfoMsgPayload where
    toMsgPayload :: InfoMsgPayload -> MsgPayload
    toMsgPayload = InfoMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe InfoMsgPayload
    fromMsgPayload = \case
        InfoMsgPayload' d -> Just d
        _               -> Nothing

instance IsMsgPayload MenuMsgPayload where
    toMsgPayload :: MenuMsgPayload -> MsgPayload
    toMsgPayload = MenuMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe MenuMsgPayload
    fromMsgPayload = \case
        MenuMsgPayload' d -> Just d
        _                 -> Nothing

instance IsMsgPayload RoomMsgPayload where
    toMsgPayload :: RoomMsgPayload -> MsgPayload
    toMsgPayload = RoomMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe RoomMsgPayload
    fromMsgPayload = \case
        RoomMsgPayload' d -> Just d
        _                 -> Nothing

instance IsMsgPayload ParticleMsgPayload where
    toMsgPayload :: ParticleMsgPayload -> MsgPayload
    toMsgPayload = ParticleMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe ParticleMsgPayload
    fromMsgPayload = \case
        ParticleMsgPayload' d -> Just d
        _                     -> Nothing

instance IsMsgPayload PlayerMsgPayload where
    toMsgPayload :: PlayerMsgPayload -> MsgPayload
    toMsgPayload = PlayerMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe PlayerMsgPayload
    fromMsgPayload = \case
        PlayerMsgPayload' d -> Just d
        _                   -> Nothing

instance IsMsgPayload ProjectileMsgPayload where
    toMsgPayload :: ProjectileMsgPayload -> MsgPayload
    toMsgPayload = ProjectileMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe ProjectileMsgPayload
    fromMsgPayload = \case
        ProjectileMsgPayload' d -> Just d
        _                       -> Nothing

instance IsMsgPayload NewThinkProjectileMsgPayload where
    toMsgPayload :: NewThinkProjectileMsgPayload -> MsgPayload
    toMsgPayload = NewThinkProjectileMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe NewThinkProjectileMsgPayload
    fromMsgPayload = \case
        NewThinkProjectileMsgPayload' d -> Just d
        _                               -> Nothing

instance IsMsgPayload NewUpdateProjectileMsgPayload where
    toMsgPayload :: NewUpdateProjectileMsgPayload -> MsgPayload
    toMsgPayload = NewUpdateProjectileMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe NewUpdateProjectileMsgPayload
    fromMsgPayload = \case
        NewUpdateProjectileMsgPayload' d -> Just d
        _                                -> Nothing

instance IsMsgPayload UiMsgPayload where
    toMsgPayload :: UiMsgPayload -> MsgPayload
    toMsgPayload = UiMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe UiMsgPayload
    fromMsgPayload = \case
        UiMsgPayload' d -> Just d
        _               -> Nothing

instance IsMsgPayload WorldMsgPayload where
    toMsgPayload :: WorldMsgPayload -> MsgPayload
    toMsgPayload = WorldMsgPayload'

    fromMsgPayload :: MsgPayload -> Maybe WorldMsgPayload
    fromMsgPayload = \case
        WorldMsgPayload' d -> Just d
        _                  -> Nothing

data MsgPayload
    = AudioMsgPayload' AudioMsgPayload
    | CollisionMsgPayload' CollisionMsgPayload
    | ConsoleMsgPayload' ConsoleMsgPayload
    | EnemyMsgPayload' EnemyMsgPayload
    | HurtMsgPayload' HurtMsgPayload
    | InfoMsgPayload' InfoMsgPayload
    | MenuMsgPayload' MenuMsgPayload
    | RoomMsgPayload' RoomMsgPayload
    | ParticleMsgPayload' ParticleMsgPayload
    | PlayerMsgPayload' PlayerMsgPayload
    | ProjectileMsgPayload' ProjectileMsgPayload
    | NewThinkProjectileMsgPayload' NewThinkProjectileMsgPayload
    | NewUpdateProjectileMsgPayload' NewUpdateProjectileMsgPayload
    | UiMsgPayload' UiMsgPayload
    | WorldMsgPayload' WorldMsgPayload
