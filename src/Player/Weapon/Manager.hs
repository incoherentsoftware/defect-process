module Player.Weapon.Manager
    ( module Player.Weapon.Manager.Types
    , mkWeaponManager
    , giveWeaponManagerWeapon
    , drawWeaponManager
    , thinkWeaponManager
    , updateWeaponManager
    ) where

import Data.Foldable    (sequenceA_)
import Data.Functor     ((<&>))
import Data.Maybe       (listToMaybe)
import Data.Traversable (for)
import qualified Data.Set as S

import AppEnv
import Attack
import Collision
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Msg
import Player.BufferedInputState
import Player.Types
import Player.Util
import Player.Weapon as W
import Player.Weapon.Manager.Types
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

weaponSwitchSoundPath = "event:/SFX Events/UI/weapon-switch" :: FilePath

debugHitboxColor = Color 0 0 255 155 :: Color

mkWeaponManager :: WeaponManager
mkWeaponManager = WeaponManager
    { _weapons = []
    }

weaponManagerActiveWeapon :: WeaponManager -> Maybe (Some Weapon)
weaponManagerActiveWeapon = listToMaybe . _weapons

giveWeaponManagerWeapon :: Some Weapon -> WeaponManager -> WeaponManager
giveWeaponManagerWeapon wpn@(Some w) weaponManager = case _weapons weaponManager of
    []                        -> weaponManager {_weapons = [wpn]}
    (Some w1:_)
        | _type w1 == wpnType -> weaponManager
    [wpn1, wpn2@(Some w2)]
        | _type w2 == wpnType -> weaponManager {_weapons = [wpn2, wpn1]}
    [wpn1]                    -> weaponManager {_weapons = [wpn, wpn1]}
    [_, wpn2]                 -> weaponManager {_weapons = [wpn, wpn2]}
    _                         -> weaponManager
    where wpnType = _type w

drawWeaponManager :: Player -> WeaponManager -> AppEnv DrawMsgsPhase ()
drawWeaponManager player weaponManager = do
    let atk = _attack player

    sequenceA_ $ do
        Some wpn <- weaponManagerActiveWeapon weaponManager
        Just $ (_drawOverlay wpn) player atk wpn

    whenM (readSettingsConfig _debug _drawEntityHitboxes) $
        let
            drawHitbox' = \hbx -> drawHitbox debugHitboxColor debugHitboxZIndex hbx
            atkHitbox   = attackHitbox =<< atk
        in do
            lerpOffset    <- playerLerpOffset player
            let atkHitbox' = moveHitbox lerpOffset <$> atkHitbox
            sequenceA_ $ drawHitbox' <$> atkHitbox'

isSwitchWeapon :: InputRead m => Player -> WeaponManager -> m Bool
isSwitchWeapon player weaponManager
    | length (_weapons weaponManager) < 2 = return False
    | otherwise                           = readInputState <&> \inputState ->
        SwitchWeaponAlias `aliasPressed` inputState || SwitchWeaponInput `inPlayerInputBuffer` player

thinkWeaponManager :: InputRead m => Player -> WeaponManager -> m [Msg ThinkPlayerMsgsPhase]
thinkWeaponManager player weaponManager = isSwitchWeapon player weaponManager <&> \case
    False -> []
    True  ->
        [ mkMsg $ AudioMsgPlaySoundCentered weaponSwitchSoundPath
        , mkMsg $ PlayerMsgClearInputBuffer (S.singleton SwitchWeaponInput)
        ]

updateWeaponManager :: Player -> WeaponManager -> AppEnv UpdatePlayerMsgsPhase WeaponManager
updateWeaponManager player weaponManager =
    let
        weapons = _weapons weaponManager
        atk     = _attack player
    in do
        weapons' <- isSwitchWeapon player weaponManager >>= \case
            True ->
                let wpns = drop 1 weapons ++ take 1 weapons
                in for wpns $ \(Some wpn) -> Some <$> (W._update wpn) WeaponUpdateBackground player atk wpn

            False -> for (zip [0..] weapons) $ \(i, Some wpn) ->
                let
                    wpnUpdateStatus
                        | i == 0    = WeaponUpdateForeground
                        | otherwise = WeaponUpdateBackground
                in Some <$> (W._update wpn) wpnUpdateStatus player atk wpn

        return $ weaponManager {_weapons = weapons'}
