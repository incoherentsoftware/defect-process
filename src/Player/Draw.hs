module Player.Draw
    ( drawPlayer
    , playerLerpOffset
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)

import AppEnv
import Attack
import Collision
import Configs
import Configs.All.Player
import Configs.All.Settings
import Configs.All.Settings.Debug
import Msg.Phase
import Player
import Player.Gun.Manager
import Player.Images
import Player.LockOnAim
import Player.MovementSkill as MS
import Player.Overlay.Types as OL
import Player.Weapon.Manager
import Util
import Window
import World.ZIndex

gamepadAimLineOffsetLength = 0.0 :: Float

debugHitboxColor = Color 200 200 200 155 :: Color

drawGamepadAimLine :: (GraphicsReadWrite m, MonadIO m) => Player -> m ()
drawGamepadAimLine player =
    let
        visualAimPos@(Pos2 visualAimX visualAimY) = calculateGamepadVisualAimPos player
        shoulderPos@(Pos2 shoulderX shoulderY)    = playerShoulderPos player
        aimVec                                    = vecNormalize $ visualAimPos `vecSub` shoulderPos
        visualStartPos                            = shoulderPos `vecAdd` (aimVec `vecMul` gamepadAimLineOffsetLength)
        aimAngle                                  = atan2 (visualAimY - shoulderY) (visualAimX - shoulderX)
        gamepadAimLineImg                         = _gamepadAimLine $ _images (player :: Player)
        isGamepadAimLineEnabled                   = _isGamepadAimLineEnabled $ _config player
    in when isGamepadAimLineEnabled $
        drawImageRotated visualStartPos RightDir playerAimOverlayZIndex aimAngle gamepadAimLineImg

drawPlayer :: Player -> AppEnv DrawMsgsPhase ()
drawPlayer player
    | isPlayerInDeathAnim player   = drawSprite pos dir playerBodyZIndex spr
    | isPlayerInWarpOutAnim player = drawSprite pos dir playerBodyZIndex spr
    | otherwise                    =
        let
            drawAtkSpr :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> m ()
            drawAtkSpr lerpOffset = case _attack player of
                Nothing  -> return ()
                Just atk ->
                    let
                        atkSpr = attackSprite atk
                        atkPos = _pos (atk :: Attack) `vecAdd` lerpOffset
                        atkDir = _dir (atk :: Attack)
                    in drawSpriteRotated atkPos atkDir playerBodyZIndex (_angle atk) atkSpr

            weaponManager = _weaponManager player
            gunManager    = _gunManager player
        in do
            unlessM (readSettingsConfig _debug _hidePlayer) $ do
                lerpOffset <- playerLerpOffset player
                let pos'    = pos `vecAdd` lerpOffset

                if
                    | playerAttackActive player   -> drawAtkSpr lerpOffset
                    | gunManagerActive gunManager -> drawGunManager player gunManager
                    | otherwise                   -> case _movementSkill player of
                        Just (Some moveSkill)
                            | movementSkillActive moveSkill -> (MS._draw moveSkill) player moveSkill
                        _                                   ->
                            drawSpriteEx pos' dir playerBodyZIndex 0.0 (playerOpacity player) NonScaled spr

                whenM (readSettingsConfig _debug _drawEntityHitboxes) $
                    let hbx = moveHitbox lerpOffset (playerHitbox player)
                    in drawHitbox debugHitboxColor debugHitboxZIndex hbx

                drawWeaponManager player weaponManager
                drawGunManagerOverlay player gunManager

                case _overlay player of
                    Nothing             -> return ()
                    Just (Some overlay) -> (OL._draw overlay) player overlay

            unlessM (readSettingsConfig _debug _hideTargeting) $ do
                let lockOnAim = _lockOnAim player
                drawPlayerLockOnAim lockOnAim

                gamepadLastUsed <- (== GamepadInputType) . _lastUsedInputType <$> readInputState
                when (gamepadLastUsed && _manualOverride lockOnAim) $
                    let
                        visualAimPos    = calculateGamepadVisualAimPos player
                        aimCrosshairImg = _aimCrosshair $ _images (player :: Player)
                    in do
                        drawGamepadAimLine player
                        drawImage visualAimPos RightDir playerAimOverlayZIndex aimCrosshairImg

    where
        pos = _pos (player :: Player)
        dir = _dir (player :: Player)
        spr = _sprite (player :: Player)
