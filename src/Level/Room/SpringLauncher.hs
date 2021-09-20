module Level.Room.SpringLauncher
    ( module Level.Room.SpringLauncher.Types
    , mkSpringLauncher
    , thinkSpringLauncher
    , updateSpringLauncher
    , drawSpringLauncher
    , springLauncherHitbox
    , springLauncherSurface
    , springLauncherPlayerCollision
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as L

import Collision
import Configs
import Configs.All.Level
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Id
import Level.Room.SpringLauncher.Types
import Msg
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

levelItemsPack    = \p -> PackResourceFilePath "data/levels/level-items.pack" p
idleSprPath       = levelItemsPack "spring-launcher-idle.spr"          :: PackResourceFilePath
activateSprPath   = levelItemsPack "spring-launcher-activate.spr"      :: PackResourceFilePath
idleSoundPath     = "event:/SFX Events/Level/spring-launcher-idle-c"   :: FilePath
activateSoundPath = "event:/SFX Events/Level/spring-launcher-activate" :: FilePath

activateCooldown = 0.1                :: Secs
debugHitboxColor = Color 38 127 0 200 :: Color

mkSpringLauncherSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m SpringLauncherSprites
mkSpringLauncherSprites =
    SpringLauncherSprites <$>
    loadPackSprite idleSprPath <*>
    loadPackSprite activateSprPath

mkSpringLauncher :: (ConfigsRead m, GraphicsRead m, MonadIO m, FileCache m) => Pos2 -> m SpringLauncher
mkSpringLauncher pos = do
    msgId <- newId
    cfg   <- _level <$> readConfigs
    sprs  <- mkSpringLauncherSprites

    return $ SpringLauncher
        { _msgId    = msgId
        , _pos      = pos
        , _cooldown = 0.0
        , _sprite   = _idle sprs
        , _sprites  = sprs
        , _config   = cfg
        }

thinkSpringLauncher :: SpringLauncher -> [Msg ThinkLevelMsgsPhase]
thinkSpringLauncher springLauncher = [mkMsg $ AudioMsgPlaySoundContinuous idleSoundPath hashedId pos]
    where
        hashedId = hashId $ _msgId springLauncher
        pos      = _pos springLauncher

processSpringLauncherMsgs :: MsgsRead UpdateLevelMsgsPhase m => SpringLauncher -> m SpringLauncher
processSpringLauncherMsgs springLauncher = L.foldl' processMsg springLauncher <$> readMsgsTo (_msgId springLauncher)
    where
        processMsg :: SpringLauncher -> RoomMsgPayload -> SpringLauncher
        processMsg !sl d = case d of
            RoomMsgUpdateSpringLauncher update -> update sl
            _                                  -> sl

updateSpringLauncher :: MsgsRead UpdateLevelMsgsPhase m => SpringLauncher -> m SpringLauncher
updateSpringLauncher springLauncher = processSpringLauncherMsgs springLauncher'
    where
        spr  = _sprite springLauncher
        sprs = _sprites springLauncher

        spr'
            | spr == _activate sprs && spriteFinished spr = _idle sprs
            | otherwise                                   = updateSprite spr

        springLauncher' = springLauncher
            { _cooldown = max 0.0 (_cooldown springLauncher - timeStep)
            , _sprite   = spr'
            }

drawSpringLauncher :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => SpringLauncher -> m ()
drawSpringLauncher springLauncher =
    let
        hbx = springLauncherHitbox springLauncher
        pos = hitboxBotLeft hbx
        spr = _sprite springLauncher
    in do
        drawSprite pos RightDir levelItemZIndex spr

        whenM (readSettingsConfig _debug _drawItemHitboxes) $
            drawHitbox debugHitboxColor levelItemZIndex hbx

springLauncherHitbox :: SpringLauncher -> Hitbox
springLauncherHitbox springLauncher = rectHitbox pos hbxWidth hbxHeight
    where
        Pos2 x y  = _pos (springLauncher :: SpringLauncher)
        cfg       = _config springLauncher
        hbxWidth  = _springLauncherWidth cfg
        hbxHeight = _springLauncherHeight cfg
        pos       = Pos2 (x - hbxWidth / 2.0) (y - hbxHeight)

springLauncherSurface :: SpringLauncher -> Surface
springLauncherSurface springLauncher = mkPlatformSurface (rectHitbox pos surfaceWidth surfaceHeight)
    where
        Pos2 x y           = _pos (springLauncher :: SpringLauncher)
        cfg                = _config springLauncher
        surfaceWidth       = _springLauncherSurfaceWidth cfg
        surfaceHeight      = _springLauncherSurfaceHeight cfg
        pos                = Pos2 (x - surfaceWidth / 2.0) (y - surfaceHeight)

springLauncherPlayerCollision :: SpringLauncher -> [Msg ThinkCollisionMsgsPhase]
springLauncherPlayerCollision springLauncher
    | _cooldown springLauncher > 0.0 = []
    | otherwise                      =
        let
            launchPlayer = \(Vel2 velX _) -> Vel2 velX launchVelY
            update       = \sl -> sl
                { _cooldown = activateCooldown
                , _sprite   = _activate (_sprites sl)
                }

            launchVelY = _springLauncherVelY $ _config springLauncher
            pos        = _pos (springLauncher :: SpringLauncher)
        in
            [ mkMsgEx (PlayerMsgUpdateVelocity launchPlayer) MsgEndOrder
            , mkMsgEx PlayerMsgClearAttack MsgFrontOrder
            , mkMsgEx PlayerMsgCancelMovementSkill MsgFrontOrder
            , mkMsgEx PlayerMsgResetDoubleJump MsgFrontOrder
            , mkMsgTo (RoomMsgUpdateSpringLauncher update) (_msgId springLauncher)
            , mkMsg $ AudioMsgPlaySound activateSoundPath pos
            ]
