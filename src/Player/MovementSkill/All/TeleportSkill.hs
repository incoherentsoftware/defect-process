module Player.MovementSkill.All.TeleportSkill
    ( mkTeleportSkill
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (isNothing)

import Collision.Hitbox
import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.Teleport
import Constants
import FileCache
import Msg
import Particle.All.Simple
import Player
import Player.BufferedInputState
import Player.MovementSkill as MS
import Player.MovementSkill.All.TeleportSkill.DummyCollisionProjectile
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

blinkFromEffectPath   = PackResourceFilePath "data/particles/particles.pack" "blink-from.spr" :: PackResourceFilePath
teleportSoundFilePath = "event:/SFX Events/Player/Skills/teleport"                            :: FilePath

data TeleportSkillSprites = TeleportSkillSprites
    { _teleport    :: Sprite
    , _airTeleport :: Sprite
    }

mkTeleportSkillSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m TeleportSkillSprites
mkTeleportSkillSprites =
    TeleportSkillSprites <$>
    loadPackSpr "teleport.spr" <*>
    loadPackSpr "air-teleport.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-skills.pack" f

data TeleportSkillData = TeleportSkillData
    { _blinkDirVec      :: Vec2
    , _sprites          :: TeleportSkillSprites
    , _config           :: TeleportConfig
    }

mkTeleportSkillData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m TeleportSkillData
mkTeleportSkillData = do
    sprs <- mkTeleportSkillSprites
    cfg  <- readConfig _playerSkill (_teleport :: PlayerSkillConfig -> TeleportConfig)

    return $ TeleportSkillData
        { _blinkDirVec = zeroVec2
        , _sprites     = sprs
        , _config      = cfg
        }

mkTeleportSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some MovementSkill)
mkTeleportSkill = do
    teleportSkillData <- mkTeleportSkillData
    return . Some $ (mkMovementSkill teleportSkillData TeleportSkill)
        { _think  = thinkTeleportSkill
        , _update = updateTeleportSkill
        , _draw   = drawTeleportSkill
        }

blinkMsgs :: Player -> MovementSkill TeleportSkillData -> [Msg ThinkPlayerMsgsPhase]
blinkMsgs player teleportSkill =
    -- dummy collision projectile sends the player set position message
    [ mkMsg $ NewThinkProjectileMsgAddM dummyCollisionProj
    , mkMsg $ PlayerMsgUpdateMovementSkill updateActive
    , mkMsg $ ParticleMsgAddM mkBlinkFromEffect
    ]
    where
        teleportSkillData  = _data teleportSkill
        cfg                = _config (teleportSkillData :: TeleportSkillData)
        blinkOffsetPos     = toPos2 $ _blinkDirVec teleportSkillData `vecMul` _blinkMaxDistance cfg
        playerPos          = _pos (player :: Player)
        blinkPos           = playerPos `vecAdd` blinkOffsetPos
        playerHbx          = playerHitbox player
        dummyCollisionProj = mkDummyCollisionProjectile playerHbx blinkPos
        playerCenterPos    = hitboxCenter playerHbx
        playerDir          = _dir (player :: Player)
        mkBlinkFromEffect  = loadSimpleParticle playerCenterPos playerDir worldEffectZIndex blinkFromEffectPath

        updateActive = \ms -> ms
            { _status = InactiveMovement
            , _sprite = Nothing
            , _update = deferredUpdateTeleportSkill
            }

thinkTeleportSkill :: InputRead m => MovementSkillThink TeleportSkillData m
thinkTeleportSkill canUseSkill player teleportSkill = think <$> readInputState
    where
        think :: InputState -> [Msg ThinkPlayerMsgsPhase]
        think inputState
            | movementSkillActive teleportSkill =
                let sprFinished = maybe True spriteFinished (MS._sprite teleportSkill)
                in if
                    | _gettingHit flags -> [mkMsg PlayerMsgCancelMovementSkill]
                    | not sprFinished   -> [mkMsgEx (PlayerMsgSetVelocity blinkVel) MsgAfterNormalOrder]
                    | otherwise         -> blinkMsgs player teleportSkill

            | moveSkillPressed && canUseSkill && _numCharges teleportSkill > 0 =
                let
                    aliasHold' = \alias -> alias `aliasHold` inputState

                    blinkDirVec
                        | aliasHold' UpAlias    = Vec2 0.0 (-1.0)
                        | aliasHold' DownAlias  = Vec2 0.0 1.0
                        | aliasHold' LeftAlias  = Vec2 (-1.0) 0.0
                        | aliasHold' RightAlias = Vec2 1.0 0.0
                        | otherwise             = case _dir (player :: Player) of
                            LeftDir  -> Vec2 (-1.0) 0.0
                            RightDir -> Vec2 1.0 0.0

                    sprs                        = _sprites (teleportSkillData :: TeleportSkillData)
                    spr
                        | _touchingGround flags = _teleport (sprs :: TeleportSkillSprites)
                        | otherwise             = _airTeleport sprs

                    updateActive = \ms -> ms
                        { _data             = (_data ms) {_blinkDirVec = blinkDirVec}
                        , _status           = ActiveNotCancelableMovement
                        , _sprite           = Just spr
                        , _cooldown         = _blinkCooldown $ _config (_data ms :: TeleportSkillData)
                        , _canRefreshCharge = False
                        , _numCharges       = _numCharges ms - 1
                        }

                    pos = _pos (player :: Player)
                in
                    [ mkMsgEx (PlayerMsgSetVelocity blinkVel) MsgAfterNormalOrder
                    , mkMsg $ PlayerMsgUpdateMovementSkill updateActive
                    , mkMsg PlayerMsgUsedMovementSkill
                    , mkMsg $ AudioMsgPlaySound teleportSoundFilePath pos
                    ]

            | otherwise = []

            where
                moveSkillPressed  =
                    MovementSkillAlias `aliasPressed` inputState || MovementSkillInput `inPlayerInputBuffer` player
                flags             = _flags player
                teleportSkillData = _data teleportSkill
                blinkVel          = _blinkVel $ _config (teleportSkillData :: TeleportSkillData)

-- defer update 1 frame to prevent immediately canceling teleport on the same frame it's started
deferredUpdateTeleportSkill :: Monad m => MovementSkillUpdate TeleportSkillData m
deferredUpdateTeleportSkill _ teleportSkill = return $ teleportSkill
    { _update = \_ ts -> return $ ts {_update = updateTeleportSkill}
    }

updateTeleportSkill :: Monad m => MovementSkillUpdate TeleportSkillData m
updateTeleportSkill player teleportSkill = return $ teleportSkill
    { _status           = status
    , _sprite           = spr
    , _cooldown         = cooldown
    , _numCharges       = numCharges
    , _canRefreshCharge = canRefreshCharge
    }
    where
        cooldown = max 0.0 (_cooldown teleportSkill - timeStep)
        spr      = updateSprite <$> MS._sprite teleportSkill

        canRefreshCharge
            | _touchingGround (_flags player) = True
            | otherwise                       = _canRefreshCharge teleportSkill

        numCharges
            | cooldown <= 0.0 && canRefreshCharge = playerMovementSkillMaxNumCharges player
            | otherwise                           = _numCharges teleportSkill

        status
            | movementSkillActive teleportSkill && isNothing spr = InactiveMovement
            | otherwise                                          = _status teleportSkill

drawTeleportSkill :: (GraphicsReadWrite m, MonadIO m) => MovementSkillDraw TeleportSkillData m
drawTeleportSkill player teleportSkill = case MS._sprite teleportSkill of
    Nothing  -> return ()
    Just spr -> do
        pos <- graphicsLerpPos (_pos player) (_vel player)
        drawSpriteEx pos (_dir player) playerBodyZIndex 0.0 (playerOpacity player) NonScaled spr
