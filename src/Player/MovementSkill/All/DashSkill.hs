module Player.MovementSkill.All.DashSkill
    ( mkDashSkill
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.Dash
import Constants
import FileCache
import Msg
import Particle.All.Simple
import Player
import Player.BufferedInputState
import Player.Momentum
import Player.MovementSkill as MS
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

movementSkillsPath = "data/player/player-skills.pack"                              :: FilePath
dashEffectPath     = PackResourceFilePath movementSkillsPath "dash-effect.spr"     :: PackResourceFilePath
dashAirEffectPath  = PackResourceFilePath movementSkillsPath "dash-air-effect.spr" :: PackResourceFilePath
dashSoundFilePath  = "event:/SFX Events/Player/Skills/dash"                        :: FilePath

cancelableFrameTagName = FrameTagName "cancelable" :: FrameTagName

data DashSkillSprites = DashSkillSprites
    { _dashSprite :: Sprite
    }

mkDashSkillSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m DashSkillSprites
mkDashSkillSprites =
    DashSkillSprites <$> loadPackSpr "dash.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath movementSkillsPath f

data DashSkillData = DashSkillData
    { _sprites :: DashSkillSprites
    , _config  :: DashConfig
    }

mkDashSkillData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m DashSkillData
mkDashSkillData = do
    dashSkillSprites <- mkDashSkillSprites
    cfg              <- readConfig _playerSkill _dash

    return $ DashSkillData
        { _sprites = dashSkillSprites
        , _config  = cfg
        }

mkDashSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some MovementSkill)
mkDashSkill = do
    dashSkillData <- mkDashSkillData
    return . Some $ (mkMovementSkill dashSkillData DashSkill)
        { _think  = thinkDashSkill
        , _update = updateDashSkill
        , _draw   = drawDashSkill
        }

thinkDashSkill :: InputRead m => MovementSkillThink DashSkillData m
thinkDashSkill canUseSkill player dashSkill = think <$> readInputState
    where
        think :: InputState -> [Msg ThinkPlayerMsgsPhase]
        think inputState
            | active = if
                | inAir && sprWillFinish  -> [mkMsgEx (PlayerMsgSetVelocity zeroVel2) MsgAfterNormalOrder]
                | not (_gettingHit flags) -> [mkMsgEx (PlayerMsgSetVelocity dashVel) MsgAfterNormalOrder]
                | otherwise               -> [mkMsg PlayerMsgCancelMovementSkill]

            | moveSkillPressed && canUseSkill && _numCharges dashSkill > 0 =
                let
                    updateActive = \ms -> ms
                        { _status           = ActiveNotCancelableMovement
                        , _sprite           = Just . _dashSprite $ _sprites (_data ms :: DashSkillData)
                        , _cooldown         = _dashCooldown cfg
                        , _canRefreshCharge = False
                        , _numCharges       = _numCharges ms - 1
                        }

                    effectPath        = if inAir then dashAirEffectPath else dashEffectPath
                    mkEffectsParticle = loadSimpleParticle pos dir playerMovementSkillOverlayZIndex effectPath
                in
                    [ mkMsgEx (PlayerMsgSetDirection dir) MsgAfterNormalOrder
                    , mkMsgEx (PlayerMsgSetVelocity dashVel) MsgAfterNormalOrder
                    , mkMsg $ PlayerMsgUpdateMovementSkill updateActive
                    , mkMsg PlayerMsgUsedMovementSkill
                    , mkMsg $ ParticleMsgAddM mkEffectsParticle
                    , mkMsg $ AudioMsgPlaySound dashSoundFilePath pos
                    ]

            | otherwise = []

            where
                active        = isMovementSkillStatusActive $ _status dashSkill
                pos           = _pos player
                flags         = _flags player
                inAir         = not $ _touchingGround flags
                sprWillFinish = case MS._sprite dashSkill of
                    Just spr -> not (spriteFinished spr) && spriteFinished (updateSprite spr)
                    Nothing  -> False

                cfg             = _config (_data dashSkill :: DashSkillData)
                dashGravity
                    | inAir     = 0.0
                    | otherwise = _dashGroundGravity cfg

                moveSkillPressed =
                    MovementSkillAlias `aliasPressed` inputState ||
                    MovementSkillInput `inPlayerInputBuffer` player

                dir
                    | not active && LeftAlias `aliasHold` inputState  = LeftDir
                    | not active && RightAlias `aliasHold` inputState = RightDir
                    | otherwise                                       = _dir player

                momentumAirSpeedX = abs . vecX . _airVel $ _momentum player
                dashSpeed         = _dashSpeed cfg
                dashSpeed'        = if
                    | inAir && momentumAirSpeedX > dashSpeed -> momentumAirSpeedX
                    | otherwise                              -> dashSpeed
                dashVel           = Vel2 (dashSpeed' * directionNeg dir) (dashGravity * timeStep)


updateDashSkill :: Monad m => MovementSkillUpdate DashSkillData m
updateDashSkill player dashSkill = return $ case MS._sprite dashSkill of
    Just spr
        | movementSkillActive dashSkill ->
            let
                spr'          = updateSprite spr
                sprFinished   = spriteFinished spr'
                sprWillFinish = spriteFinished $ updateSprite spr'
                isCancelable  = cancelableFrameTagName `isSpriteFrameTag` spr'

                status
                    | sprFinished   = InactiveMovement
                    | sprWillFinish = ActiveWalkCancelableMovement
                    | isCancelable  = ActiveCancelableMovement
                    | otherwise     = ActiveNotCancelableMovement

            in dashSkill
                { _status           = status
                , _sprite           = Just spr'
                , _cooldown         = cooldown
                , _numCharges       = numCharges
                , _canRefreshCharge = canRefreshCharge
                }

    _ -> dashSkill
        { _cooldown         = cooldown
        , _numCharges       = numCharges
        , _canRefreshCharge = canRefreshCharge
        }

    where
        cooldown = max 0.0 (_cooldown dashSkill - timeStep)

        canRefreshCharge
            | _touchingGround (_flags player) = True
            | otherwise                       = _canRefreshCharge dashSkill

        numCharges
            | cooldown <= 0.0 && canRefreshCharge = playerMovementSkillMaxNumCharges player
            | otherwise                           = _numCharges dashSkill

drawDashSkill :: (GraphicsReadWrite m, MonadIO m) => MovementSkillDraw DashSkillData m
drawDashSkill player dashSkill =
    let
        active  = movementSkillActive dashSkill
        pos     = _pos player
        vel     = _vel player
        dir     = _dir player
        opacity = playerOpacity player
    in case MS._sprite dashSkill of
        Just spr
            | active -> do
                pos' <- graphicsLerpPos pos vel
                drawSpriteEx pos' dir playerBodyZIndex 0.0 opacity NonScaled spr
        _            -> return ()
