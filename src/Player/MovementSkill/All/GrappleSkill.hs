module Player.MovementSkill.All.GrappleSkill
    ( mkGrappleSkill
    , thinkGrappleSkill
    , updateGrappleSkill
    , drawGrappleSkill
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.PlayerSkill.Grapple
import Constants
import FileCache
import Msg
import Player
import Player.BufferedInputState
import Player.Gun.FireDrawState
import Player.MovementSkill as MS
import Player.MovementSkill.All.GrappleSkill.Data
import Player.MovementSkill.All.GrappleSkill.Projectile
import Player.MovementSkill.All.GrappleSkill.Projectile.Util
import Player.MovementSkill.All.GrappleSkill.Util
import Projectile as P
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

throwSoundFilePath = "event:/SFX Events/Player/Skills/grapple-throw" :: FilePath

forceAirThrowPlayerOffsetY = -1.0  :: PosY
airThrowVelY               = 0.1   :: VelY
towardsInitialVelYDecay    = 600.0 :: Float

mkGrappleSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some MovementSkill)
mkGrappleSkill = do
    grappleData <- mkGrappleData
    return . Some $ (mkMovementSkill grappleData GrappleSkill)
        { MS._think  = thinkGrappleSkill
        , MS._update = updateGrappleSkill
        , MS._draw   = drawGrappleSkill
        }

drawGrappleSkill :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => MovementSkillDraw GrappleData m
drawGrappleSkill player grappleSkill =
    let
        grappleData = MS._data grappleSkill
        pos         = _pos (player :: Player)
        vel         = _vel (player :: Player)
        dir         = _dir (player :: Player)
        opacity     = playerOpacity player
    in case _status (grappleData :: GrappleData) of
        TowardsGrappling _ _ _ _ _ -> do
            pos'          <- graphicsLerpPos pos vel
            let towardsImg = _playerTowards $ _images (grappleData :: GrappleData)
            drawImageEx pos' dir playerMovementSkillOverlayZIndex 0.0 opacity NonScaled towardsImg

        _ -> drawGunFireDrawStateEx player opacity (_fireDrawState grappleData)

updateGrappleSkill :: (ConfigsRead m, MonadIO m, MsgsRead UpdatePlayerMsgsPhase m) => MovementSkillUpdate GrappleData m
updateGrappleSkill player grappleSkill =
    let
        grappleData       = MS._data grappleSkill
        isInactive        = not $ movementSkillActive grappleSkill
        notGrapplingState = grappleData {_status = NotGrappling} :: GrappleData
        gettingHit        = _gettingHit $ _flags player

        canRefreshCharge
            | _touchingGround (_flags player) = True
            | otherwise                       = _canRefreshCharge grappleSkill

        cooldown                                  = max 0.0 (_cooldown grappleSkill - timeStep)
        numCharges
            | cooldown <= 0.0 && canRefreshCharge = playerMovementSkillMaxNumCharges player
            | otherwise                           = _numCharges grappleSkill

        grappleSkill' = grappleSkill
            { _cooldown         = cooldown
            , _numCharges       = numCharges
            , _canRefreshCharge = canRefreshCharge
            }
    in do
        -- need to break grapple if player position suddenly suddenly shifts (mark/recall) to avoid weird interactions
        isPlayerResetPrevHitbox <- readIsPlayerResetPrevHitboxMsg

        case _status (grappleData :: GrappleData) of
            PullGrappling _
                | isInactive || isPlayerResetPrevHitbox -> return $ grappleSkill' {MS._data = notGrapplingState}
                | otherwise                             -> do
                    grappleData' <- updateGrappleData player grappleData
                    return $ grappleSkill'
                        { MS._data   = grappleData'
                        , MS._status = ActiveCancelableMovement
                        }

            TowardsGrappling _ _ _ _ _ -> return $ if
                | isInactive || isPlayerResetPrevHitbox -> grappleSkill' {MS._data = notGrapplingState}
                | otherwise                             -> grappleSkill' {MS._status = ActiveCancelableMovement}

            ShotGrapple _ _ _
                | gettingHit || isPlayerResetPrevHitbox -> return $ grappleSkill' {MS._data = notGrapplingState}
                | otherwise                             -> do
                    grappleData' <- updateGrappleData player grappleData
                    return $ grappleSkill'
                        { MS._data   = grappleData'
                        , MS._status = ActiveNotCancelableMovement
                        }

            HangtimeAfterGrappling _ -> return $ grappleSkill' {MS._status = InactiveMovement}
            NotGrappling             -> return $ grappleSkill' {MS._status = InactiveMovement}


thinkTowardsGrappling
    :: MovementSkill GrappleData
    -> Pos2
    -> VelY
    -> Secs
    -> Overshoot
    -> Maybe Direction
    -> Player
    -> [Msg ThinkPlayerMsgsPhase]
thinkTowardsGrappling grappleSkill targetPos@(Pos2 _ targetY) initialVelY ttl overshoot faceDir player
    | touchingGround && velY > 0.0 =
        [ interruptGrapplingMsg
        , playerSetVelGravityMsg
        ]

    | _touchingWallNearTop flags =
        [ mkMsgEx (PlayerMsgSetVelocity $ _playerTowardsWallTopPopUpVel cfg) MsgAfterNormalOrder
        , interruptGrapplingMsg
        ]

    | _touchingWall flags =
        let
            grappleStatus        = HangtimeAfterGrappling playerAfterHangtimeSecs
            updateTowards        = \ms -> ms
                { MS._data = (MS._data ms) {_status = grappleStatus} :: GrappleData
                }
            hangtimeGrapplingMsg = mkMsg $ PlayerMsgUpdateMovementSkill updateTowards
        in [hangtimeGrapplingMsg, zeroVelMsg]

    | _touchingPlatform flags && playerY - targetY <= playerTowardsPlatformPopUpMinGroundDist && towardsVelY < 0.0 =
        [ mkMsgEx (PlayerMsgSetVelocity $ _playerTowardsPlatformPopUpVel cfg) MsgAfterNormalOrder
        , interruptGrapplingMsg
        ]

    | ttl > 0.0 =
        let
            targetVel@(Vel2 _ targetVelY) = towardsVel `vecAdd` Vel2 0.0 initialVelY
            willOvershoot                 = calcWillOvershoot playerPos targetVel targetPos
        in if
            | overshoot == PreventOvershoot && willOvershoot ->
                let
                    grappleStatus = HangtimeAfterGrappling playerAfterHangtimeSecs
                    updateTowards = \ms -> ms
                        { MS._data = (MS._data ms) {_status = grappleStatus} :: GrappleData
                        }
                    faceDirMsgs   =
                        maybe [] (\dir -> [mkMsgEx (PlayerMsgSetDirection dir) MsgAfterNormalOrder]) faceDir
                in
                    [ playerSetVelGravityMsg
                    , mkMsg $ PlayerMsgUpdateMovementSkill updateTowards
                    ] ++ faceDirMsgs

            | willOvershoot                      -> [interruptGrapplingMsg]
            | touchingGround && targetVelY > 0.0 -> [interruptGrapplingMsg, zeroVelMsg]
            | touchingRoof && targetVelY < 0.0   ->
                [ interruptGrapplingMsg
                , mkMsgEx (PlayerMsgUpdateVelocity $ \(Vel2 _ vY) -> Vel2 0.0 vY) MsgAfterNormalOrder
                ]

            | otherwise ->
                let
                    initialVelY'  = min 0.0 (initialVelY + towardsInitialVelYDecay * timeStep)
                    ttl'          = maxZero $ ttl - timeStep
                    grappleStatus = TowardsGrappling targetPos initialVelY' ttl' overshoot faceDir
                    updateTowards = \ms -> ms
                        { MS._data = (MS._data ms) {_status = grappleStatus} :: GrappleData
                        }
                in
                    [ mkMsgEx (PlayerMsgSetVelocity targetVel) MsgAfterNormalOrder
                    , mkMsg $ PlayerMsgUpdateMovementSkill updateTowards
                    ]

    | otherwise = [interruptGrapplingMsg, zeroVelMsg]

    where
        cfg                                     = _config (MS._data grappleSkill :: GrappleData)
        playerAfterHangtimeSecs                 = _playerAfterHangtimeSecs cfg
        playerTowardsPlatformPopUpMinGroundDist = _playerTowardsPlatformPopUpGroundMinDistance cfg

        velY                            = vecY $ _vel (player :: Player)
        flags                           = _flags player
        touchingGround                  = _touchingGround flags
        touchingRoof                    = _touchingRoof flags
        playerPos@(Pos2 _ playerY)      = _pos (player :: Player)
        towardsVel@(Vel2 _ towardsVelY) = playerTowardsVel playerPos targetPos cfg

        updateInterrupt = \ms -> ms
            { MS._data = (MS._data ms) {_status = NotGrappling} :: GrappleData
            }

        playerSetVelGravityMsg = mkMsgEx (PlayerMsgSetVelocity $ playerGravityVel player) MsgAfterNormalOrder
        interruptGrapplingMsg  = mkMsg $ PlayerMsgUpdateMovementSkill updateInterrupt
        zeroVelMsg             = mkMsgEx (PlayerMsgSetVelocity zeroVel2) MsgAfterNormalOrder

thinkPullGrappling :: MovementSkill GrappleData -> Secs -> Player -> [Msg ThinkPlayerMsgsPhase]
thinkPullGrappling _ ttl _
    | ttl > 0.0 =
        let
            ttl' = maxZero $ ttl - timeStep

            updatePull = \ms -> ms
                { MS._data = (MS._data ms) {_status = PullGrappling ttl'} :: GrappleData
                }
        in
            [ mkMsg $ PlayerMsgUpdateMovementSkill updatePull
            , mkMsgEx (PlayerMsgSetVelocity zeroVel2) MsgAfterNormalOrder
            ]

    | otherwise =
        let
            updatePull = \ms -> ms
                { MS._data = (MS._data ms) {_status = NotGrappling} :: GrappleData
                }
        in [mkMsg $ PlayerMsgUpdateMovementSkill updatePull]

thinkShootingGrapple
    :: MovementSkill GrappleData
    -> ShotType
    -> ShotReleased
    -> Secs
    -> Player
    -> [Msg ThinkPlayerMsgsPhase]
thinkShootingGrapple grappleSkill shotType shotReleased ttl player =
    let
        updateGrappleMsg :: GrappleStatus -> Msg ThinkPlayerMsgsPhase
        updateGrappleMsg status = mkMsg $ PlayerMsgUpdateMovementSkill update
            where
                update = \ms -> ms
                    { MS._data = (MS._data ms) {_status = status} :: GrappleData
                    }

        playerVel
            | _touchingGround (_flags player) = playerGravityVel player
            | otherwise                       = Vel2 0.0 airThrowVelY
        setVelMsg                             = mkMsgEx (PlayerMsgSetVelocity playerVel) MsgAfterNormalOrder

        ttl' = maxZero $ ttl - timeStep
    in (setVelMsg:) $ case shotReleased of
        NoShotReleased ->
            let
                mkGrappleProj = do
                    Some grappleProj <- mkGrappleProjectile shotType player grappleSkill

                    let
                        updateProjMsgId = \ms -> ms
                            { MS._data = (MS._data ms)
                                { _projectileMsgId = P._msgId grappleProj
                                } :: GrappleData
                            }
                    writeMsgs [mkMsg $ PlayerMsgUpdateMovementSkill updateProjMsgId]

                    return $ Some grappleProj
            in
                [ updateGrappleMsg $ ShotGrapple shotType ShotReleased ttl'
                , mkMsg $ NewThinkProjectileMsgAddM mkGrappleProj
                ]

        ShotReleased
            | ttl' <= 0.0 -> [updateGrappleMsg NotGrappling]
            | otherwise   -> [updateGrappleMsg $ ShotGrapple shotType ShotReleased ttl']

thinkHangtime :: MovementSkill GrappleData -> Secs -> Player -> [Msg ThinkPlayerMsgsPhase]
thinkHangtime _ ttl player =
    [ mkMsgEx (PlayerMsgSetVelocity $ playerAntiGravityVel player) MsgAfterNormalOrder
    , mkMsg $ PlayerMsgUpdateMovementSkill updateHangtime
    ]
        where
            ttl'           = maxZero $ ttl - timeStep
            grappleStatus  = if ttl' <= 0.0 then NotGrappling else HangtimeAfterGrappling ttl'
            updateHangtime = \ms -> ms
                { MS._data = (MS._data ms) {_status = grappleStatus} :: GrappleData
                }

thinkHangtimeAfterGrappling :: MovementSkill GrappleData -> Secs -> InputState -> Player -> [Msg ThinkPlayerMsgsPhase]
thinkHangtimeAfterGrappling grappleSkill ttl inputState player
    | _touchingGround (_flags player)  = [interruptGrapplingMsg]
    -- cancel hangtime
    | DownAlias `aliasHold` inputState =
        let hangtimeCancelFallVel = _playerHangtimeCancelFallVel $ _config (MS._data grappleSkill :: GrappleData)
        in
            [ mkMsg $ PlayerMsgUpdateVelocity (vecAdd hangtimeCancelFallVel)
            , interruptGrapplingMsg
            ]
    | otherwise                        = thinkHangtime grappleSkill ttl player
    where
        updateHangtime        = \ms -> ms
            { MS._data = (MS._data ms) {_status = NotGrappling} :: GrappleData
            }
        interruptGrapplingMsg = mkMsg $ PlayerMsgUpdateMovementSkill updateHangtime

thinkNotGrappling :: MovementSkill GrappleData -> Bool -> InputState -> Player -> [Msg ThinkPlayerMsgsPhase]
thinkNotGrappling grappleSkill canUseSkill inputState player
    | moveSkillPressed && canUseSkill && _numCharges grappleSkill > 0 =
        let
            pos        = _pos (player :: Player)
            upHeldDown = UpAlias `aliasHold` inputState
            shotType   = if upHeldDown then PullShot else TowardsShot

            flags         = _flags player
            forceAirThrow = _onMovingLeftPlatform flags || _onMovingRightPlatform flags

            updatePos = \(Pos2 x y) -> if
                | forceAirThrow -> Pos2 x (y + forceAirThrowPlayerOffsetY)
                | otherwise     -> Pos2 x y

            updateNotGrappling = \ms ->
                let
                    msData                  = MS._data ms
                    cfg                     = _config (msData :: GrappleData)
                    playerThrowHangtimeSecs = _playerThrowHangtimeSecs cfg
                in ms
                    { MS._data          = msData
                        { _status   = ShotGrapple shotType NoShotReleased playerThrowHangtimeSecs
                        , _aimAngle = playerAimAngle player
                        } :: GrappleData
                    , _cooldown         = _grappleCooldown cfg
                    , _canRefreshCharge = False
                    , _numCharges       = _numCharges ms - 1
                    }
        in
            [ mkMsgEx (PlayerMsgSetVelocity zeroVel2) MsgAfterNormalOrder
            , mkMsg $ PlayerMsgUpdatePosition updatePos
            , mkMsg $ PlayerMsgUpdateMovementSkill updateNotGrappling
            , setGrappleThrowFireDrawStateMsg
            , mkMsg $ AudioMsgPlaySound throwSoundFilePath pos
            , mkMsg PlayerMsgUsedMovementSkill
            ]

    | otherwise = []

    where
        moveSkillPressed =
            MovementSkillAlias `aliasPressed` inputState || MovementSkillInput `inPlayerInputBuffer` player

thinkGrappleSkill :: (InputRead m, MsgsWrite ThinkPlayerMsgsPhase m) => MovementSkillThink GrappleData m
thinkGrappleSkill canUseSkill player grappleSkill =
    let
        grappleData                  = MS._data grappleSkill
        shotOrTowardsOrPullGrappling = case _status (grappleData :: GrappleData) of
            ShotGrapple _ _ _          -> True
            TowardsGrappling _ _ _ _ _ -> True
            PullGrappling _            -> True
            _                          -> False

        status = _status (grappleData  :: GrappleData)
        flags  = _flags player

        removeGrappleProjMsg = mkMsgTo (ProjectileMsgSetTtl 0.0) (_projectileMsgId grappleData)
    in do
        when (not shotOrTowardsOrPullGrappling) $
            -- this will no-op most of the time to the previous now dead projectile id
            writeMsgs [removeGrappleProjMsg]

        inputState <- readInputState

        return $ if
            | _gettingHit flags -> [removeGrappleProjMsg, mkMsg PlayerMsgCancelMovementSkill]
            | otherwise         -> case status of
                TowardsGrappling pos initialVelY ttl overshoot dir ->
                    thinkTowardsGrappling grappleSkill pos initialVelY ttl overshoot dir player
                PullGrappling ttl                          ->
                    thinkPullGrappling grappleSkill ttl player
                HangtimeAfterGrappling ttl                 ->
                    thinkHangtimeAfterGrappling grappleSkill ttl inputState player
                ShotGrapple shotType shotReleased ttl      ->
                    thinkShootingGrapple grappleSkill shotType shotReleased ttl player
                NotGrappling                               ->
                    thinkNotGrappling grappleSkill canUseSkill inputState player
