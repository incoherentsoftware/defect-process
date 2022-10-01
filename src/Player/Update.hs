module Player.Update
    ( updatePlayer
    ) where

import Control.Monad       (guard)
import Control.Monad.State (execState, execStateT, get, gets, lift, modify, put, when)
import Data.Functor        ((<&>))
import Data.Maybe          (isJust)
import qualified SDL.Raw

import AppEnv
import Attack
import Configs
import Configs.All.Player
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Msg
import Particle.All.Simple
import Player
import Player.BufferedInputState
import Player.Gun.Manager
import Player.LockOnAim
import Player.Messages
import Player.Meter
import Player.Momentum
import Player.MovementSkill
import Player.Overlay.Types as OL
import Player.SecondarySkill.Manager
import Player.SoundHashedIds
import Player.Sprites
import Player.TimersCounters
import Player.Upgrade
import Player.Weapon.Manager
import Util
import Window.Graphics
import Window.InputState
import World.UI.Voiceover
import World.ZIndex

canSwitchDirFrameTagName = "canSwitchDirection" :: FrameTagName

doubleJumpEffectPath    =
    PackResourceFilePath "data/particles/particles.pack" "double-jump.spr" :: PackResourceFilePath
hurtSoundFilePath       = "event:/SFX Events/Player/hurt"                  :: FilePath
jumpSoundFilePath       = "event:/SFX Events/Player/jump"                  :: FilePath
doubleJumpSoundFilePath = "event:/SFX Events/Player/double-jump"           :: FilePath
spawnSoundFilePath      = "event:/SFX Events/Player/spawn"                 :: FilePath
deathSoundFilePath      = "event:/SFX Events/Player/death"                 :: FilePath
grindSoundFilePath      = "event:/SFX Events/Player/grind-c"               :: FilePath
warpOutSoundFilePath    = "event:/SFX Events/Player/warp-out"              :: FilePath

overrideMomentumFrameTagName      = "overrideMomentum"     :: FrameTagName
overrideMomentumTempFrameTagName  = "overrideMomentumTemp" :: FrameTagName
overrideMomentumVertFrameTagName  = "overrideMomentumVert" :: FrameTagName
lockAttackDirFrameTagName         = "lockAttackDir"        :: FrameTagName
airMomentumVelXThresholdOffset    = 5.0                    :: VelX
airMomentumAtkSpeedThreshold      = 5.0                    :: Speed
groundMomentumVelXThresholdOffset = 5.0                    :: VelX
groundFrictionSpeedXThreshold     = 1000.0                 :: SpeedX
minDeathHitlagSecs                = 0.15                   :: Secs

movePlayerHorizontal :: Direction -> Player -> Player
movePlayerHorizontal dir player = player
    { _dir   = dir
    , _vel   = vel'
    , _flags = flags {_movingLeftRight = True}
    }
    where
        playerCfg                  = _config (player :: Player)
        moveAccel                  = _moveAccel playerCfg
        moveSpeed                  = _moveSpeed playerCfg
        moveSpeedShootModifier     = _moveSpeedShootModifier playerCfg
        airMomentumDecelerateSpeed = _airMomentumDecelerateSpeed playerCfg

        vel@(Vel2 velX velY) = _vel (player :: Player)
        dirNeg               = directionNeg dir
        flags                = _flags player
        inAir                = not $ _touchingGround flags
        gunActive            = gunManagerActive $ _gunManager player

        vel'
            | inAir && abs velX > moveSpeed = case dir of
                LeftDir
                    | velX > 0.0 -> Vel2 (velX - airMomentumDecelerateSpeed * timeStep) velY
                    | otherwise  -> vel
                RightDir
                    | velX < 0.0 -> Vel2 (velX + airMomentumDecelerateSpeed * timeStep) velY
                    | otherwise  -> vel
            | gunActive                     = Vel2 (moveSpeed * moveSpeedShootModifier * dirNeg) velY
            | otherwise                     =
                let
                    velX' = flip execState velX $ do
                        -- reset player horizontal velocity when turning around
                        get >>= \vx -> when (dir == RightDir && vx < 0.0) $ put 0.0
                        get >>= \vx -> when (dir == LeftDir && vx > 0.0) $ put 0.0

                        modify (+ moveAccel * timeStep * dirNeg)
                        case dir of
                            RightDir -> modify $ min moveSpeed
                            LeftDir  -> modify $ max (-moveSpeed)
                in Vel2 velX' velY

updatePlayerMoveLeftRight :: InputState -> Player -> Player
updatePlayerMoveLeftRight inputState player
    | cantMove              = player
    | leftHeld && rightHeld = case playerBufferedInputStateLastDir (_bufferedInputState player) of
        LeftDir  -> moveLeft
        RightDir -> moveRight
    | leftHeld              = moveLeft
    | rightHeld             = moveRight
    | otherwise             = if
        | inAir && abs velX > moveSpeed + airMomentumVelXThresholdOffset -> player
        | otherwise                                                      -> player {_vel = Vel2 0.0 velY}
    where
        atkWalkCancelable       = playerAttackWalkCancelable player
        isRestrictedByAtk
            | atkWalkCancelable = False
            | otherwise         = maybe False attackVelHasVelX (playerAttackVel player)
        isRestrictedByMoveSkill = not $ playerMovementSkillWalkCancelable player

        flags          = _flags player
        inAir          = not $ _touchingGround flags
        gettingHit     = _gettingHit flags
        onSpeedRail    = _onSpeedRail flags
        cantMove       = isRestrictedByAtk || isRestrictedByMoveSkill || gettingHit || onSpeedRail
        Vel2 velX velY = _vel player
        moveSpeed      = _moveSpeed $ _config (player :: Player)
        atk            = _attack player

        leftHeld  = LeftAlias `aliasHold` inputState
        rightHeld = RightAlias `aliasHold` inputState

        overrideDir = \p -> (p :: Player)
            { _dir = case atk of
                Just atk'
                    | lockAttackDirFrameTagName `isAttackFrameTag` atk' -> _dir (atk' :: Attack)
                _                                                       -> _dir (p :: Player)
            }

        updateFlags = \p -> if
            | atkWalkCancelable && isJust atk -> p {_flags = flags {_walkCanceled = True}}
            | otherwise                       -> p

        moveLeft  = overrideDir . updateFlags $ movePlayerHorizontal LeftDir player
        moveRight = overrideDir . updateFlags $ movePlayerHorizontal RightDir player

updatePlayerAimTarget
    :: (ConfigsRead m, InputRead m, MsgsRead UpdatePlayerMsgsPhase m)
    => InputState
    -> Player
    -> m Player
updatePlayerAimTarget inputState player =
    let
        dir               = _dir (player :: Player)
        playerCfg         = _config (player :: Player)
        gamepadAimDist    = _gamepadAimDist playerCfg
        defaultAimXOffset = _defaultAimXOffset playerCfg
    in do
        lockOnAim <- updatePlayerLockOnAim player (_lockOnAim player)
        aimPos    <- (_lastUsedInputType <$> readInputState) <&> \case
            MouseKbInputType -> _mouseWorldPos inputState
            GamepadInputType ->
                let
                    axisOffsetPos
                        | _manualOverride lockOnAim =
                            let
                                xAxis = gamepadAxis SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTX inputState
                                yAxis = gamepadAxis SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTY inputState
                            in vecNormalize (Pos2 xAxis yAxis) `vecMul` gamepadAimDist
                        | otherwise                 = Pos2 (directionNeg dir * defaultAimXOffset) 0.0
                in playerShoulderPos player `vecAdd` axisOffsetPos

        return $ player
            { _aimPos    = aimPos
            , _lockOnAim = lockOnAim
            }

updatePlayerGravity :: Player -> Player
updatePlayerGravity player
    | moveSkillActive = player
    | otherwise       = player {_vel = Vel2 velX (velY + gravity * timeStep)}
    where
        moveSkillActive = playerMovementSkillActive player
        gravity         = _gravity $ _config (player :: Player)
        Vel2 velX velY  = _vel (player :: Player)

updatePlayerVelMomentum :: Player -> Player
updatePlayerVelMomentum player = player
    { _vel      = vel'
    , _momentum = momentum'
    }
    where
        flags                  = _flags (player :: Player)
        gettingHit             = _gettingHit flags
        inAir                  = not $ _touchingGround flags
        airStallAttacksCounter = _airStallAttacksCounter $ _timersCounters player
        forceDownAirStall      = inAir && airStallAttacksCounter > 1
        momentum               = _momentum player

        cfg                   = _config (player :: Player)
        airStallForcedGravity = _airStallForcedGravity cfg
        airStallMaxVelY       = _airStallMaxVelY cfg

        adjustAtkVelAirStall :: Vel2 -> Vel2 -> Vel2
        adjustAtkVelAirStall atkVel@(Vel2 atkVelX _) (Vel2 _ playerVelY)
            | isPlayerAttackVelAirStall atkVel player && forceDownAirStall =
                let
                    airStallForcedVelY = airStallForcedGravity * timeStep
                    antiGravityVelY    = (-_gravity cfg) * timeStep
                    velY               = flip execState playerVelY $ do
                        modify (+ antiGravityVelY)
                        modify (min airStallMaxVelY . max 0.0)
                        modify (+ airStallForcedVelY)
                in Vel2 atkVelX velY
            | otherwise                                                    = atkVel

        airAtkVelMomentum :: Vel2 -> MomentumOverride -> (Vel2, PlayerMomentum)
        airAtkVelMomentum atkVel@(Vel2 atkVelX atkVelY) momentumOveride = case momentumOveride of
            MomentumOverrideAll -> (atkVel, setPlayerMomentumAir atkVel momentum)

            MomentumOverrideAllTemp -> (atkVel, updatePlayerMomentumAirTtl momentum)

            MomentumOverrideVert ->
                let
                    airSpeedX                   = abs airVelX
                    atkSpeedX                   = abs atkVelX
                    atkVelX'
                        | airSpeedX > atkSpeedX = if atkVelX < 0.0 then -airSpeedX else airSpeedX
                        | otherwise             = atkVelX
                    atkVel'                     = Vel2 atkVelX' atkVelY
                in (atkVel', setPlayerMomentumAir atkVel' momentum)

            MomentumOverrideNone
                | airSpeed > atkSpeed -> if
                    | atkSpeed < airMomentumAtkSpeedThreshold ->
                        let
                            airMomentum
                                | _airTtl momentum <= 0.0 = resetPlayerMomentumAir momentum
                                | otherwise               = updatePlayerMomentumAirTtl momentum
                        in (atkVel, airMomentum)
                    | otherwise                               ->
                        let atkVelOverride = vecMul (vecNormalize atkVel) airSpeed
                        in (atkVelOverride, setPlayerMomentumAir atkVelOverride momentum)

                | otherwise ->
                    let
                        airMomentum
                            | _airTtl momentum <= 0.0 = setPlayerMomentumAir atkVel momentum
                            | otherwise               = updatePlayerMomentumAirTtl momentum
                    in (atkVel, airMomentum)

            where
                atkSpeed                = vecMagnitude atkVel
                airVel@(Vel2 airVelX _) = _airVel momentum
                airSpeed                = vecMagnitude airVel

        groundAtkVelMomentum :: Vel2 -> MomentumOverride -> (Vel2, PlayerMomentum)
        groundAtkVelMomentum atkVel@(Vel2 atkVelX atkVelY) momentumOveride = case momentumOveride of
            MomentumOverrideAll -> (atkVel, setPlayerMomentumGround atkVelX momentum)

            MomentumOverrideAllTemp -> (atkVel, updatePlayerMomentumGroundFriction momentum)

            MomentumOverrideVert ->
                let
                    momentumSpeedX                   = abs momentumVelX
                    atkSpeedX                        = abs atkVelX
                    atkVelX'
                        | momentumSpeedX > atkSpeedX = if atkVelX < 0.0 then -momentumSpeedX else momentumSpeedX
                        | otherwise                  = atkVelX
                    atkVel'                          = Vel2 atkVelX' atkVelY
                in (atkVel', setPlayerMomentumGround atkVelX' momentum)

            MomentumOverrideNone
                | abs momentumVelX > abs atkVelX ->
                    (Vel2 momentumVelX atkVelY, updatePlayerMomentumGroundFriction momentum)
                | otherwise                      -> (atkVel, setPlayerMomentumGround atkVelX momentum)

            where momentumVelX = _groundVelX momentum

        vel@(Vel2 velX _) = _vel (player :: Player)
        (vel', momentum')
            | gettingHit  = (vel, resetPlayerMomentumAir momentum)
            | otherwise   = case _attack player of
                Just atk ->
                    let
                        atkVel = adjustAtkVelAirStall (attackVel atk `attackVelToVel2` vel) vel

                        momentumOveride
                            | overrideMomentumFrameTagName `isAttackFrameTag` atk     = MomentumOverrideAll
                            | overrideMomentumTempFrameTagName `isAttackFrameTag` atk = MomentumOverrideAllTemp
                            | overrideMomentumVertFrameTagName `isAttackFrameTag` atk = MomentumOverrideVert
                            | otherwise                                               = MomentumOverrideNone
                    in if
                        | inAir     -> airAtkVelMomentum atkVel momentumOveride
                        | otherwise -> groundAtkVelMomentum atkVel momentumOveride

                Nothing
                    | inAir     -> (vel, setPlayerMomentumAir vel momentum)
                    | otherwise ->
                        let
                            moveSpeed                         = _moveSpeed $ _config (player :: Player)
                            speedXThreshold                   = moveSpeed + groundMomentumVelXThresholdOffset
                            momentumVelX
                                | abs velX >= speedXThreshold = velX
                                | otherwise                   = 0.0
                        in (vel, setPlayerMomentumGround momentumVelX momentum)

updatePlayerSprite :: Player -> Player
updatePlayerSprite player = player {_sprite = spr'}
    where
        switchSprite :: Sprite -> Sprite
        switchSprite newSpr
            | spr /= newSpr = newSpr
            | otherwise     = updateSprite spr

        velY          = vecY $ _vel (player :: Player)
        flags         = _flags (player :: Player)
        spr           = _sprite (player :: Player)
        sprites       = _sprites (player :: Player)
        idleSpr       = _idle sprites
        doubleJumpSpr = _doubleJump (sprites :: PlayerSprites)

        spr'
            | spr == _hurt sprites = if
                | spriteFinished spr -> switchSprite idleSpr
                | otherwise          -> updateSprite spr

            | _gettingHit flags  = switchSprite $ _hurt sprites
            | _onSpeedRail flags = switchSprite $ _grind (sprites :: PlayerSprites)

            | not (_touchingGround flags) = if
                | velY <= 0.0 -> if
                    | _doubleJumped flags  -> doubleJumpSpr
                    | spr == doubleJumpSpr -> updateSprite spr
                    | otherwise            -> switchSprite $ _jump sprites
                | otherwise   -> switchSprite $ _fall sprites

            | _movingLeftRight flags || _walkCanceled flags = switchSprite $ _walk sprites
            | otherwise                                     = switchSprite idleSpr

updatePlayerPos :: Player -> Player
updatePlayerPos player = player
    { _pos        = pos'
    , _prevHitbox = prevHitbox
    }
    where
        pos            = _pos (player :: Player)
        Vel2 velX velY = _vel (player :: Player)
        flags          = _flags (player :: Player)

        velX'
            | velX > 0.0 && _touchingRightWall flags = 0.0
            | velX < 0.0 && _touchingLeftWall flags  = 0.0
            | otherwise                              = velX
        velY'
            | velY > 0.0 && _touchingGround flags = 0.0
            | otherwise                           = velY

        vel  = Vel2 velX' velY'
        pos' = toPos2 $ pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)

        prevHitbox
            | _resetPrevHitbox flags = playerHitbox $ player {_pos = pos'}
            | otherwise              = _prevHitbox player

updatePlayerGettingHit :: Player -> Player
updatePlayerGettingHit player = player {_flags = flags'}
    where
        flags       = _flags (player :: Player)
        gettingHit  = _gettingHit flags
        spr         = _sprite (player :: Player)
        sprites     = _sprites (player :: Player)
        isHurtSpr   = spr == _hurt sprites
        sprActive   = not $ spriteFinished spr
        gettingHit' = gettingHit && isHurtSpr && sprActive
        flags'      = flags {_gettingHit = gettingHit'}

jumpPlayer :: MsgsWrite UpdatePlayerMsgsPhase m => Player -> m Player
jumpPlayer player =
    let
        pos          = _pos (player :: Player)
        jumpSoundMsg = mkMsg $ AudioMsgPlaySound jumpSoundFilePath pos

        jumpVelY = _jumpVelY $ _config (player :: Player)
        velX     = vecX $ _vel (player :: Player)
        flags    = (_flags player)
            { _jumped         = True
            , _risingJump     = PlayerRisingFullJumpFlag 0.0
            , _touchingGround = False
            }
    in do
        writeMsgs [jumpSoundMsg]
        return $ player
            { _vel   = Vel2 velX jumpVelY
            , _flags = flags
            }

doubleJumpPlayer :: MsgsWrite UpdatePlayerMsgsPhase m => Player -> m Player
doubleJumpPlayer player =
    let
        pos                 = _pos (player :: Player)
        dir                 = _dir (player :: Player)
        doubleJumpEffectMsg =
            mkMsg $ ParticleMsgAddM (loadSimpleParticle pos dir worldEffectZIndex doubleJumpEffectPath)
        doubleJumpSoundMsg  = mkMsg $ AudioMsgPlaySound doubleJumpSoundFilePath pos
        velX                = vecX $ _vel (player :: Player)
        doubleJumpVelY      = _doubleJumpVelY $ _config (player :: Player)

        flags = (_flags player)
            { _jumped       = True
            , _doubleJumped = True
            }

        timersCounters  = _timersCounters player
        timersCounters' = timersCounters
            { _airStallAttacksCounter = 0
            , _doubleJumpCounter      = _doubleJumpCounter timersCounters + 1
            }
    in do
        writeMsgs [doubleJumpEffectMsg, doubleJumpSoundMsg]
        return $ player
            { _vel            = Vel2 velX doubleJumpVelY
            , _flags          = flags
            , _timersCounters = timersCounters'
            }

platformDropPlayer :: Player -> Player
platformDropPlayer player = player
    { _flags = (_flags player)
        { _platformDropped  = True
        , _platformDropping = True
        , _onPlatform       = False
        }
    , _vel   = Vel2 velX platformDropSpeed
    }
    where
        platformDropSpeed = _platformDropSpeed $ _config (player :: Player)
        velX              = vecX $ _vel (player :: Player)

updatePlayerJump :: MsgsWrite UpdatePlayerMsgsPhase m => InputState -> Player -> m Player
updatePlayerJump inputState player
    | jumpPressed && canJump =
        let
            onPlatform         = _onPlatform flags
            timersCounters     = _timersCounters player
            onGround           = _touchingGround flags || _graceJumpTtl timersCounters > 0.0
            maxDoubleJumpCount = playerUpgradeCount DoubleJumpUpgradeType player + 1
            canDoubleJump      = _doubleJumpCounter timersCounters < maxDoubleJumpCount

            jumpDownPressed =
                (JumpAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState) ||
                JumpDownInput `inPlayerInputBuffer` player
        in if
            | onPlatform && jumpDownPressed                         -> return $ platformDropPlayer player
            | onGround && risingJumpFlag == PlayerNotRisingJumpFlag -> jumpPlayer player
            | canDoubleJump                                         -> doubleJumpPlayer player
            | otherwise                                             -> return player

    | otherwise =
        let
            playerCfg              = _config (player :: Player)
            shortJumpReleaseSecs   = _shortJumpReleaseSecs playerCfg
            shortJumpThresholdVelY = _shortJumpThresholdVelY playerCfg
            shortJumpVelY          = _shortJumpVelY playerCfg
            Vel2 velX velY         = _vel (player :: Player)
        in return $ case risingJumpFlag of
            PlayerRisingFullJumpFlag jumpHeldSecs
                | jumpHeldSecs <= shortJumpReleaseSecs && JumpAlias `aliasNotHold` inputState ->
                    let flags' = flags {_risingJump = PlayerRisingShortJumpFlag}
                    in player {_flags = flags'}
                | otherwise                                                                   ->
                    let
                        jumpHeldSecs' = jumpHeldSecs + timeStep
                        flags'        = flags {_risingJump = PlayerRisingFullJumpFlag jumpHeldSecs'}
                    in player {_flags = flags'}

            PlayerRisingShortJumpFlag
                | velY > shortJumpThresholdVelY -> player
                    { _vel   = Vel2 velX shortJumpVelY
                    , _flags = flags {_risingJump = PlayerNotRisingJumpFlag}
                    }
                | otherwise                     -> player

            PlayerNotRisingJumpFlag -> player

    where
        flags               = _flags player
        gettingHit          = _gettingHit flags
        risingJumpFlag      = _risingJump flags
        moveSkillCancelable = playerMovementSkillCancelable player
        atkCancelable       = playerAttackCancelable player
        canJump             = not gettingHit && moveSkillCancelable && atkCancelable
        jumpPressed         = JumpAlias `aliasPressed` inputState || JumpInput `inPlayerInputBuffer` player

updatePlayerDirFromAttack :: Player -> Player
updatePlayerDirFromAttack player = player {_dir = dir'}
    where
        dir  = _dir (player :: Player)
        dir' = maybe dir (_dir :: Attack -> Direction) (_attack player)

updatePlayerAttack :: InputState -> Player -> Player
updatePlayerAttack inputState player = player {_attack = attack}
    where
        flags            = _flags player
        gettingHit       = _gettingHit flags
        jumped           = _jumped flags
        walkCanceled     = _walkCanceled flags
        platformDropped  = _platformDropped flags
        moveSkillCancels = playerMovementSkillStatus player == ActiveNotCancelableMovement
        firedGun         = _firedGun $ _flags player
        atkCanceled      = or [gettingHit, jumped, walkCanceled, platformDropped, moveSkillCancels, firedGun]

        attack = do
            atk <- _attack player
            guard $ not atkCanceled

            let
                canSwitchDir    = canSwitchDirFrameTagName `isAttackFrameTag` atk
                leftHeld        = LeftAlias `aliasHold` inputState
                rightHeld       = RightAlias `aliasHold` inputState
                bufferedLastDir = playerBufferedInputStateLastDir $ _bufferedInputState player

                dir
                    | canSwitchDir && leftHeld && rightHeld = bufferedLastDir
                    | canSwitchDir && leftHeld              = LeftDir
                    | canSwitchDir && rightHeld             = RightDir
                    | otherwise                             = _dir (player :: Player)

                pos  = _pos (player :: Player)
                atk' = updateAttack pos dir atk

            guard $ not (_done (atk' :: Attack))
            Just atk'

updatePlayerOverlay :: Player -> Player
updatePlayerOverlay player = case _overlay player of
    Nothing ->
        let
            isSliding   = _isSliding $ _momentum player
            speedX      = abs . vecX $ _vel (player :: Player)
            onSpeedRail = _onSpeedRail $ _flags player
            overlays    = _overlays player
        in if
            | isSliding && speedX >= groundFrictionSpeedXThreshold ->
                player {_overlay = Just $ Some (_groundFriction overlays)}
            | onSpeedRail                                          ->
                player {_overlay = Just $ Some (_grindSparks overlays)}
            | otherwise                                            -> player

    Just (Some overlay) -> player
        { _overlay =
            let overlay' = (OL._update overlay) player overlay
            in if
                | OL._done overlay' -> Nothing
                | otherwise         -> Just $ Some overlay'
        }

updatePlayerInSpawnAnim :: MsgsWrite UpdatePlayerMsgsPhase m => Player -> m Player
updatePlayerInSpawnAnim player = do
    let spr = _sprite (player :: Player)
    when (_frameIndex spr == 0 && _frameChanged spr) $
        let pos = _pos (player :: Player)
        in writeMsgs [mkMsg $ AudioMsgPlaySound spawnSoundFilePath pos]

    let
        spr'
            | spriteFinished spr = _idle $ _sprites player
            | otherwise          = updateSprite spr
    return (player {_sprite = spr'} :: Player)

updatePlayerInDeathAnim :: MsgsWrite UpdatePlayerMsgsPhase m => Player -> m Player
updatePlayerInDeathAnim player = do
    let spr = _sprite (player :: Player)
    when (_frameIndex spr == 0 && _frameChanged spr) $
        let pos = _pos (player :: Player)
        in writeMsgs
            [ mkMsg $ UiMsgShowVoiceoverText _deathDisplayText
            , mkMsg $ AudioMsgPlaySound deathSoundFilePath pos
            , mkMsg $ WorldMsgHitlag minDeathHitlagSecs
            ]
    when (spriteFinished spr) $
        writeMsgs [mkMsg WorldMsgDeactivate]

    return (player {_sprite = updateSprite spr} :: Player)

updatePlayerInWarpOutAnim :: MsgsWrite UpdatePlayerMsgsPhase m => Player -> m Player
updatePlayerInWarpOutAnim player = do
    let spr = _sprite (player :: Player)
    when (spriteFinished spr) $
        writeMsgs [mkMsg WorldMsgDeactivate]

    return (player {_sprite = updateSprite spr} :: Player)

updatePlayerMeter :: ConfigsRead m => Player -> m Player
updatePlayerMeter player = updateMeter <$> readConfig _settings _debug
    where
        updateMeter :: DebugConfig -> Player
        updateMeter debugCfg = player {_meter = meter'}
            where
                upgradeCount                  = playerUpgradeCount MeterUpgradeType player
                meter                         = setPlayerMeterUpgradeCount upgradeCount (_meter player)
                meter'
                    | _infiniteMeter debugCfg = fillPlayerMeterFull meter
                    | otherwise               = meter

updatePlayer :: Player -> AppEnv UpdatePlayerMsgsPhase Player
updatePlayer player
    | isPlayerInSpawnAnim player   = updatePlayerInSpawnAnim player
    | isPlayerInDeathAnim player   = updatePlayerInDeathAnim player
    | isPlayerInWarpOutAnim player = updatePlayerInWarpOutAnim player

    | otherwise = do
        inputState <- if
            | _warpingOut (_flags player) -> inactivateInputState <$> readInputState
            | otherwise                   -> readInputState

        player' <- flip execStateT player $ do
            let oldPrevHitbox = _prevHitbox player

            modify $ \p -> p
                { _timersCounters = updatePlayerTimersCounters (_flags p) (_timersCounters p)
                , _flags          = updatePlayerFlags $ _flags p
                , _prevHitbox     = playerHitbox p
                }

            get >>= lift . updatePlayerMessages oldPrevHitbox >>= put
            modify $ updatePlayerMoveLeftRight inputState
            get >>= lift . updatePlayerAimTarget inputState >>= put
            get >>= lift . updatePlayerJump inputState >>= put
            modify updatePlayerGravity

            get >>= \p -> case _movementSkill p of
                Just (Some ms) -> do
                    ms' <- lift $ updateMovementSkill p ms
                    put $ p {_movementSkill = Just (Some ms')}
                Nothing        -> return ()

            get >>= \p -> do
                secondarySkillMgr <- lift $ updateSecondarySkillManager p (_secondarySkillManager p)
                put $ p {_secondarySkillManager = secondarySkillMgr}

            get >>= \p -> do
                gunMgr <- lift $ updateGunManager p (_gunManager p)
                put $ p
                    { _dir        = if
                        | gunManagerActive gunMgr -> gunManagerDir gunMgr
                        | otherwise               -> _dir (p :: Player)
                    , _gunManager = gunMgr
                    }

            modify $ updatePlayerAttack inputState

            get >>= \p -> do
                weaponMgr <- lift $ updateWeaponManager p (_weaponManager p)
                put $ p {_weaponManager = weaponMgr}
            modify updatePlayerDirFromAttack

            modify updatePlayerOverlay
            modify updatePlayerVelMomentum
            modify updatePlayerPos
            modify updatePlayerSprite
            modify updatePlayerGettingHit

            get >>= lift . updatePlayerMeter >>= put
            get >>= lift . updatePlayerBufferedInput >>= put

            isInvincible <- lift $ readSettingsConfig _debug _playerInvincible
            when (isHealthZero (_health player) && not isInvincible) $
                modify $ \p -> (p :: Player) {_sprite = _death (_sprites p)}

            flags <- gets _flags
            when (_warpingOut flags && _touchingGround flags) $ do
                modify $ \p -> (p :: Player) {_sprite = _warpOut (_sprites p)}
                get >>= lift . \p -> writeMsgs [mkMsg $ AudioMsgPlaySound warpOutSoundFilePath (_pos (p :: Player))]

        let
            prevGettingHit = _gettingHit $ _flags player
            gettingHit     = _gettingHit $ _flags player'
            onSpeedRail    = _onSpeedRail $ _flags player'
            pos            = _pos (player' :: Player)

        when (gettingHit && not prevGettingHit) $
            writeMsgs [mkMsg $ AudioMsgPlaySound hurtSoundFilePath pos]

        when onSpeedRail $
            let hashedId = _grind (_soundHashedIds player' :: PlayerSoundHashedIds)
            in writeMsgs [mkMsg $ AudioMsgPlaySoundContinuous grindSoundFilePath hashedId pos]

        return player'
