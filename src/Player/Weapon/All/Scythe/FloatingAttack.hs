module Player.Weapon.All.Scythe.FloatingAttack
    ( FloatingAttackOnDoneBehavior(..)
    , FloatingAttackData(..)
    , mkFloatingAttack
    , removeFloatingAtkMsg
    , updateFloatingAtkStatusMsg
    , addFloatingAtkMsgs
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe, isJust)
import qualified Data.Set as S

import Attack
import Collision
import Configs
import Configs.All.Player
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.Scythe
import Constants
import Id
import InfoMsg.Util
import Msg
import Player.Weapon as W
import Player.Weapon.All.Scythe.Data
import Projectile as P
import Util
import Window.Graphics
import Window.InputState
import World.Surface
import World.ZIndex

innerHitboxWidth         = 88.0 * 2.0 :: Float
innerHitboxHeight        = 88.0 * 2.0 :: Float
vertSlashWallLandEpsilon = 5.0        :: Float

floatingTrackingLineColor = Color 38 40 52 255 :: Color

surfaceCollisionOnlyFrameTagName   = FrameTagName "surfaceCollisionOnly"   :: FrameTagName
ignoreSurfaceCollisionFrameTagName = FrameTagName "ignoreSurfaceCollision" :: FrameTagName
vertSlashLandPlatformFrameTagName  = FrameTagName "vertSlashLandPlatform"  :: FrameTagName
vertSlashLandSurfaceFrameTagName   = FrameTagName "vertSlashLandSurface"   :: FrameTagName

defaultRegisteredCollisions = S.fromList
    [ ProjRegisteredEnemyCollision
    , ProjRegisteredRoomItemCollision
    , ProjRegisteredSurfaceCollision
    ] :: S.Set ProjectileRegisteredCollision

data FloatingAttackOnDoneBehavior
    = LingerOnDone
    | VanishOnDone
    | NextAttackOnDone AttackDescription
    deriving Eq

data FloatingAttackData = FloatingAttackData
    { _attack            :: Attack
    , _onDone            :: FloatingAttackOnDoneBehavior
    , _scytheAttackDescs :: ScytheAttackDescriptions
    , _config            :: ScytheConfig
    , _knownPlayerPos    :: Maybe Pos2
    , _enoughChargeHeld  :: Bool
    , _glowOverlaySprite :: Maybe Sprite
    }

mkFloatingAttackData
    :: (ConfigsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> AttackDescription
    -> ScytheAttackDescriptions
    -> m FloatingAttackData
mkFloatingAttackData pos dir atkDesc scytheAtkDescs = do
    atk <- mkAttack pos dir atkDesc
    cfg <- readConfig _playerWeapon _scythe

    return $ FloatingAttackData
        { _attack            = atk
        , _onDone            = attackDescOnDoneBehavior atkDesc scytheAtkDescs
        , _scytheAttackDescs = scytheAtkDescs
        , _config            = cfg
        , _knownPlayerPos    = Nothing
        , _enoughChargeHeld  = False
        , _glowOverlaySprite = Nothing
        }

floatingAttackInnerHitbox :: Attack -> Hitbox
floatingAttackInnerHitbox atk = rectHitbox pos innerHitboxWidth innerHitboxHeight
    where pos = _pos (atk :: Attack) `vecSub` Pos2 (innerHitboxWidth / 2.0) (innerHitboxHeight / 2.0)

floatingAttackHitbox :: ProjectileHitbox FloatingAttackData
floatingAttackHitbox floatingAtk = fromMaybe (floatingAttackInnerHitbox atk) (attackHitbox atk)
    where atk = _attack (P._data floatingAtk :: FloatingAttackData)

attackDescOnDoneBehavior :: AttackDescription -> ScytheAttackDescriptions -> FloatingAttackOnDoneBehavior
attackDescOnDoneBehavior atkDesc scytheAtkDescs
    | atkDesc == multiSlash1       = NextAttackOnDone multiSlash2
    | atkDesc == multiSlash2       = NextAttackOnDone multiSlash3
    | atkDesc == multiSlash3       = NextAttackOnDone multiSlash4
    | atkDesc == multiSlash4       = NextAttackOnDone multiSlash5
    | atkDesc == multiSlash5       = NextAttackOnDone multiSlashGlow
    | atkDesc == multiSlashGlow    = LingerOnDone
    | atkDesc == vertSpinSlash     = NextAttackOnDone vertSpinSlashGlow
    | atkDesc == vertSpinSlashGlow = LingerOnDone
    | atkDesc == riseSlash         = NextAttackOnDone riseSlashGlow
    | atkDesc == riseSlashGlow     = LingerOnDone
    | atkDesc == pullSlash         = NextAttackOnDone pullSlashGlow
    | atkDesc == pullSlashGlow     = LingerOnDone
    | atkDesc == diagSpinSlash     = NextAttackOnDone diagSpinSlashGlow
    | atkDesc == diagSpinSlashGlow = LingerOnDone
    | otherwise                    = VanishOnDone
    where
        multiSlash1       = _multiSlash1 scytheAtkDescs
        multiSlash2       = _multiSlash2 scytheAtkDescs
        multiSlash3       = _multiSlash3 scytheAtkDescs
        multiSlash4       = _multiSlash4 scytheAtkDescs
        multiSlash5       = _multiSlash5 scytheAtkDescs
        multiSlashGlow    = _multiSlashGlow scytheAtkDescs
        vertSpinSlash     = _vertSpinSlash scytheAtkDescs
        vertSpinSlashGlow = _vertSpinSlashGlow scytheAtkDescs
        riseSlash         = _riseSlash scytheAtkDescs
        riseSlashGlow     = _riseSlashGlow scytheAtkDescs
        pullSlash         = _pullSlash scytheAtkDescs
        pullSlashGlow     = _pullSlashGlow scytheAtkDescs
        diagSpinSlash     = _diagSpinSlash scytheAtkDescs
        diagSpinSlashGlow = _diagSpinSlashGlow scytheAtkDescs

mkFloatingAttack
    :: (ConfigsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> AttackDescription
    -> ScytheAttackDescriptions
    -> m (Projectile FloatingAttackData)
mkFloatingAttack pos dir atkDesc scytheAtkDescs = do
    floatingAtkData <- mkFloatingAttackData pos dir atkDesc scytheAtkDescs
    msgId           <- newId
    let
        atk      = _attack (floatingAtkData :: FloatingAttackData)
        dummyHbx = DummyHitbox $ _pos (atk :: Attack)

    return $ (mkProjectile floatingAtkData msgId dummyHbx maxSecs)
        { _hitbox               = floatingAttackHitbox
        , _registeredCollisions = defaultRegisteredCollisions
        , _think                = thinkFloatingAttack
        , _update               = updateFloatingAttack
        , _draw                 = drawFloatingAttack
        , _processCollisions    = processFloatingAttackCollisions
        }

wasSentRemoveMsg :: MsgsRead ThinkProjectileMsgsPhase m => Projectile FloatingAttackData -> m Bool
wasSentRemoveMsg floatingAtk = processMsgs <$> readMsgsTo (P._msgId floatingAtk)
    where
        processMsgs :: [ProjectileMsgPayload] -> Bool
        processMsgs []     = False
        processMsgs (d:ds) = case d of
            ProjectileMsgSetTtl 0.0 -> True
            _                       -> processMsgs ds

thinkFloatingAttack
    :: (ConfigsRead m, MonadIO m, MsgsRead ThinkProjectileMsgsPhase m)
    => ProjectileThink FloatingAttackData m
thinkFloatingAttack floatingAtk = wasSentRemoveMsg floatingAtk >>= \case
    True  -> return [infoPosMsg]
    False -> ((infoPosMsg:thinkAttack atk) ++) <$> case _onDone floatingAtkData of
        LingerOnDone
            | atkCancelable || atkDone -> return floatingAtkReadyMsgs

        VanishOnDone
            | atkDone ->
                let
                    updateScytheSt = \scythe -> scythe
                        { W._data = (W._data scythe) {_floatingAttackStatus = FloatingAttackInactive}
                        }
                in return
                    [ mkMsg $ PlayerMsgUpdateWeapon updateScytheSt
                    , mkMsgTo (ProjectileMsgSetTtl 0.0) floatingAtkMsgId
                    ]

        NextAttackOnDone nextFloatingAtkDesc
            | atkDone       -> mkNextFloatingAttackMsgs nextFloatingAtkDesc
            | atkCancelable -> return floatingAtkReadyMsgs

        _ -> return []

    where
        floatingAtkData  = P._data floatingAtk
        floatingAtkMsgId = P._msgId floatingAtk
        atk              = _attack (floatingAtkData :: FloatingAttackData)
        atkPos           = _pos (atk :: Attack)
        atkDir           = _dir (atk :: Attack)
        atkDone          = _done atk
        atkCancelable    = attackCancelable atk
        infoPosMsg       = mkMsg $ InfoMsgProjectilePos atkPos NullId floatingAtkMsgId
        scytheAtkDescs   = _scytheAttackDescs (floatingAtkData :: FloatingAttackData)

        mkNextFloatingAttackMsgs
            :: (ConfigsRead m1, MonadIO m1)
            => AttackDescription
            -> m1 [Msg ThinkProjectileMsgsPhase]
        mkNextFloatingAttackMsgs nextFloatingAtkDesc = do
            nextFloatingAtk <- mkFloatingAttack atkPos atkDir nextFloatingAtkDesc scytheAtkDescs
            let
                nextFloatingAtkMsgId = P._msgId nextFloatingAtk
                floatingAtkStatus    = FloatingAttackActive nextFloatingAtkMsgId 0.0
                updateScytheSt       = \scythe -> scythe
                    { W._data = (W._data scythe) {_floatingAttackStatus = floatingAtkStatus}
                    }

            return
                [ mkMsg $ NewUpdateProjectileMsgAdd (Some nextFloatingAtk)
                , mkMsg $ PlayerMsgUpdateWeapon updateScytheSt
                , mkMsgTo (ProjectileMsgSetTtl 0.0) floatingAtkMsgId
                ]

        floatingAtkReadyMsgs =
            let
                floatingAtkStatus = FloatingAttackActiveReady floatingAtkMsgId atkPos atkDir 0.0
                updateScytheSt    = \scythe -> scythe
                    { W._data = (W._data scythe) {_floatingAttackStatus = floatingAtkStatus}
                    }
            in [mkMsg $ PlayerMsgUpdateWeapon updateScytheSt]

floatingAttackVel :: Attack -> Projectile FloatingAttackData -> Vel2
floatingAttackVel atk floatingAtk
    | _done atk = zeroVel2
    | otherwise = attackVelToVel2 (attackVel atk) (P._vel floatingAtk)

attackGlowOverlay :: Attack -> ScytheAttackDescriptions -> Maybe Sprite
attackGlowOverlay atk scytheAtkDescs
    | atkIs _multiSlashGlow    = Just $ _sprite (_multiSlashGlowOverlay scytheAtkDescs)
    | atkIs _vertSpinSlashGlow = Just $ _sprite (_vertSpinSlashGlowOverlay scytheAtkDescs)
    | atkIs _riseSlashGlow     = Just $ _sprite (_riseSlashGlowOverlay scytheAtkDescs)
    | atkIs _pullSlashGlow     = Just $ _sprite (_pullSlashGlowOverlay scytheAtkDescs)
    | atkIs _diagSpinSlashGlow = Just $ _sprite (_diagSpinSlashGlowOverlay scytheAtkDescs)
    | otherwise                = Nothing
    where atkIs = \atkDescField -> _description atk == atkDescField scytheAtkDescs

updateFloatingAttack
    :: (InputRead m, MsgsReadWrite UpdateProjectileMsgsPhase m)
    => ProjectileUpdate FloatingAttackData m
updateFloatingAttack floatingAtk =
    let
        processInfoMsgs :: [InfoMsgPayload] -> Maybe Pos2
        processInfoMsgs []     = Nothing
        processInfoMsgs (d:ds) = case d of
            InfoMsgPlayer playerInfo -> Just $ playerInfoPos playerInfo
            _                        -> processInfoMsgs ds

        floatingAtkData = P._data floatingAtk
        atk             = _attack (floatingAtkData :: FloatingAttackData)
        vel             = floatingAttackVel atk floatingAtk
        pos             = _pos (atk :: Attack) `vecAdd` toPos2 (vel `vecMul` timeStep)
        dir             = _dir (atk :: Attack)

        enoughChargeHeld = _enoughChargeHeld floatingAtkData
        scytheAtkDescs   = _scytheAttackDescs (floatingAtkData :: FloatingAttackData)
        glowOverlaySpr   = case _glowOverlaySprite floatingAtkData of
            Just overlaySpr
                | enoughChargeHeld -> Just $ updateSprite overlaySpr
            Nothing
                | enoughChargeHeld -> attackGlowOverlay atk scytheAtkDescs
            _                      -> Nothing
    in do
        knownPlayerPos <- processInfoMsgs <$> readMsgs

        return $ floatingAtk
            { P._data = floatingAtkData
                { _attack            = updateAttack pos dir atk
                , _knownPlayerPos    = knownPlayerPos
                , _glowOverlaySprite = glowOverlaySpr
                } :: FloatingAttackData
            , P._vel  = vel
            }

processFloatingAttackCollisions :: ProjectileProcessCollisions FloatingAttackData
processFloatingAttackCollisions collisions floatingAtk = foldr processCollision [] collisions
    where
        floatingAtkData          = P._data floatingAtk
        atk                      = _attack floatingAtkData
        isAtkHitbox              = isJust $ attackHitbox atk
        isSurfaceCollisionOnly   = surfaceCollisionOnlyFrameTagName `isAttackFrameTag` atk
        isIgnoreSurfaceCollision = ignoreSurfaceCollisionFrameTagName `isAttackFrameTag` atk

        processCollision :: ProjectileCollision -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        processCollision collision !msgs = case collision of
            ProjEnemyCollision enemy
                | isAtkHitbox && not isSurfaceCollisionOnly -> attackEnemyHitMessages enemy atk ++ msgs
            ProjRoomItemCollision (Some roomItem)
                | isAtkHitbox                               -> attackCollisionEntityHitMessages roomItem atk ++ msgs
            ProjSurfaceCollision surfaceHbx surfaceType
                | not isIgnoreSurfaceCollision              -> surfaceHitMessages surfaceType surfaceHbx ++ msgs
            _                                               -> msgs

        mkVertSlashLandAttack :: MonadIO m => Hitbox -> Projectile FloatingAttackData -> m Attack
        mkVertSlashLandAttack surfaceHbx fa = mkAttack pos dir (_vertSlashLand scytheAtkDescs)
            where
                faAtk          = _attack $ P._data fa
                x              = vecX $ _pos (faAtk :: Attack)
                pos            = Pos2 x (hitboxTop surfaceHbx)
                dir            = _dir (faAtk :: Attack)
                scytheAtkDescs = _scytheAttackDescs (floatingAtkData :: FloatingAttackData)

        surfaceHitMessages :: SurfaceType -> Hitbox -> [Msg ThinkCollisionMsgsPhase]
        surfaceHitMessages surfaceType surfaceHbx = case surfaceType of
            GeneralSurface     -> generalSurfaceHitMessages surfaceHbx
            PlatformSurface    -> vertSlashLandPlatformMsgs
            SpeedRailSurface _ -> vertSlashLandPlatformMsgs
            where
                innerHbx                     = floatingAttackInnerHitbox atk
                isVertSlashLandPlatformFrame = vertSlashLandPlatformFrameTagName `isAttackFrameTag` atk
                isIntersect                  = surfaceHbx `intersectsHitbox` innerHbx

                vertSlashLandPlatformMsgs
                    | isVertSlashLandPlatformFrame && isIntersect =
                        let
                            update = \fa ->
                                let
                                    faData         = P._data fa
                                    scytheAtkDescs = _scytheAttackDescs (floatingAtkData :: FloatingAttackData)
                                in if
                                    -- vert-slash -> vert-slash-land
                                    | _attack faData `attackIs` _vertSlash scytheAtkDescs -> do
                                        vertSlashLandAtk <- mkVertSlashLandAttack surfaceHbx fa
                                        return $ fa
                                            { P._data = faData {_attack = vertSlashLandAtk}
                                            }
                                    | otherwise                                           -> return fa
                        in [mkMsgTo (ProjectileMsgUpdateM update) (P._msgId floatingAtk)]

                    | otherwise = []

        generalSurfaceHitMessages :: Hitbox -> [Msg ThinkCollisionMsgsPhase]
        generalSurfaceHitMessages surfaceHbx
            | surfaceHbx `intersectsHitbox` innerHbx =
                let
                    Vel2 velX velY            = floatingAttackVel atk floatingAtk
                    offsetX
                        | velX `approxEq` 0.0 = Nothing
                        | velX < 0.0          = Just $ hitboxRight surfaceHbx - hitboxLeft innerHbx
                        | otherwise           = Just $ hitboxLeft surfaceHbx - hitboxRight innerHbx
                    offsetY
                        | velY `approxEq` 0.0 = Nothing
                        | velY < 0.0          = Just $ hitboxBot surfaceHbx - hitboxTop innerHbx
                        | otherwise           = Just $ hitboxTop surfaceHbx - hitboxBot innerHbx

                    offset = case (offsetX, offsetY) of
                        (Nothing, Nothing)                -> zeroPos2
                        (Just offsetX', Nothing)          -> Pos2 offsetX' 0.0
                        (Nothing, Just offsetY')          -> Pos2 0.0 offsetY'
                        (Just offsetX', Just offsetY')
                            | abs offsetX' < abs offsetY' -> Pos2 offsetX' 0.0
                            | otherwise                   -> Pos2 0.0 offsetY'

                    update = \fa ->
                        let
                            faData   = P._data fa
                            faAtk    = _attack faData

                            scytheAttackDescs        = _scytheAttackDescs (floatingAtkData :: FloatingAttackData)
                            vertSlash                = _vertSlash scytheAttackDescs
                            noInsideSurfacesAtkDescs =
                                [ _pullSlash scytheAttackDescs
                                , _vertSpinSlash scytheAttackDescs
                                , _diagSpinSlash scytheAttackDescs
                                , _multiSlash1 scytheAttackDescs
                                , _riseSlash scytheAttackDescs
                                ]
                        in do
                            faAtk' <- if
                                -- vert-slash -> vert-slash-land
                                | faAtk `attackIs` vertSlash ->
                                    let
                                        -- ignore wall collisions
                                        approxEqEx'     = \f1 f2 -> approxEqEx f1 f2 vertSlashWallLandEpsilon
                                        ignoreIntersect =
                                            approxEqEx' (hitboxRight surfaceHbx) (hitboxLeft innerHbx) ||
                                            approxEqEx' (hitboxLeft surfaceHbx) (hitboxRight innerHbx)

                                        isVertSlashLandSurfaceFrame =
                                            vertSlashLandSurfaceFrameTagName `isAttackFrameTag` faAtk
                                    in if
                                        | ignoreIntersect || not isVertSlashLandSurfaceFrame -> return faAtk
                                        | otherwise                                          ->
                                            mkVertSlashLandAttack surfaceHbx fa

                                -- prevent various attacks from going inside surfaces
                                | faAtk `attackIn` noInsideSurfacesAtkDescs -> return $ (faAtk :: Attack)
                                    { _pos = _pos (faAtk :: Attack) `vecAdd` offset
                                    }

                                | otherwise -> return faAtk

                            return $ fa
                                { P._data = faData {_attack = faAtk'}
                                }
                in [mkMsgTo (ProjectileMsgUpdateM update) (P._msgId floatingAtk)]

            | otherwise = []

            where innerHbx = floatingAttackInnerHitbox atk

drawFloatingAttack :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw FloatingAttackData m
drawFloatingAttack floatingAtk =
    let
        atk             = _attack (P._data floatingAtk :: FloatingAttackData)
        atkDesc         = _description atk
        floatingAtkData = P._data floatingAtk
        scytheAtkDescs  = _scytheAttackDescs (floatingAtkData :: FloatingAttackData)
        cfg             = _config (floatingAtkData :: FloatingAttackData)

        isAtkLingerOnDone       = attackDescOnDoneBehavior atkDesc scytheAtkDescs == LingerOnDone
        oscillateOffset
            | isAtkLingerOnDone =
                let
                    amplitude   = _glowOscillateAmplitude cfg
                    period      = _glowOscillatePeriod cfg
                    elapsedSecs = _elapsedSecs $ attackSprite atk
                    offsetY     = amplitude * sin (2 * pi / period * elapsedSecs)
                in Pos2 0.0 offsetY
            | otherwise         = zeroPos2

        pos = _pos (atk :: Attack) `vecAdd` oscillateOffset
        vel = floatingAttackVel atk floatingAtk
        dir = _dir (atk :: Attack)
        spr = fromMaybe (attackSprite atk) (_glowOverlaySprite floatingAtkData)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir worldProjectileZIndex spr

        when (_showTrackingLine cfg && isAtkLingerOnDone) $
            case _knownPlayerPos floatingAtkData of
                Nothing                     -> return ()
                Just (Pos2 playerX playerY) -> do
                    playerHeight <- readConfig _player (_height :: PlayerConfig -> Float)
                    let playerY'  = playerY - playerHeight / 2.0
                    drawLine (Pos2 playerX playerY') pos' floatingTrackingLineColor playerWeaponOverlayZIndex

removeFloatingAtkMsg :: MsgId -> Msg ThinkPlayerMsgsPhase
removeFloatingAtkMsg floatingAtkMsgId = mkMsgTo (ProjectileMsgSetTtl 0.0) floatingAtkMsgId

updateFloatingAtkStatusMsg :: AllowMsgWrite p PlayerMsgPayload => FloatingAttackStatus -> Msg p
updateFloatingAtkStatusMsg floatingAtkStatus = mkMsg $ PlayerMsgUpdateWeapon updateWpn
    where
        updateWpn = \scythe -> scythe
            { W._data = (W._data scythe) {_floatingAttackStatus = floatingAtkStatus}
            }

addFloatingAtkMsgs
    :: (ConfigsRead m, MonadIO m)
    => AttackDescription
    -> Pos2
    -> Direction
    -> AttackDescription
    -> MsgId
    -> ScytheData
    -> m [Msg ThinkPlayerMsgsPhase]
addFloatingAtkMsgs playerSummonMoveDesc pos dir floatingAtkDesc floatingAtkMsgId scytheData = do
    let scytheAtkDescs = _scytheAttackDescs (scytheData :: ScytheData)
    newFloatingAtk    <- mkFloatingAttack pos dir floatingAtkDesc scytheAtkDescs

    return
        [ mkMsg $ PlayerMsgSetAttackDesc playerSummonMoveDesc
        , mkMsg $ NewThinkProjectileMsgAdd (Some newFloatingAtk)
        , updateFloatingAtkStatusMsg $ FloatingAttackActive (P._msgId newFloatingAtk) 0.0
        , removeFloatingAtkMsg floatingAtkMsgId
        ]
