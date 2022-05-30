module Player.Messages
    ( updatePlayerMessages
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (StateT, execStateT, get, gets, lift, modify, put, when)
import Data.Dynamic           (toDyn)
import Data.Foldable          (foldlM)
import Data.Typeable          (Typeable)
import qualified Data.List as L
import qualified Data.Set as S

import AppEnv
import Attack
import Attack.Hit
import Collision.Hitbox
import Configs
import Configs.All.Level
import Configs.All.Player
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Msg
import Particle.All.Simple
import Player
import Player.Gun as G
import Player.Gun.Manager
import Player.Meter
import Player.Momentum
import Player.MovementSkill as MS
import Player.SecondarySkill as SS
import Player.SecondarySkill.Manager
import Player.TimersCounters
import Player.Upgrade
import Player.Upgrade.Manager
import Player.Weapon as W
import Player.Weapon.Manager
import Util
import World.Surface
import World.Util
import World.ZIndex

particlesPath           = \f -> PackResourceFilePath "data/particles/particles.pack" f
landingSoftEffectPath   = particlesPath "landing-soft.spr"   :: PackResourceFilePath
landingMediumEffectPath = particlesPath "landing-medium.spr" :: PackResourceFilePath
landingHardEffectPath   = particlesPath "landing-hard.spr"   :: PackResourceFilePath
landingAttackEffectPath = particlesPath "landing-attack.spr" :: PackResourceFilePath
playerHurtEffectPath    = particlesPath "player-hurt.spr"    :: PackResourceFilePath

onPlatformEpsilon       = 25.0 :: Distance
wallStickDistX          = 0.1  :: Distance
onMovingPlatformEpsilon = 1.0  :: Distance

updatePlayerMessages :: Hitbox -> Player -> AppEnv UpdatePlayerMsgsPhase Player
updatePlayerMessages oldPlayerPrevHbx player =
    processCollisionMsgs oldPlayerPrevHbx player >>=
    processPlayerMsgs >>=
    processHurtMsgs

updatePlayerWeapon :: Typeable d => (Weapon d -> Weapon d) -> Player -> Player
updatePlayerWeapon updateWeapon player = player {_weaponManager = weaponManager'}
    where
        weaponManager  = _weaponManager player
        weapons        =
            [ Some $ (W._processDynamic wpn) (toDyn updateWeapon) wpn
            | Some wpn <- _weapons weaponManager
            ]
        weaponManager' = weaponManager {_weapons = weapons}

updatePlayerGun :: Typeable d => (Gun d -> Gun d) -> Player -> Player
updatePlayerGun updateGun player = player {_gunManager = playerGunMgr {_guns = guns}}
    where
        playerGunMgr = _gunManager player
        guns         =
            [ Some $ (G._updateDynamic gun) (toDyn updateGun) gun
            | Some gun <- _guns playerGunMgr
            ]

buyPlayerWeapon :: Some Weapon -> GoldValue -> Player -> Player
buyPlayerWeapon weapon cost player = (givePlayerWeapon weapon player) {_gold = playerGold}
    where playerGold = max 0 (_gold player - cost)

buyPlayerGun :: Some Gun -> GoldValue -> Player -> Player
buyPlayerGun gun cost player = (givePlayerGun gun player) {_gold = playerGold}
    where playerGold = max 0 (_gold player - cost)

buyPlayerMovementSkill :: Some MovementSkill -> GoldValue -> Player -> Player
buyPlayerMovementSkill movementSkill cost player = player
    { _movementSkill = Just movementSkill
    , _gold          = playerGold
    }
    where playerGold = max 0 (_gold player - cost)

buyPlayerSecondarySkill :: Some SecondarySkill -> GoldValue -> Player -> Player
buyPlayerSecondarySkill secondarySkill cost player = player
    { _secondarySkillManager = secondarySkillMgr
    , _gold                  = playerGold
    }
    where
        secondarySkillMgr = giveSecondarySkillManagerSkill Nothing secondarySkill (_secondarySkillManager player)
        playerGold        = max 0 (_gold player - cost)

buyPlayerUpgrade :: PlayerUpgradeType -> GoldValue -> Player -> Player
buyPlayerUpgrade upgradeType cost player = player
    { _upgradeManager = upgradeMgr
    , _gold           = gold
    }
    where
        upgradeMgr = givePlayerUpgradeManagerUpgrade upgradeType (_upgradeManager player)
        gold       = max 0 (_gold player - cost)

buyPlayerHealth :: GoldValue -> Player -> Player
buyPlayerHealth cost player = player
    { _health = health {_value = _maxValue (health :: Health)}
    , _gold   = gold
    }
    where
        health = _health player
        gold   = max 0 (_gold player - cost)

clearPlayerWeapon :: WeaponType -> Player -> Player
clearPlayerWeapon wpnType player = player {_weaponManager = weaponMgr}
    where weaponMgr = clearWeaponManagerWeapon wpnType (_weaponManager player)

clearPlayerGun :: GunType -> Player -> Player
clearPlayerGun gunType player = player {_gunManager = gunMgr}
    where gunMgr = clearGunManagerGun gunType (_gunManager player)

clearPlayerSecondarySkillSlot :: SecondarySkillType -> Player -> Player
clearPlayerSecondarySkillSlot secondarySkillType player = player {_secondarySkillManager = secondarySkillMgr}
    where secondarySkillMgr = clearSecondarySkillManagerSkill secondarySkillType (_secondarySkillManager player)

setPlayerAttack :: Attack -> Player -> Player
setPlayerAttack atk player = player
    { _attack         = Just atk
    , _flags          = flags {_attacked = True}
    , _timersCounters = timersCounters {_airStallAttacksCounter = airStallAttacksCounter'}
    }
    where
        flags          = _flags player
        inAir          = not $ _touchingGround flags
        atkVel         = attackVel atk `attackVelToVel2` _vel (player :: Player)
        timersCounters = _timersCounters player

        airStallAttacksCounter                                 = _airStallAttacksCounter timersCounters
        airStallAttacksCounter'
            | inAir && isPlayerAttackVelAirStall atkVel player = airStallAttacksCounter + 1
            | otherwise                                        = airStallAttacksCounter

setPlayerAttackDesc :: MonadIO m => AttackDescription -> Player -> m Player
setPlayerAttackDesc atkDesc player = setPlayerAttackDescEx pos dir atkDesc player
    where
        pos = _pos (player :: Player)
        dir = _dir (player :: Player)

setPlayerAttackDescEx :: MonadIO m => Pos2 -> Direction -> AttackDescription -> Player -> m Player
setPlayerAttackDescEx pos dir atkDesc player = do
    atk <- mkAttack pos dir atkDesc
    return $ setPlayerAttack atk player

updatePlayerPos :: (Pos2 -> Pos2) -> Player -> Player
updatePlayerPos update player
    | (x' < x) && touchingLeftWall  = player
    | (x' > x) && touchingRightWall = player
    | otherwise                     = player {_pos = pos'} :: Player
    where
        pos@(Pos2 x _)    = _pos (player :: Player)
        pos'@(Pos2 x' _)  = update pos
        flags             = _flags player
        touchingLeftWall  = _touchingLeftWall flags
        touchingRightWall = _touchingRightWall flags

pushbackPlayerOffset :: OffsetX -> Player -> Player
pushbackPlayerOffset offsetX player
    | playerMovementSkillActive player = player
    | otherwise                        =
        let
            standardPushback = vecAdd $ Pos2 offsetX 0.0
            update           = case _wallProximity (_flags player) of
                PlayerWallProximityLeft wallOffsetX
                    | wallOffsetX <= 0.0                                           -> id
                    | wallOffsetX > 0.0 && offsetX < 0.0 && -wallOffsetX < offsetX -> vecAdd $ Pos2 (-wallOffsetX) 0.0
                    | otherwise                                                    -> standardPushback
                PlayerWallProximityRight wallOffsetX
                    | wallOffsetX <= 0.0                                           -> id
                    | wallOffsetX > 0.0 && offsetX > 0.0 && wallOffsetX < offsetX  -> vecAdd $ Pos2 wallOffsetX 0.0
                    | otherwise                                                    -> standardPushback
                PlayerWallProximityNone                                            -> standardPushback
        in updatePlayerPos update player

updatePlayerGold :: MsgsWrite UpdatePlayerMsgsPhase m => (GoldValue -> GoldValue) -> Player -> m Player
updatePlayerGold updateGold player =
    let
        gold     = _gold player
        gold'    = max (GoldValue 0) (updateGold gold)
        goldDiff = gold' - gold
    in do
        writeMsgs [mkMsg $ UiMsgGainedGold goldDiff]
        return $ player {_gold = gold'}

resetPlayerAirStallDoubleJump :: Player -> Player
resetPlayerAirStallDoubleJump player = player
    { _timersCounters = (_timersCounters player)
        { _airStallAttacksCounter = 0
        , _doubleJumpCounter      = 0
        }
    }

processPlayerMsgs :: Player -> AppEnv UpdatePlayerMsgsPhase Player
processPlayerMsgs player = foldlM processMsg player =<< readMsgs
    where
        processMsg :: Player -> PlayerMsgPayload -> AppEnv UpdatePlayerMsgsPhase Player
        processMsg !p d = case d of
            PlayerMsgSetVelocity vel                    -> return (p {_vel = vel} :: Player)
            PlayerMsgUpdateVelocity update              -> return (p {_vel = update $ _vel (p :: Player)} :: Player)
            PlayerMsgSetDirection dir                   -> return (p {_dir = dir} :: Player)
            PlayerMsgSetPosition pos                    -> return (p {_pos = pos} :: Player)
            PlayerMsgUpdatePosition update              -> return $ updatePlayerPos update p
            PlayerMsgPushbackOffset offsetX             -> return $ pushbackPlayerOffset offsetX p
            PlayerMsgFiredGun                           -> return $ p {_flags = flags {_firedGun = True}}
            PlayerMsgUpdateWeapon wpnUpdate             -> return $ updatePlayerWeapon wpnUpdate p
            PlayerMsgUpdateGun update                   -> return $ updatePlayerGun update p
            PlayerMsgUpdateMovementSkill update         -> return $ updatePlayerMovementSkill update
            PlayerMsgCancelMovementSkill                -> return $ cancelPlayerMovementSkill p
            PlayerMsgUpdateSecondarySkill slot update   -> return $ updatePlayerSecondarySkill slot update
            PlayerMsgSetPhased                          -> return $ p {_flags = flags {_phased = True}}
            PlayerMsgUpdateGold update                  -> updatePlayerGold update p
            PlayerMsgClearAttack                        -> return $ p {_attack = Nothing}
            PlayerMsgSetAttack atk                      -> return $ setPlayerAttack atk p
            PlayerMsgSetAttackDesc atkDesc              -> setPlayerAttackDesc atkDesc p
            PlayerMsgSetAttackDescEx pos dir atkDesc    -> setPlayerAttackDescEx pos dir atkDesc p
            PlayerMsgUpdateAttack update                -> return $ p {_attack = update <$> _attack p}
            PlayerMsgUpdateAttackM update               -> updatePlayerAttackM update
            PlayerMsgUsedMovementSkill                  -> return $ p {_flags = flags {_movementSkilled = True}}
            PlayerMsgBuyWeapon wpn cost                 -> return $ buyPlayerWeapon wpn cost p
            PlayerMsgBuyGun gun cost                    -> return $ buyPlayerGun gun cost p
            PlayerMsgBuyMovementSkill moveSkill cost    -> return $ buyPlayerMovementSkill moveSkill cost p
            PlayerMsgBuySecondarySkill secSkill cost    -> return $ buyPlayerSecondarySkill secSkill cost p
            PlayerMsgBuyUpgrade upgrade cost            -> return $ buyPlayerUpgrade upgrade cost p
            PlayerMsgBuyHealth cost                     -> return $ buyPlayerHealth cost p
            PlayerMsgInteract _                         -> return p
            PlayerMsgClearWeapon wpnType                -> return $ clearPlayerWeapon wpnType p
            PlayerMsgClearGun gunType                   -> return $ clearPlayerGun gunType p
            PlayerMsgClearSecondarySkill ssType         -> return $ clearPlayerSecondarySkillSlot ssType p
            PlayerMsgSetSecondarySkillSlots ntT upT dnT -> return $ setPlayerSecondarySkillManagerOrder ntT upT dnT p
            PlayerMsgClearInputBuffer _                 -> return p
            PlayerMsgGainMeter meter                    -> return $ p {_meter = gainPlayerMeter meter (_meter p)}
            PlayerMsgFillMeterFull                      -> return $ p {_meter = fillPlayerMeterFull (_meter p)}
            PlayerMsgSpendMeter meter                   -> return $ p {_meter = spendPlayerMeter meter (_meter p)}
            PlayerMsgResetDoubleJump                    -> return $ resetPlayerAirStallDoubleJump p
            PlayerMsgResetAirStallAttacksCounter        -> return resetPlayerAirStall
            PlayerMsgForceInAir                         -> return forcePlayerInAir
            PlayerMsgWarpOut                            -> return $ p {_flags = flags {_warpingOut = True}}
            PlayerMsgTouchingInfoSign                   -> return $ p {_flags = flags {_touchingInfoSign = True}}
            where
                updatePlayerMovementSkill :: Typeable d => (MovementSkill d -> MovementSkill d) -> Player
                updatePlayerMovementSkill update = p {_movementSkill = update' <$> _movementSkill p}
                    where update' = \(Some ms) -> Some $ (MS._updateDynamic ms) (toDyn update) ms

                updatePlayerSecondarySkill
                    :: Typeable d
                    => SecondarySkillSlot
                    -> (SecondarySkill d -> SecondarySkill d)
                    -> Player
                updatePlayerSecondarySkill slot update = p {_secondarySkillManager = secondarySkillMgr'}
                    where
                        update'            = \(Some ss) -> Some $ (SS._updateDynamic ss) (toDyn update) ss
                        secondarySkillMgr  = _secondarySkillManager p
                        secondarySkillMgr' = case slot of
                            SecondarySkillNeutralSlot ->
                                secondarySkillMgr {_neutralSlot = update' <$> _neutralSlot secondarySkillMgr}
                            SecondarySkillUpSlot      ->
                                secondarySkillMgr {_upSlot = update' <$> _upSlot secondarySkillMgr}
                            SecondarySkillDownSlot    ->
                                secondarySkillMgr {_downSlot = update' <$> _downSlot secondarySkillMgr}

                updatePlayerAttackM = \update -> do
                    atk <- maybe (return Nothing) (fmap Just . update) (_attack p)
                    return $ p {_attack = atk}

                resetPlayerAirStall = p {_timersCounters = (_timersCounters p) {_airStallAttacksCounter = 0}}

                forcePlayerInAir = p
                    { _flags          = flags {_touchingGround = False}
                    , _timersCounters = (_timersCounters p) {_graceJumpTtl = 0.0}
                    }

                flags = _flags p

playerTouchingGround :: PosY -> SurfaceType -> Hitbox -> LevelConfig -> Player -> Player
playerTouchingGround groundY surfType oldPlayerPrevHbx levelCfg player = case surfType of
    GeneralSurface ->
        let
            flags'  = flags
                { _touchingGround = True
                , _risingJump     = PlayerNotRisingJumpFlag
                }
            player' = player {_flags = flags'}
        in if
            | velY >= 0.0 -> player'
                { _pos      = Pos2 x groundY
                , _vel      = Vel2 velX 0.0
                , _momentum = resetMomentumAir
                } :: Player
            | otherwise   -> player'

    PlatformSurface ->
        let
            oldY       = hitboxBot oldPlayerPrevHbx
            onPlatform = oldY < groundY || approxEqEx y groundY onPlatformEpsilon
            flags'     = flags {_touchingPlatform = True}
            player'    = player {_flags = flags'}
        in if
            | _platformDropping flags   -> player'
            | onPlatform && velY >= 0.0 -> player'
                { _pos      = Pos2 x groundY
                , _vel      = Vel2 velX 0.0
                , _momentum = resetMomentumAir
                , _flags    = flags'
                    { _touchingGround = True
                    , _onPlatform     = True
                    , _risingJump     = PlayerNotRisingJumpFlag
                    }
                }
            | otherwise                 -> player'

    SpeedRailSurface dir ->
        let
            oldY       = hitboxBot oldPlayerPrevHbx
            onPlatform = oldY < groundY || approxEqEx y groundY onPlatformEpsilon
            playerDir  = _dir (player :: Player)
            flags'     = flags {_touchingPlatform = True}
            player'    = player {_flags = flags'}

            playerMaxTurnaroundSpeed = _speedRailMaxPlayerTurnaroundSpeed levelCfg
            velX'
                | dir /= playerDir   = if
                    | velX < (-playerMaxTurnaroundSpeed) -> -playerMaxTurnaroundSpeed
                    | velX > playerMaxTurnaroundSpeed    -> playerMaxTurnaroundSpeed
                    | otherwise                          -> velX
                | otherwise          = velX

            speedRailAccel                             = _speedRailAcceleration levelCfg
            speedRailAccel'
                | dir /= playerDir                     = speedRailAccel
                | otherwise                            = speedRailAccel
            speedRailVelX                              = velX' + speedRailAccel' * timeStep * directionNeg dir
            speedRailMaxSpeed                          = _speedRailMaxSpeed levelCfg
            speedRailVelX'
                | speedRailVelX < (-speedRailMaxSpeed) = -speedRailMaxSpeed
                | speedRailVelX > speedRailMaxSpeed    = speedRailMaxSpeed
                | otherwise                            = speedRailVelX
        in if
            | _noSpeedRailTtl (_timersCounters player') > 0.0 || _platformDropping flags -> player'

            | onPlatform && velY >= 0.0 -> player'
                { _pos      = Pos2 x groundY
                , _vel      = Vel2 speedRailVelX' 0.0
                , _dir      = dir
                , _momentum = resetMomentumAir
                , _attack   = Nothing
                , _flags    = flags'
                    { _onSpeedRail    = True
                    , _touchingGround = True
                    , _onPlatform     = True
                    , _risingJump     = PlayerNotRisingJumpFlag
                    }
                }

            | otherwise -> player'

    where
        Pos2 x y         = _pos (player :: Player)
        Vel2 velX velY   = _vel (player :: Player)
        flags            = _flags player
        resetMomentumAir = resetPlayerMomentumAir $ _momentum player

playerTouchingWall :: PosX -> PosY -> WallSurfaceType -> Player -> Player
playerTouchingWall wallX wallTopY wallType player = player
    { _pos        = pos
    , _prevHitbox = prevHitbox
    , _flags      = flags
    }
    where
        Pos2 x y = _pos (player :: Player)
        hbx      = playerHitbox player
        hbxLeft  = hitboxLeft hbx
        hbxRight = hitboxRight hbx

        xOffset = case wallType of
            LeftWallSurface
                | hbxLeft < wallX  -> wallX - hbxLeft - wallStickDistX
                | otherwise        -> 0.0
            RightWallSurface
                | hbxRight > wallX -> wallX - hbxRight + wallStickDistX
                | otherwise        -> 0.0
        pos     = Pos2 (x + xOffset) y

        -- reset prev hitbox to prevent pushback interaction from confusing collision detection
        prevHitbox
            | abs xOffset > 0.0 = playerHitbox $ player {_pos = pos}
            | otherwise         = _prevHitbox player

        hbxTop             = hitboxTop hbx
        nearWallTopMaxDist = _nearWallTopMaxDist $ _config (player :: Player)
        isNearTop          = hbxTop < wallTopY || hbxTop - wallTopY <= nearWallTopMaxDist

        flags = (_flags player)
            { _touchingWall        = True
            , _touchingWallNearTop = isNearTop
            , _touchingLeftWall    = wallType == LeftWallSurface
            , _touchingRightWall   = wallType == RightWallSurface
            }

playerTouchingRoof :: PosY -> Player -> Player
playerTouchingRoof roofY player = player
    { _pos   = Pos2 x (roofY + playerHeight)
    , _vel   = Vel2 velX (if velY < 0.0 then 0.0 else velY)
    , _flags = (_flags player) {_touchingRoof = True}
    }
    where
        x              = vecX $ _pos (player :: Player)
        Vel2 velX velY = _vel (player :: Player)
        cfg            = _config (player :: Player)
        playerHeight   = _height (cfg :: PlayerConfig)

playerWillFallOffGround :: Player -> Player
playerWillFallOffGround player = player
    { _flags = (_flags player) {_willFallOffGround = True}
    }

mkGroundImpactEffectMsgs :: Vel2 -> Bool -> Player -> [Msg UpdatePlayerMsgsPhase]
mkGroundImpactEffectMsgs (Vel2 _ prevVelY) prevAtkActive player
    | prevVelY < minSoftLandingVelY = []
    | otherwise                     = [mkMsg $ ParticleMsgAddM loadParticle]
    where
        pos                  = _pos (player :: Player)
        cfg                  = _config (player :: Player)
        minSoftLandingVelY   = _minSoftLandingVelY cfg
        minAttackLandingVelY = _minAttackLandingVelY cfg
        minHardLandingVelY   = _minHardLandingVelY cfg
        minMediumLandingVelY = _minMediumLandingVelY cfg

        impactEffectPath
            | prevAtkActive && prevVelY >= minAttackLandingVelY = landingAttackEffectPath
            | prevVelY >= minHardLandingVelY                    = landingHardEffectPath
            | prevVelY >= minMediumLandingVelY                  = landingMediumEffectPath
            | otherwise                                         = landingSoftEffectPath

        loadParticle = loadSimpleParticle pos RightDir worldEffectZIndex impactEffectPath

playerWallProximity :: OffsetX -> WallSurfaceType -> Player -> Player
playerWallProximity wallOffsetX surfType player = player {_flags = flags}
    where
        wallProximityFlag = case surfType of
            LeftWallSurface  -> PlayerWallProximityLeft wallOffsetX
            RightWallSurface -> PlayerWallProximityRight wallOffsetX
        flags             = (_flags player) {_wallProximity = wallProximityFlag}

processCollisionMsgs :: (ConfigsRead m, MsgsReadWrite UpdatePlayerMsgsPhase m) => Hitbox -> Player -> m Player
processCollisionMsgs oldPlayerPrevHbx player = do
    levelCfg <- _level <$> readConfigs

    let
        processMsgs :: Player -> CollisionMsgPayload -> Player
        processMsgs !p d = case d of
            CollisionMsgTouchingGround groundY surfType     ->
                playerTouchingGround groundY surfType oldPlayerPrevHbx levelCfg p
            CollisionMsgTouchingWall wallX wallTopY wallDir -> playerTouchingWall wallX wallTopY wallDir p
            CollisionMsgTouchingRoof roofY                  -> playerTouchingRoof roofY p
            CollisionMsgWillFallOffGround                   -> playerWillFallOffGround p
            CollisionMsgWallProximity wallOffsetX surfType  -> playerWallProximity wallOffsetX surfType p
            CollisionMsgMovingPlatform _ _                  -> p

        prevTouchingGround = _touchingGround $ _flags player

        processMovingPlatformMsgs :: Player -> CollisionMsgPayload -> Player
        processMovingPlatformMsgs !p d
            | prevTouchingGround && _touchingGround (_flags p) = case d of
                CollisionMsgMovingPlatform platHbx projectedPlatHbx ->
                    let
                        pHbx             = playerHitbox p
                        onMovingPlatform =
                            pHbx `intersectsHitbox` projectedPlatHbx &&
                            approxEqEx (hitboxBot pHbx) (hitboxTop platHbx) onMovingPlatformEpsilon
                    in if
                        | onMovingPlatform ->
                            let
                                offsetX  = hitboxLeft projectedPlatHbx - hitboxLeft platHbx
                                flags    = (_flags p)
                                    { _onMovingLeftPlatform  = offsetX < 0.0
                                    , _onMovingRightPlatform = offsetX > 0.0
                                    }
                                offsetX' = case _wallProximity flags of
                                    PlayerWallProximityLeft wallOffsetX
                                        | wallOffsetX <= 0.0                                           -> 0.0
                                        | wallOffsetX > 0.0 && offsetX < 0.0 && -wallOffsetX < offsetX -> -wallOffsetX
                                        | otherwise                                                    -> offsetX
                                    PlayerWallProximityRight wallOffsetX
                                        | wallOffsetX <= 0.0                                           -> 0.0
                                        | wallOffsetX > 0.0 && offsetX > 0.0 && wallOffsetX < offsetX  -> wallOffsetX
                                        | otherwise                                                    -> offsetX
                                    PlayerWallProximityNone                                            -> offsetX
                            in updatePlayerPos (`vecAdd` Pos2 offsetX' 0.0) (p {_flags = flags})
                        | otherwise        -> p

                _ -> p

            | otherwise = p

        modifyFlags :: Monad m1 => (PlayerFlags -> PlayerFlags) -> StateT Player m1 ()
        modifyFlags update = modify $ \p -> p {_flags = update (_flags p)}

    player' <- flip execStateT player $ do
        modifyFlags $ \f -> f {_touchingGround = False}
        get >>= \p -> L.foldl' processMsgs p <$> lift (readMsgsTo (_msgId p)) >>= put
        get >>= \p -> L.foldl' processMovingPlatformMsgs p <$> lift readMsgs >>= put

        platformDropping <- gets (_platformDropping . _flags)
        touchingPlatform <- gets (_touchingPlatform . _flags)
        when (platformDropping && not touchingPlatform) $
            modifyFlags $ \f -> f {_platformDropping = False}

        gets (_touchingGround . _flags) >>= \case
            True  -> modify resetPlayerAirStallDoubleJump
            False -> modifyFlags $ \f -> f {_touchingGround = False}

    when (_touchingGround (_flags player') && not prevTouchingGround) $
        let
            prevVel       = _vel (player :: Player)
            prevAtkActive = playerAttackActive player
        in writeMsgs $ mkGroundImpactEffectMsgs prevVel prevAtkActive player'

    return player'

processHurtMsgs :: (ConfigsRead m, MsgsReadWrite UpdatePlayerMsgsPhase m) => Player -> m Player
processHurtMsgs player = foldlM processMsgs player =<< readMsgsTo (_msgId player)
    where
        processMsgs :: (ConfigsRead m1, MsgsWrite UpdatePlayerMsgsPhase m1) => Player -> HurtMsgPayload -> m1 Player
        processMsgs !p d = case d of
            HurtMsgAttackHit atkHit ->
                let
                    atkHashedId      = _hashedId atkHit
                    hitByHashedIds   = _hitByHashedIds p
                    isAtkPrevHit     = atkHashedId `S.member` hitByHashedIds
                    isHurtInvincible = isPlayerHurtInvincible p
                    isPhased         = _phased $ _flags p

                    pos            = hitboxCenter $ playerHitbox p
                    dir            = _dir (p :: Player)
                    mkHurtEffect   = loadSimpleParticle pos dir playerOverBodyZIndex playerHurtEffectPath
                in if
                    | isAtkPrevHit -> return p

                    | isHurtInvincible || isPhased ->
                        return $ p {_hitByHashedIds = atkHashedId `S.insert` hitByHashedIds}

                    | otherwise -> do
                        p' <- hurtPlayer atkHit <$> readConfig _settings _debug <*> pure p
                        let
                            cfg                             = _config p'
                            hitlagSecs
                                | isHealthZero (_health p') = _deathHitlagSecs cfg
                                | otherwise                 = _hurtHitlagSecs cfg

                        writeMsgs
                            [ mkMsg $ ParticleMsgAddM mkHurtEffect
                            , mkMsg $ WorldMsgHitlag hitlagSecs
                            ]
                        return p'

hurtPlayer :: AttackHit -> DebugConfig -> Player -> Player
hurtPlayer atkHit debugCfg player = player
    { _vel            = atkVel
    , _dir            = dir
    , _health         = health
    , _flags          = flags {_gettingHit = True}
    , _hitByHashedIds = atkHashedId `S.insert` hitByHashedIds
    , _timersCounters = timersCounters
    }
    where
        hitByHashedIds   = _hitByHashedIds player
        flags            = _flags player
        atkHashedId      = _hashedId atkHit
        Damage damageVal = _damage (atkHit :: AttackHit)
        damageVal'       = ceiling $ fromIntegral damageVal * _playerDamageMultiplier debugCfg
        health           = decreaseHealth (Damage damageVal') (_health player)

        atkVel@(Vel2 atkVelX _) = _vel (atkHit :: AttackHit)
        dir
            | atkVelX < 0.0     = RightDir
            | atkVelX > 0.0     = LeftDir
            | otherwise         = _dir (player :: Player)

        timersCounters = (_timersCounters player)
            { _hurtInvincibleTtl = _hurtInvincibleSecs $ _config player
            }
