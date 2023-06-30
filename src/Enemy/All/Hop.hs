module Enemy.All.Hop
    ( allHopEnemyPreloadPackFilePaths
    , mkHopEnemy
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, get, modify, put)

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Hop
import Configs.All.EnemyLockOn
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy as E
import Enemy.All.Hop.AI
import Enemy.All.Hop.Behavior
import Enemy.All.Hop.Data
import Enemy.All.Hop.Sprites
import FileCache
import Id
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Util
import Window.Graphics

allHopEnemyPreloadPackFilePaths = ["data/enemies/hop-enemy.pack"] :: [FilePath]

hurtSoundPath = "event:/SFX Events/Enemy/Hop/hurt" :: FilePath
onGroundVelY  = 0.1                                :: VelY

mkHopEnemy :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkHopEnemy pos dir = do
    enemyId           <- newId
    enemyData         <- mkHopEnemyData
    enemy             <- mkEnemyWithId enemyId enemyData pos dir
    hopCfg            <- readEnemyConfig _hop
    lockOnReticleData <- readEnemyLockOnConfig _hop
    tauntedData       <- mkEnemyTauntedData $ _tauntedUnderlayDrawScale hopCfg

    return . Some $ enemy
        { _type                   = Just HopEnemy
        , _health                 = _health (hopCfg :: HopEnemyConfig)
        , _hitbox                 = hopEnemyHitbox
        , _inHitstun              = hopEnemyInHitstun
        , _lockOnReticleData      = lockOnReticleData
        , _tauntedData            = Just tauntedData
        , _thinkAI                = thinkAI
        , _updateHurtResponse     = updateHurtResponse
        , _updateGroundResponse   = updateGroundResponse
        , _updateHangtimeResponse = updateHangtimeResponse
        , _updateSprite           = updateSpr
        }

hopEnemyHitbox :: EnemyHitbox HopEnemyData
hopEnemyHitbox enemy
    | _behavior enemyData `elem` [SpawnBehavior, DeathBehavior] = dummyHitbox $ Pos2 x (y - height / 2.0)
    | otherwise                                                 = rectHitbox pos width height
    where
        enemyData = _data enemy
        Pos2 x y  = E._pos enemy
        cfg       = _hop (_config enemyData :: EnemyConfig)
        width     = _width (cfg :: HopEnemyConfig)
        height    = _height (cfg :: HopEnemyConfig)
        pos       = Pos2 (x - width / 2.0) (y - height)

hopEnemyInHitstun :: EnemyInHitstun HopEnemyData
hopEnemyInHitstun enemy = case _behavior (_data enemy) of
    HurtBehavior _ _    -> True
    LaunchedBehavior _  -> True
    FallenBehavior _    -> True
    GetUpBehavior       -> True
    WallSplatBehavior _ -> True
    _                   -> False

updateSpr :: EnemyUpdateSprite HopEnemyData
updateSpr enemy = case _behavior enemyData of
    IdleBehavior _                     -> setOrUpdateEnemySpr $ _idle sprs
    LaunchedBehavior _
        | velY <= 0.0 || inHangtimeVel -> setOrUpdateEnemySpr $ _launched sprs
        | otherwise                    -> setOrUpdateEnemySpr $ _fall sprs
    HurtBehavior _ hurtType
        | justGotHit                   -> setEnemyHurtSprite enemy $ case hurtType of
            WallHurt      -> _wallHurt sprs
            FallenHurt    -> _fallenHurt sprs
            KnockDownHurt -> _knockDownFallen sprs
            AirHurt       -> _airHurt sprs
            LaunchUpHurt  -> _launchUp sprs
            StandHurt     -> _hurt sprs
        | otherwise                    -> updateEnemySprite enemy
    WallSplatBehavior _                -> setOrUpdateEnemySpr $ _wallSplat sprs
    HopLongBehavior                    -> setOrUpdateEnemySpr $ _hopLong sprs
    HopLongLandBehavior                -> setOrUpdateEnemySpr $ _hopLongLand sprs
    HopShortBehavior                   -> setOrUpdateEnemySpr $ _hopShort sprs
    HopShortLandBehavior               -> setOrUpdateEnemySpr $ _hopShortLand sprs
    DeathBehavior                      -> (setOrUpdateEnemySpr $ _death sprs) {_draw = Just drawEnemyDeath}
    AttackHopLongBehavior              -> clearEnemySpr
    AttackHopLongLandBehavior          -> clearEnemySpr
    AttackHopShortBehavior             -> clearEnemySpr
    AttackHopShortLandBehavior         -> clearEnemySpr
    SpawnBehavior                      -> setOrUpdateEnemySpr $ _spawn sprs
    FallenBehavior _                   -> setOrUpdateEnemySpr $ _fallen sprs
    GetUpBehavior                      -> setOrUpdateEnemySpr $ _getUp sprs
    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr
        clearEnemySpr       = clearEnemySprite enemy

        justGotHit    = enemyJustGotHit enemy
        velY          = vecY $ E._vel enemy
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        enemyData     = _data enemy
        sprs          = _sprites enemyData

updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse HopEnemyData m
updateHurtResponse atkHit enemy
    | behavior == DeathBehavior = return enemy

    | isStagger || isAirVulnerable || atkAlwaysLaunches =
        let
            -- prevent sliding on the ground from downwards aerial attacks
            atkVel'
                | onGround && atkVelY > 0.0 = Vel2 0.0 atkVelY
                | otherwise                 = atkVel

            isFallen    = isFallenBehavior behavior || isFallenHurtBehavior behavior
            isKnockDown = onGround && atkVelY > 0.0 && not isFallen

            hurtType
                | atkVelY < 0.0 || atkAlwaysLaunches = LaunchUpHurt
                | otherwise                          = case behavior of
                    _
                        | isKnockDown           -> KnockDownHurt
                    LaunchedBehavior _          -> AirHurt
                    HurtBehavior _ LaunchUpHurt -> AirHurt
                    HurtBehavior _ AirHurt      -> AirHurt
                    FallenBehavior _            -> FallenHurt
                    HurtBehavior _ FallenHurt   -> FallenHurt
                    WallSplatBehavior _         -> WallHurt
                    HurtBehavior _ WallHurt     -> WallHurt
                    _                           -> StandHurt

            enemyCfg    = _config enemyData
            hitstunSecs = enemyHitstunFromAttackHit atkHit enemyCfg
            hurtSecs    = flip execState hitstunSecs $ do
                -- don't override longer hitstun w/ shorter one
                get >>= \secs -> case behavior of
                    HurtBehavior hurtTtl _
                        | hurtTtl > secs -> put hurtTtl
                    _                    -> return ()

                -- enforce min time in fallen state
                when (hurtType `elem` [FallenHurt, KnockDownHurt]) $
                    modify $ max (_minFallenSecs enemyCfg)

            behavior'                      = HurtBehavior hurtSecs hurtType
            enemyData'                     = enemyData {_behavior = behavior'}
            dir                            = maybe (E._dir enemy) flipDirection (_dir (atkHit :: AttackHit))
            touchingGround
                | hurtType == LaunchUpHurt = False
                | otherwise                = onGround
        in do
            when (atkDmg > 0) $ do
                unless justGotHit $
                    writeMsgs
                        [ mkMsg $ ParticleMsgAddM (mkEnemyHurtParticle enemy atkHit hurtEffectData)
                        , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticle atkHit)
                        ]
                unlessM ((justGotHit ||) <$> readSettingsConfig _debug _disableEnemyHurtSfx) $
                    writeMsgs [mkMsg $ AudioMsgPlaySound hurtSoundPath atkPos]

            return $ enemy
                { _data          = enemyData'
                , _vel           = atkVel'
                , _dir           = dir
                , _attack        = Nothing
                , _health        = hp
                , _launchTargetY = attackHitLaunchTargetY (E._pos enemy) atkHit
                , _flags         = flags
                    { _touchingGround = touchingGround
                    , _justGotHit     = Just atkPos
                    }
                }

    | otherwise = do
        when (atkDmg > 0 && not justGotHit) $
            writeMsgs
                [ mkMsg $ ParticleMsgAddM (mkEnemyHurtParticleEx enemy atkHit hurtEffectData WeakHitEffect)
                , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticleEx atkHit WeakHitEffect)
                ]

        return $ enemy
            { _health = hp
            , _flags  = flags {_justGotHit = Just atkPos}
            }

    where
        enemyData      = _data enemy
        cfg            = _hop (_config enemyData :: EnemyConfig)
        hurtEffectData = _hurtEffectData cfg
        behavior       = _behavior enemyData
        onGround       = enemyTouchingGround enemy
        inAir          = not onGround

        isWeakAtkHitVel = _isWeakVel atkHit
        velY            = vecY $ E._vel enemy
        stagger         = _stagger (atkHit :: AttackHit)
        isStagger       = stagger >= _staggerThreshold cfg || case behavior of
            HurtBehavior _ _
                | isWeakAtkHitVel -> velY >= 0 && inAir
                | otherwise       -> True
            LaunchedBehavior _
                | isWeakAtkHitVel -> velY >= 0
                | otherwise       -> True
            FallenBehavior _      -> True
            WallSplatBehavior _   -> True
            _                     -> False

        atkPos                  = _intersectPos atkHit
        justGotHit              = enemyJustGotHit enemy
        flags                   = _flags enemy
        atkAlwaysLaunches       = _alwaysLaunches atkHit
        atkVel@(Vel2 _ atkVelY) = _vel (atkHit :: AttackHit)
        atkDmg                  = _damage (atkHit :: AttackHit)
        hp                      = decreaseEnemyHealth atkDmg enemy
        isAirVulnerable         = inAir && not isWeakAtkHitVel

updateGroundResponse :: MsgsWrite UpdateEnemyMsgsPhase m => EnemyUpdateGroundResponse HopEnemyData m
updateGroundResponse groundY enemy
    | velY >= 0.0 =
        let
            x              = vecX $ E._pos enemy
            enemyData      = _data enemy
            minFallenSecs  = _minFallenSecs $ _config enemyData
            fallenBehavior = FallenBehavior minFallenSecs

            behavior         = _behavior enemyData
            isPrevLaunched   = isLaunchedBehavior behavior
            allLandBehaviors = [AttackHopShortLandBehavior, HopLongLandBehavior, HopShortLandBehavior]
            vel              = if
                | behavior `elem` allLandBehaviors -> Vel2 0.0 onGroundVelY
                | otherwise                        -> Vel2 velX onGroundVelY

            behavior' = case behavior of
                HurtBehavior _ LaunchUpHurt -> fallenBehavior
                HurtBehavior _ AirHurt      -> fallenBehavior
                LaunchedBehavior _          -> fallenBehavior
                _                           -> behavior
        in do
            when isPrevLaunched $
                let effectDrawScale = _groundImpactEffectDrawScale $ _hop (_config enemyData :: EnemyConfig)
                in writeMsgs $ enemyGroundImpactMessages effectDrawScale enemy

            return $ enemy
                { _data  = enemyData {_behavior = behavior'}
                , _pos   = Pos2 x groundY
                , _vel   = vel
                , _flags = flags
                }

    | otherwise = return $ enemy {_flags = flags}

    where
        Vel2 velX velY = E._vel enemy
        flags          = (_flags enemy) {_touchingGround = True}

updateHangtimeResponse :: EnemyUpdateHangtimeResponse HopEnemyData
updateHangtimeResponse hangtimeSecs enemy
    | behavior == DeathBehavior = enemy
    | inAir                     =
        let enemyData' = enemyData {_behavior = LaunchedBehavior hangtimeSecs}
        in enemy {_data = enemyData'}
    | otherwise                = enemy
    where
        enemyData = _data enemy
        behavior  = _behavior enemyData
        inAir     = not $ enemyTouchingGround enemy
