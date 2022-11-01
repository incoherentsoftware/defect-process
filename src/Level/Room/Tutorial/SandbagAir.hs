module Level.Room.Tutorial.SandbagAir
    ( mkSandbagAir
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, get, modify, put)
import Data.Functor           ((<&>))
import qualified Data.Map as M

import Attack.Hit
import Attack.Util
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Flying
import Configs.All.EnemyLockOn
import Enemy as E
import Level.Room.Tutorial.SandbagAir.AI
import Level.Room.Tutorial.SandbagAir.Behavior
import Level.Room.Tutorial.SandbagAir.Data
import Level.Room.Tutorial.SandbagAir.Sprites
import FileCache
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Util
import Window.Graphics

spritePrefix = "dummy-air-" :: String

additionalLockOnReticleDataOffsetMap = M.fromList
    [ ("dummy-air-dematerialize", repeat (Pos2 0.0 30.0))
    , ("dummy-air-rematerialize", repeat (Pos2 0.0 (-32.0)))
    ] :: M.Map String [Pos2]

readLockOnReticleData :: ConfigsRead m => m EnemyLockOnReticleData
readLockOnReticleData = readEnemyLockOnConfig _flying <&> \lockOnReticleData -> lockOnReticleData
    { _offsetMap =
        M.union additionalLockOnReticleDataOffsetMap .
        M.mapKeys (spritePrefix ++) <$>
        _offsetMap lockOnReticleData
    }

mkSandbagAir :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkSandbagAir pos dir = do
    enData            <- mkSandbagAirData pos dir
    enemy             <- mkEnemy enData pos dir
    flyingCfg         <- readEnemyConfig _flying
    lockOnReticleData <- readLockOnReticleData

    return . Some $ enemy
        { _type                   = Just FlyingEnemy  -- pretend to be flying enemy
        , _health                 = _health (flyingCfg :: FlyingEnemyConfig)
        , _hitbox                 = hitbox
        , _lockOnReticleData      = lockOnReticleData
        , _thinkAI                = thinkAI
        , _updateHurtResponse     = updateHurtResponse
        , _updateGroundResponse   = updateGroundResponse
        , _updateHangtimeResponse = updateHangtimeResponse
        , _updateSprite           = updateSpr
        }

hitbox :: EnemyHitbox SandbagAirData
hitbox enemy = case _behavior enemyData of
    SpawnBehavior         -> dummyHbx
    DeathBehavior         -> dummyHbx
    DematerializeBehavior -> dummyHbx
    RematerializeBehavior -> dummyHbx
    _                     -> rectHitbox pos width height
    where
        Pos2 x y  = _pos enemy
        enemyData = _data enemy
        cfg       = _flying (_config enemyData :: EnemyConfig)
        width     = _width (cfg :: FlyingEnemyConfig)
        height    = _height (cfg :: FlyingEnemyConfig)
        pos       = Pos2 (x - width / 2.0) (y - height)
        dummyHbx  = dummyHitbox $ Pos2 x (y - height / 2.0)

updateSpr :: EnemyUpdateSprite SandbagAirData
updateSpr enemy = case _behavior enemyData of
    IdleBehavior                       -> setOrUpdateEnemySpr $ _idle sprs
    LaunchedBehavior _ _
        | velY <= 0.0 || inHangtimeVel -> setOrUpdateEnemySpr $ _launched sprs
        | otherwise                    -> setOrUpdateEnemySpr $ _fall sprs
    HurtBehavior _ hurtType
        | justGotHit                   -> setEnemyHurtSprite enemy $ case hurtType of
            WallHurt      -> _wallHurt sprs
            FallenHurt    -> _fallenHurt sprs
            KnockDownHurt -> _knockDownFall sprs
            LaunchUpHurt  -> _launchUp sprs
            LaunchedHurt  -> _launchedHurt sprs
            FlyingHurt    -> _hurt sprs
        | otherwise                    -> updateEnemySprite enemy
    WallSplatBehavior _                -> setOrUpdateEnemySpr $ _wallSplat sprs
    FallenBehavior _                   -> setOrUpdateEnemySpr $ _fallen sprs
    DematerializeBehavior              -> setOrUpdateEnemySpr $ _dematerialize sprs
    RematerializeBehavior              -> setOrUpdateEnemySpr $ _rematerialize sprs
    SpawnBehavior                      -> setOrUpdateEnemySpr $ _spawn sprs
    DeathBehavior                      -> (setOrUpdateEnemySpr (_death sprs)) {_draw = Just drawEnemyDeath}
    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr

        justGotHit    = enemyJustGotHit enemy
        velY          = vecY $ E._vel enemy
        enemyData     = _data enemy
        sprs          = _sprites enemyData
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)

updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse SandbagAirData m
updateHurtResponse atkHit enemy
    | behavior == DeathBehavior = return enemy

    | isStagger || atkAlwaysLaunches =
        let
            atkVel@(Vel2 _ atkVelY)         = _vel (atkHit :: AttackHit)
            onGround                        = enemyTouchingGround enemy
            atkVel'
                -- prevent sliding on the ground from downwards aerial attacks
                | onGround && atkVelY > 0.0 = Vel2 0.0 atkVelY
                | otherwise                 = atkVel
            isFallen                        = isFallenBehavior behavior
            isFallenHurt                    = isFallenHurtBehavior behavior

            hurtType
                | atkVelY < 0.0 || atkAlwaysLaunches              = LaunchUpHurt
                | atkVelY > 0.0 && not (isFallen || isFallenHurt) = KnockDownHurt
                | otherwise                                       = case behavior of
                    HurtBehavior _ LaunchUpHurt  -> LaunchUpHurt
                    LaunchedBehavior _ _         -> LaunchedHurt
                    HurtBehavior _ LaunchedHurt  -> LaunchedHurt
                    HurtBehavior _ KnockDownHurt -> LaunchedHurt
                    FallenBehavior _             -> FallenHurt
                    HurtBehavior _ FallenHurt    -> FallenHurt
                    WallSplatBehavior _          -> WallHurt
                    HurtBehavior _ WallHurt      -> WallHurt
                    _                            -> FlyingHurt

            enemyCfg    = _config enemyData
            hitstunSecs = enemyHitstunFromAttackHit atkHit enemyCfg
            hurtSecs    = flip execState hitstunSecs $ do
                -- don't override longer hitstun w/ shorter one
                get >>= \secs -> case behavior of
                    HurtBehavior hurtTtl _
                        | hurtTtl > secs -> put hurtTtl
                    _                    -> return ()

                -- enforce min time in wall hit state (same as wall splat)
                when (hurtType == WallHurt) $
                    modify $ max (_minWallSplatSecs enemyCfg)

                -- enforce min time in fallen state
                when (hurtType `elem` [FallenHurt, KnockDownHurt]) $
                    modify $ max (_minFallenSecs enemyCfg)

            behavior'                      = HurtBehavior hurtSecs hurtType
            enemyData'                     = enemyData {_behavior = behavior'}
            dir                            = E._dir enemy
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

            return $ enemy
                { _data          = enemyData'
                , _vel           = atkVel'
                , _dir           = maybe dir flipDirection (_dir (atkHit :: AttackHit))
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
        cfg            = _flying (_config enemyData :: EnemyConfig)
        hurtEffectData = _hurtEffectData cfg
        behavior       = _behavior enemyData

        isWeakAtkHitVel = _isWeakVel atkHit
        velY            = vecY $ E._vel enemy
        stagger         = _stagger (atkHit :: AttackHit)
        isStagger       = stagger >= _staggerThreshold cfg || case behavior of
            HurtBehavior _ _
                | isWeakAtkHitVel -> velY >= 0
                | otherwise       -> True
            LaunchedBehavior _ _
                | isWeakAtkHitVel -> velY >= 0
                | otherwise       -> True
            FallenBehavior _      -> True
            WallSplatBehavior _   -> True
            _                     -> False

        atkAlwaysLaunches = _alwaysLaunches atkHit
        atkPos            = _intersectPos atkHit
        atkDmg            = _damage atkHit
        justGotHit        = enemyJustGotHit enemy
        flags             = _flags enemy
        hp                = decreaseEnemyHealth atkDmg enemy

updateGroundResponse :: MsgsWrite UpdateEnemyMsgsPhase m => EnemyUpdateGroundResponse SandbagAirData m
updateGroundResponse groundY enemy
    | velY >= 0.0 =
        let
            x              = vecX $ E._pos enemy
            enemyData      = _data enemy
            minFallenSecs  = _minFallenSecs $ _config enemyData
            fallenBehavior = FallenBehavior minFallenSecs
            behavior       = _behavior enemyData

            isPrevLaunched = case behavior of
                HurtBehavior _ LaunchUpHurt  -> True
                HurtBehavior _ LaunchedHurt  -> True
                HurtBehavior _ KnockDownHurt -> not $ enemyTouchingGround enemy
                LaunchedBehavior _ _         -> True
                _                            -> False

            behavior' = case behavior of
                HurtBehavior _ LaunchUpHurt  -> fallenBehavior
                HurtBehavior _ KnockDownHurt -> fallenBehavior
                LaunchedBehavior _ _         -> fallenBehavior
                _                            -> behavior
        in do
            when isPrevLaunched $
                let effectDrawScale = _groundImpactEffectDrawScale $ _flying (_config enemyData :: EnemyConfig)
                in writeMsgs $ enemyGroundImpactMessages effectDrawScale enemy

            return $ enemy
                { _data  = enemyData {_behavior = behavior'}
                , _pos   = Pos2 x groundY
                , _vel   = Vel2 velX 0.1
                , _flags = flags
                }

    | otherwise = return $ enemy {_flags = flags}

    where
        Vel2 velX velY = E._vel enemy
        flags          = (_flags enemy) {_touchingGround = True}

updateHangtimeResponse :: EnemyUpdateHangtimeResponse SandbagAirData
updateHangtimeResponse hangtimeSecs enemy
    | behavior == DeathBehavior = enemy
    | inAir                     = enemy
        { _data = enemyData {_behavior = LaunchedBehavior hangtimeSecs InHangtime}
        }
    | otherwise                 = enemy
    where
        enemyData = _data enemy
        behavior  = _behavior enemyData
        inAir     = not $ enemyTouchingGround enemy
