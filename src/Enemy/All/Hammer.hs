module Enemy.All.Hammer
    ( allHammerEnemyPreloadPackFilePaths
    , mkHammerEnemy
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, get, modify, put)
import qualified Data.Text as T

import Attack.Hit
import Attack.Util
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Hammer
import Configs.All.EnemyLockOn
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy as E
import Enemy.All.Hammer.AI
import Enemy.All.Hammer.Behavior
import Enemy.All.Hammer.Data
import Enemy.All.Hammer.Sprites
import FileCache
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Util
import Window.Graphics

allHammerEnemyPreloadPackFilePaths = ["data/enemies/hammer-enemy.pack"] :: [FilePath]

hurtSoundPath       = "event:/SFX Events/Enemy/Hammer/hurt" :: FilePath
onlyMeteorDebugFlag = T.toLower "onlyMeteor"                :: T.Text
onlySwingDebugFlag  = T.toLower "onlySwing"                 :: T.Text

intangibleBehaviors =
    [ SpawnBehavior
    , DeathBehavior
    , ReFormBehavior
    , AttackLandBehavior
    ] :: [HammerEnemyBehavior]

mkHammerEnemy :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkHammerEnemy pos dir = do
    enData            <- mkHammerEnemyData  pos
    enemy             <- mkEnemy enData pos dir
    hammerCfg         <- readEnemyConfig _hammer
    lockOnReticleData <- readEnemyLockOnConfig _hammer

    return . Some $ enemy
        { _type                   = Just HammerEnemy
        , _health                 = _health (hammerCfg :: HammerEnemyConfig)
        , _hitbox                 = hitbox
        , _lockOnReticleData      = lockOnReticleData
        , _thinkAI                = thinkAI
        , _updateHurtResponse     = updateHurtResponse
        , _updateGroundResponse   = updateGroundResponse
        , _updateHangtimeResponse = updateHangtimeResponse
        , _updateSprite           = updateHammerSpr
        , _setDebugBehavior       = setDebugBehavior
        }

hitbox :: EnemyHitbox HammerEnemyData
hitbox enemy
    | _behavior enemyData `elem` intangibleBehaviors = dummyHitbox $ Pos2 x (y - height / 2.0)
    | otherwise                                      = hbx
    where
        enemyData = _data enemy
        Pos2 x y  = _pos enemy
        cfg       = _hammer (_config enemyData :: EnemyConfig)
        width     = _width (cfg :: HammerEnemyConfig)
        height    = _height (cfg :: HammerEnemyConfig)
        pos       = Pos2 (x - width / 2.0) (y - height)
        hbx       = rectHitbox pos width height

updateHammerSpr :: EnemyUpdateSprite HammerEnemyData
updateHammerSpr enemy = case _behavior enemyData of
    IdleBehavior _                     -> setOrUpdateEnemySpr $ _idle sprs
    PatrolBehavior                     -> setOrUpdateEnemySpr $ _forwardsFly sprs
    TeleportBehavior                   -> setOrUpdateEnemySpr $ _teleport sprs
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
            AirHurt       -> _hurt sprs
        | otherwise                    -> updateEnemySprite enemy
    WallSplatBehavior _                -> setOrUpdateEnemySpr $ _wallSplat sprs
    DeathBehavior                      -> (setOrUpdateEnemySpr (_death sprs)) {_draw = Just drawEnemyDeath}
    AttackBehavior                     -> clearEnemySprite enemy
    AttackLandBehavior                 -> clearEnemySprite enemy
    SpawnBehavior                      -> setOrUpdateEnemySpr $ _spawn sprs
    FallenBehavior _                   -> setOrUpdateEnemySpr $ _fallen sprs
    SitUpBehavior                      -> setOrUpdateEnemySpr $ _sitUp sprs
    ReFormBehavior                     -> setOrUpdateEnemySpr $ _reForm sprs
    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr

        justGotHit    = enemyJustGotHit enemy
        velY          = vecY $ E._vel enemy
        enemyData     = _data enemy
        sprs          = _sprites enemyData
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)


updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse HammerEnemyData m
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

            isFallen     = isFallenBehavior behavior
            isFallenHurt = isFallenHurtBehavior behavior

            hurtType
                | atkVelY < 0.0 || atkAlwaysLaunches              = LaunchUpHurt
                | atkVelY > 0.0 && not (isFallen || isFallenHurt) = KnockDownHurt
                | otherwise                                       = case behavior of
                    HurtBehavior _ LaunchUpHurt  -> LaunchUpHurt
                    LaunchedBehavior _ _         -> LaunchedHurt
                    HurtBehavior _ LaunchedHurt  -> LaunchedHurt
                    HurtBehavior _ KnockDownHurt -> LaunchedHurt
                    FallenBehavior _             -> FallenHurt
                    SitUpBehavior                -> FallenHurt
                    HurtBehavior _ FallenHurt    -> FallenHurt
                    WallSplatBehavior _          -> WallHurt
                    HurtBehavior _ WallHurt      -> WallHurt
                    _                            -> AirHurt

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
                unlessM ((justGotHit ||) <$> readSettingsConfig _debug _disableEnemyHurtSfx) $
                    writeMsgs [mkMsg $ AudioMsgPlaySound hurtSoundPath atkPos]

            return $ enemy
                { _data          = enemyData'
                , _vel           = atkVel'
                , _dir           = maybe dir flipDirection (_dir (atkHit :: AttackHit))
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
        cfg            = _hammer (_config enemyData :: EnemyConfig)
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
        justGotHit        = enemyJustGotHit enemy
        flags             = _flags enemy
        atkDmg            = _damage atkHit
        hp                = decreaseEnemyHealth atkDmg enemy

updateGroundResponse :: MsgsWrite UpdateEnemyMsgsPhase m => EnemyUpdateGroundResponse HammerEnemyData m
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
                let effectDrawScale = _groundImpactEffectDrawScale $ _hammer (_config enemyData :: EnemyConfig)
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

updateHangtimeResponse :: EnemyUpdateHangtimeResponse HammerEnemyData
updateHangtimeResponse hangtimeSecs enemy
    | behavior == DeathBehavior = enemy
    | inAir                     =
        let
            behavior'  = LaunchedBehavior hangtimeSecs InHangtime
            enemyData' = enemyData {_behavior = behavior'}
        in enemy {_data = enemyData'}
    | otherwise                 = enemy
    where
        enemyData = _data enemy
        behavior  = _behavior enemyData
        inAir     = not $ enemyTouchingGround enemy

setDebugBehavior :: EnemySetDebugBehavior HammerEnemyData
setDebugBehavior flags enemy = (debugTxt, enemy {_data = enemyData'})
    where
        enemyData = _data enemy
        cfg       = _config enemyData
        hammerCfg = _hammer (cfg :: EnemyConfig)

        (hammerCfg', debugTxt)
            | onlyMeteorDebugFlag `elem` flags =
                ( hammerCfg
                    { _attackMeteorRangeX = abs $ _attackMeteorRangeX hammerCfg
                    , _attackSwingRangeX  = -1.0 * abs (_attackSwingRangeX hammerCfg)
                    }
                , " | only meteor attack"
                )

            | onlySwingDebugFlag `elem` flags =
                ( hammerCfg
                    { _attackMeteorRangeX = -1.0 * abs (_attackMeteorRangeX hammerCfg)
                    , _attackSwingRangeX  = abs $ _attackSwingRangeX hammerCfg
                    }
                , " | only swing attack"
                )

            | otherwise = (hammerCfg, "")

        enemyData' = enemyData
            { _config = cfg {_hammer = hammerCfg'}
            }
